module MapReduce where

data MapReduce = MapReduce {
    termSequence :: Term T.Datum,
    termMap :: Maybe F1,
    termBase :: Maybe (Term T.Datum),
    termReduce :: Maybe F2,
    termFinalize :: Maybe F1 }

tvSequence :: Term t -> Maybe (State QuerySettings BaseTerm)
tvSequence (Term s Nothing Nothing Nothing Nothing) = Just s
tvSequence _ = Nothing

tvFinalize :: Term t -> Maybe (State QuerySettings BaseTerm, Maybe F1)
tvFinalize (Term s Nothing Nothing Nothing f) = Just (s, f)
tvFinalize _ = Nothing

tvMap :: Term t -> Maybe (State QuerySettings BaseTerm, Maybe F1, Maybe F1)
tvMap (Term s m Nothing Nothing f) = Just (s, m, f)
tvMap _ = Nothing

tvReduce :: Term t -> Maybe (State QuerySettings BaseTerm, Maybe (Term T.Datum),
                             Maybe F2, Maybe F1)
tvReduce (Term s Nothing b r f) = Just (s, b, r, f)
tvReduce _ = Nothing

tvMapReduce :: Term t -> Maybe (State QuerySettings BaseTerm, Maybe F1,
                                Maybe (Term T.Datum), Maybe F2, Maybe F1)
tvMapReduce (Term s m b r f) = Just (s, m, b, r, f)
tvMapReduce _ = Nothing

baseTerm :: Term t -> State QuerySettings BaseTerm
baseTerm (tvSequence -> Just s) = s
baseTerm Term {..} = let
  s = Term termSequence Nothing Nothing Nothing Nothing
  m = maybe s (\f -> bop MAP (s, f) []) termMap
  r = maybe m (\f -> bop REDUCE (m, f) (maybe [] (\x -> ["base":=x]) termBase)) termReduce
  f = maybe r ($ m)
  in baseTerm f
