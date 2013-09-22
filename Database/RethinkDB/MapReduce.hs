module Database.RethinkDB.MapReduce where

import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Text as T

import Database.RethinkDB.Protobuf.Ql2.Term.TermType
import qualified Database.RethinkDB.Protobuf.Ql2.Datum as Datum

import Database.RethinkDB.ReQL
import Database.RethinkDB.Objects

termToMapReduce ::
  (ReQL -> ReQL) -> State QuerySettings (ReQL -> ReQL, ReQL -> ReQL -> ReQL, ReQL -> ReQL)
termToMapReduce f = do
  v <- newVarId
  body <- baseReQL $ f (op VAR [v] ())
  let MapReduce map_ reduce finally = toMapReduce v body
  return (map_, reduce, finally)

toReduce :: MapReduce -> (ReQL -> ReQL, ReQL -> ReQL -> ReQL, ReQL -> ReQL)
toReduce (None t) = (\_ -> expr (), \_ _ -> expr (), const t)
toReduce (Map m) = ((\x -> expr [x]) . m, unionReduce, id)
toReduce (MapReduce m r f) = (m, r, f)

unionReduce :: ReQL -> ReQL -> ReQL
unionReduce a b = op UNION (a, b) ()

sameVar :: Int -> BaseArray -> Bool
sameVar x [BaseReQL DATUM (Just (Datum.Datum{ Datum.r_num = Just y })) _ _] =
  fromIntegral x == y
sameVar _ _ = False

notNone :: MapReduce -> Bool
notNone None{} = False
notNone _ = True

wrap :: BaseReQL -> ReQL
wrap = ReQL . return

toFun1 :: ReQL -> (ReQL -> ReQL)
toFun1 f a = op FUNCALL (f, a) ()

toFun2 :: ReQL -> (ReQL -> ReQL -> ReQL)
toFun2 f a b = op FUNCALL (f, a, b) ()

toMapReduce :: Int -> BaseReQL -> MapReduce
toMapReduce _ t@(BaseReQL DATUM _ _ _) = None $ wrap t
toMapReduce v (BaseReQL VAR _ w _) | sameVar v w = Map id
toMapReduce v t@(BaseReQL type' _ args optargs) = let
    args' = map (toMapReduce v) args
    optargs' = map (\(BaseAttribute k vv) -> (k, toMapReduce v vv)) optargs
    count = length $ filter notNone $ args' ++ map snd optargs'
    rebuild = (if count == 1 then rebuild0 else rebuildx) type' args' optargs'
  in if count == 0 then None $ wrap t
     else if not $ count == 1
          then rebuild else
              case (type', args', optargs') of
                (MAP, [Map m, None f], []) -> Map (toFun1 f . m)
                (REDUCE, [Map m, None f], _) | Just mbase <- optargsToBase optargs ->
                  MapReduce m (toFun2 f) (maybe id (toFun2 f) mbase)
                (COUNT, [Map _], []) ->
                  MapReduce (const 1) (\a b -> op ADD (a, b) ()) id
                (tt, (Map m : _), _) | tt `elem` mappableTypes ->
                  (Map ((\x -> op tt (expr x : map expr (tail args)) (noRecurse : map baseAttrToAttr optargs)) . m))
                _ -> rebuild

optargsToBase :: [BaseAttribute] -> Maybe (Maybe ReQL)
optargsToBase [] = Just Nothing
optargsToBase [BaseAttribute "base" b] = Just (Just $ ReQL $ return b)
optargsToBase _ = Nothing

baseAttrToAttr :: BaseAttribute -> Attribute
baseAttrToAttr (BaseAttribute k v) = k := v

noRecurse :: Attribute
noRecurse = "_NO_RECURSE_" := True

mappableTypes :: [TermType]
mappableTypes = [GET_FIELD, PLUCK, WITHOUT, MERGE, HAS_FIELDS]

data MapReduce =
    None ReQL |
    Map (ReQL -> ReQL) |
    MapReduce (ReQL -> ReQL) (ReQL -> ReQL -> ReQL) (ReQL -> ReQL)

rebuild0 :: TermType -> [MapReduce] -> [(T.Text, MapReduce)] -> MapReduce
rebuild0 ttype args optargs = MapReduce maps reduce finals where
  (finally2, [mr]) = extract Nothing ttype args optargs
  (maps, reduce, finally1) = toReduce mr
  finals = finally2 . finally1

rebuildx :: TermType -> [MapReduce] -> [(Key, MapReduce)] -> MapReduce
rebuildx ttype args optargs = MapReduce maps reduces finallys where
  (finally, mrs) = extract (Just 0) ttype args optargs
  index = zip ([0..] :: [Int])
  triplets = map toReduce mrs
  maps x = expr $ map (($ x) . fst3) triplets
  reduces a b = expr $ map (uncurry $ mkReduce a b) . index $ map snd3 triplets
  finallys x = finally $ expr $ map (uncurry $ mkFinally x) . index $ map thrd3 triplets
  mkReduce a b i f = f (op NTH (a, i) ()) (op NTH (b, i) ())
  mkFinally x i f = f (op NTH (x, i) ())

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

thrd3 :: (a,b,c) -> c
thrd3 (_,_,c) = c

extract ::
  Maybe Int -> TermType -> [MapReduce] -> [(Key, MapReduce)]
  -> (ReQL -> ReQL, [MapReduce])
extract st tt args optargs = fst $ flip runState st $ runWriterT $ do
  args' <- sequence $ map extractOne args
  optargvs' <- sequence $ map extractOne (map snd optargs)
  let optargks = map fst optargs
  return $ \v -> op tt (map ($ v) args') (zipWith (:=) optargks $ map ($ v) optargvs')

extractOne :: MapReduce -> WriterT [MapReduce] (State (Maybe Int)) (ReQL -> ReQL)
extractOne (None term) = return $ const term
extractOne mr = do
  tell [mr]
  st <- get
  case st of
    Nothing -> return id
    Just n -> do
      put $ Just $ n + 1
      return $ \v -> op NTH (v, n) ()