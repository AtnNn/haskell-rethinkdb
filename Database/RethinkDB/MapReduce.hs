{-# LANGUAGE OverloadedStrings, PatternGuards #-}

module Database.RethinkDB.MapReduce (termToMapReduce) where

import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Text as T
import Data.Maybe

import Database.RethinkDB.Protobuf.Ql2.Term.TermType
import qualified Database.RethinkDB.Protobuf.Ql2.Datum as Datum

import Database.RethinkDB.ReQL
import Database.RethinkDB.Objects

-- | Takes a function that takes a sequence as an argument, and
-- returns a function that only uses that sequence once, by merging
-- the map and reduce operations. This is used by groupBy.
termToMapReduce :: (ReQL -> ReQL) -> State QuerySettings (ReQL -> ReQL)
termToMapReduce f = do
  v <- newVarId
  body <- baseReQL $ f (op VAR [v])
  return $ mrBuildFun $ toMapReduce v body

sameVar :: Int -> BaseArray -> Bool
sameVar x [BaseReQL DATUM (Just (Datum.Datum{ Datum.r_num = Just y })) _ _] =
  fromIntegral x == y
sameVar _ _ = False

notNone :: MRBuild -> Bool
notNone (MRBuild _ (None{})) = False
notNone _ = True

wrap :: BaseReQL -> ReQL
wrap = ReQL . return

toFun1 :: ReQL -> (ReQL -> ReQL)
toFun1 f a = op FUNCALL (f, a)

toFun2 :: ReQL -> (ReQL -> ReQL -> ReQL)
toFun2 f a b = op FUNCALL (f, a, b)

-- | Represents a map-reduce operation split into its map and reduce parts
data MapReduce =
  None ReQL |
  Map (ReQL -> ReQL) |
  MapReduce (ReQL -> ReQL) (ReQL -> ReQL -> ReQL) (Maybe (ReQL -> ReQL))

-- | The representation used while building a MapReduce.
data MRBuild = MRBuild {
  mrBuildFun :: ReQL -> ReQL,
  mrBuildMR :: MapReduce }

mrMap :: (ReQL -> ReQL) -> MRBuild
mrMap f = MRBuild (\s -> op MAP (s, f)) (Map f)

-- | Rewrites the term in the second argument to merge all uses of the
-- variable whose id is given in the first argument.
toMapReduce :: Int -> BaseReQL -> MRBuild
toMapReduce _ t@(BaseReQL DATUM _ _ _) = MRConst t
toMapReduce v (BaseReQL VAR _ w _) | sameVar v w = MRBuild id (Map id)
toMapReduce v t@(BaseReQL type' _ args optargs) = let
    args' = map (toMapReduce v) args
    optargs' = map (\(BaseAttribute k vv) -> (k, toMapReduce v vv)) optargs
    count = length $ filter notNone $ args' ++ map snd optargs'
    rebuild = (if count == 1 then rebuild0 else rebuildx) type' args' optargs'
  in if count == 0 then None $ wrap t
     else if not $ count == 1
          then rebuild else
              maybe rebuild (MRBuild (mkChainFun type' args' optargs')) $
              mrChain type' (map mrBuildMR args') optargs

-- only one of the MRBuilds should not be None
mrChainFun :: TermType -> [MRBuild] -> [(Text, MRBuild)] -> (ReQL -> ReQL)
mrChainFun t a oa s = ReQL $ return $ BaseReQL t (map goArg a) (map goOptArg oa)
  where
    goArg (MRBuild _ (None t)) = t
    goArg (MRBuild f _) = f s
    goOptArg (k, MRBuild _ (None t)) = k :== t
    goOptArg (k, MRBuild f _) = k :== f s

-- | Applies a reql function to MRBuild objects. This function assumes
-- that only one of the MRBuild objects is not None.
mrChain :: TermType -> [MRBuild] -> OptArgs -> MRBuild
mrChain MAP [MRBuild b (Map m), None f] [] = Just $ 
mrChain REDUCE [Map m, None f] [] = Just $ MapReduce m (toFun2 f) Nothing
                (COUNT, [Map _], []) ->
                  MapReduce (const (num 1)) (\a b -> op ADD (a, b)) Nothing
                (tt, (Map m : _), _) | tt `elem` mappableTypes ->
                  (Map ((\x -> op' tt (expr x : map expr (tail args)) (noRecurse : map baseAttrToOptArg optargs)) . m))
                _ -> rebuild

optargsToBase :: [BaseAttribute] -> Maybe (Maybe ReQL)
optargsToBase [] = Just Nothing
optargsToBase [BaseAttribute "base" b] = Just (Just $ ReQL $ return b)
optargsToBase _ = Nothing

baseAttrToOptArg :: BaseAttribute -> OptArg
baseAttrToOptArg (BaseAttribute k v) = k :== v

-- This circumvents stream polymorphism on some operations
noRecurse :: OptArg
noRecurse = "_NO_RECURSE_" :== True

mappableTypes :: [TermType]
mappableTypes = [GET_FIELD, PLUCK, WITHOUT, MERGE, HAS_FIELDS]

rebuild0 :: TermType -> [MapReduce] -> [(T.Text, MapReduce)] -> MapReduce
rebuild0 ttype args optargs = MapReduce maps reduce finals where
  (finally2, [mr]) = extract Nothing ttype args optargs
  (maps, reduce, finally1) = toReduce mr
  finals = Just $ maybe finally2 (finally2 .) finally1

rebuildx :: TermType -> [MapReduce] -> [(Key, MapReduce)] -> MapReduce
rebuildx ttype args optargs = MapReduce maps reduces finallys where
  (finally, mrs) = extract (Just 0) ttype args optargs
  index = zip ([0..] :: [Int])
  triplets = map toReduce mrs
  maps x = expr $ map (($ x) . fst3) triplets
  reduces a b = expr $ map (uncurry $ mkReduce a b) . index $ map snd3 triplets
  finallys = let fs = map thrd3 triplets in
    if all isNothing fs
       then Just finally
       else Just $ \x -> finally $ expr $ map (uncurry $ mkFinally x) . index $
                         map (fromMaybe id) fs
  mkReduce a b i f = f (op NTH (a, i)) (op NTH (b, i))
  mkFinally x i f = f (op NTH (x, i))

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
  return $ \v -> op' tt (map ($ v) args') (zipWith (:==) optargks $ map ($ v) optargvs')

extractOne :: MapReduce -> WriterT [MapReduce] (State (Maybe Int)) (ReQL -> ReQL)
extractOne (None term) = return $ const term
extractOne mr = do
  tell [mr]
  st <- get
  case st of
    Nothing -> return id
    Just n -> do
      put $ Just $ n + 1
      return $ \v -> op NTH (v, n)