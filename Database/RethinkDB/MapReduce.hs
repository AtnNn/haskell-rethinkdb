{-# LANGUAGE OverloadedStrings, PatternGuards, ImplicitParams #-}

module Database.RethinkDB.MapReduce (termToMapReduce) where

import Control.Monad.State
import Control.Monad.Writer
import Data.Text (Text)
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
  
  -- A variable is introduced to represent the sequence that f
  -- is being performed on. This variable is no longer present
  -- in the return value
  v <- newVarId
  body <- baseReQL $ f (op VAR [v])
  
  return $ mrBuildFun $ toMapReduce v body

-- | Compares the two representations of a variable
sameVar :: Int -> BaseArray -> Bool
sameVar x [BaseReQL DATUM (Just (Datum.Datum{ Datum.r_num = Just y })) _ _] =
  fromIntegral x == y
sameVar _ _ = False

-- | notNone checks that it is a map/reduce and not a constant
notNone :: MRBuild -> Bool
notNone (MRBuild _ (None{})) = False
notNone _ = True

-- | Helper function for casting up from BaseReQL into ReQL
wrap :: BaseReQL -> ReQL
wrap = ReQL . return

-- | Build a single argument function from a constant ReQL expression
toFun1 :: ReQL -> (ReQL -> ReQL)
toFun1 f a = op FUNCALL (f, a)

-- | Build a two argument function from a constant ReQL expression
toFun2 :: ReQL -> (ReQL -> ReQL -> ReQL)
toFun2 f a b = op FUNCALL (f, a, b)

-- | Represents a map-reduce operation split into its map and reduce parts
data MapReduce =
  -- | A constant
  None ReQL |
  -- | A map
  Map (ReQL -> ReQL) |
  -- | A map, a reduce and maybe a finalizer
  MapReduce (ReQL -> ReQL) (ReQL -> ReQL -> ReQL) (Maybe (ReQL -> ReQL))

-- | The representation used while building a MapReduce.
data MRBuild = MRBuild {
  -- | The actual function being built, used by termToMapReduce
  mrBuildFun :: ReQL -> ReQL,
  -- | The map/reduce representation of that function, used by toMapReduce
  mrBuildMR :: MapReduce }

mrBuild :: MapReduce -> MRBuild
mrBuild (None t) = MRBuild (\x -> op FUNCALL (t, x)) (None t)
mrBuild (Map f) = mrMap f
mrBuild (MapReduce m r f) = mrMapReduce m r f

mrMapReduce :: (ReQL -> ReQL) -> (ReQL -> ReQL -> ReQL) -> (Maybe (ReQL -> ReQL))
            -> MRBuild
mrMapReduce m r f = MRBuild (\s -> fromMaybe id f (rs (ms s))) (MapReduce m r f)
    where
      rs s = op REDUCE (s, r)
      ms s = op MAP (s, m)

mrMap :: (ReQL -> ReQL) -> MRBuild
mrMap f = MRBuild (\s -> op MAP (s, f)) (Map f)

mrConst :: BaseReQL -> MRBuild
mrConst t = MRBuild (\x -> op FUNCALL (r, x)) (None r)
  where r = wrap t

-- | Rewrites the term in the second argument to merge all uses of the
-- variable whose id is given in the first argument.
toMapReduce :: Int -> BaseReQL -> MRBuild

-- A datum stays constant
toMapReduce _ t@(BaseReQL DATUM _ _ _) = mrConst t

-- The presence of the variable 
toMapReduce v (BaseReQL VAR _ w _) | sameVar v w = MRBuild id (Map id)
toMapReduce v t@(BaseReQL type' _ args optargs) = let
  
  -- Convert all arguments into MRBuilds
  args' = map (toMapReduce v) args
  optargs' = map (\(BaseAttribute k vv) -> (k, toMapReduce v vv)) optargs
  
  -- Count how many of the arguments have been rewritten
  count = length $ filter notNone $ args' ++ map snd optargs'
  
  -- Rewrite the current term. rewrite1 is optimised for
  -- the single count case
  rewrite = (if count == 1 then rewrite1 else rewritex) type' args' optargs'
  
  in case () of
    -- Don't rewrite if there is nothing to rewrite
    _ | 0 <- count -> mrConst t
    
    -- Don't rewrite an operation that can be chained
    _ | (arg1 : _) <- args', notNone arg1 ->
      maybe rewrite (MRBuild (mrChainFun type' arg1 (tail args) optargs')) $
      mrChain type' (map mrBuildMR args') optargs
         
    -- Default to rewriting the term
    _ -> rewrite

-- only one of the MRBuilds should not be None
mrChainFun :: TermType -> MRBuild -> [BaseReQL] -> [(Text, MRBuild)]
           -> (ReQL -> ReQL)
mrChainFun t a as oa s = op' t (goArg a : map wrap as) (map goOptArg oa)
  where
    goArg (MRBuild _ (None t)) = t
    goArg (MRBuild f _) = f s
    goOptArg (k, MRBuild _ (None t)) = k :== t
    goOptArg (k, MRBuild f _) = k :== f s

-- | Applies a reql function to MRBuild objects. This function assumes
-- that only one of the MRBuild objects is not None.
mrChain :: TermType -> [MapReduce] -> [BaseAttribute] -> Maybe MapReduce
mrChain = undefined
--mrChain MAP [MRBuild b (Map m), None f] [] = Just $ 
--mrChain REDUCE [Map m, None f] [] = Just $ MapReduce m (toFun2 f) Nothing

{-
                (COUNT, [Map _], []) ->
                  MapReduce (const (num 1)) (\a b -> op ADD (a, b)) Nothing
                (tt, (Map m : _), _) | tt `elem` mappableTypes ->
                  (Map ((\x -> op' tt (expr x : map expr (tail args)) (noRecurse : map baseAttrToOptArg optargs)) . m))
                _ -> rewrite
-}

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

rewrite1 :: TermType -> [MRBuild] -> [(T.Text, MRBuild)] -> MRBuild
rewrite1 ttype args optargs = mrMapReduce maps reduce finals where
  (finally2, [mr]) = ?extract Nothing ttype args optargs
  (maps, reduce, finally1) = ?toReduce mr
  finals = Just $ maybe finally2 (finally2 .) finally1

rewritex :: TermType -> [MRBuild] -> [(Key, MRBuild)] -> MRBuild
rewritex ttype args optargs = mrMapReduce maps reduces finallys where
  (finally, mrs) = ?extract (Just 0) ttype args optargs
  index = zip ([0..] :: [Int])
  triplets = map ?toReduce mrs
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