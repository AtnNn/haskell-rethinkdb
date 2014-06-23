{-# LANGUAGE OverloadedStrings, PatternGuards, ImplicitParams #-}

module Database.RethinkDB.MapReduce (termToMapReduce) where

import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Text as T
import Data.Maybe
import Data.Foldable (toList)

import Database.RethinkDB.Protobuf.Ql2.Term.TermType
import qualified Database.RethinkDB.Protobuf.Ql2.Datum as Datum
import qualified Database.RethinkDB.Protobuf.Ql2.Datum.DatumType as Datum

import Database.RethinkDB.ReQL
import Database.RethinkDB.Objects

import qualified Database.RethinkDB.Functions as R
import Database.RethinkDB.NoClash hiding (get)

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
  
  return . applyChain $ toMapReduce v body

-- | Compares the two representations of a variable
sameVar :: Int -> BaseArray -> Bool
sameVar x [BaseReQL DATUM (Just (Datum.Datum{ Datum.r_num = Just y })) _ _] =
  fromIntegral x == y
sameVar _ _ = False

-- | notNone checks that it is a map/reduce and not a constant
notConst :: Chain -> Bool
notConst None{} = False
notConst SingletonArray{} = False
notConst _ = True

-- | Helper function for casting up from BaseReQL into ReQL
wrap :: BaseReQL -> ReQL
wrap = ReQL . return

-- | Build a single argument function from a constant ReQL expression
toFun1 :: BaseReQL -> (ReQL -> ReQL)
toFun1 f a = op FUNCALL (wrap f, a)

-- | Build a two argument function from a constant ReQL expression
toFun2 :: BaseReQL -> (ReQL -> ReQL -> ReQL)
toFun2 f a b = op FUNCALL (wrap f, a, b)

-- | Represents a map/reduce operation split into its map and reduce parts
data MRF = MRF {
      _mrfMapFun :: MapFun,
      _mrfReduceFun :: ReduceFun, 
      _mrfFinally :: ReQL -> ReQL }

-- | A Chain of ReQL expressions that can be transformed into
-- a map/reduce operation
data Chain =
  -- | A constant, not really a map/reduce operation
  None ReQL |
  -- | Just a map
  Map [Map] |
  -- | A map/reduce operation represented as parts
  MapReduceChain [Map] Reduce (ReQL -> ReQL) MRF |
  -- | A rewritten map/reduce
  MapReduce MRF |
  -- | Special cases for reduce with base
  AddBase ReQL Chain |
  SingletonArray ReQL

data TODO

-- | A built-in map operation
data Map =
  BuiltInMap TermType [ReQL] [OptArg] MapFun

data MapFun = MapFun {
  _mapConcat :: Bool, 
  _mapFun :: ReQL -> ReQL }

data Reduce =
  BuiltInReduce TermType [ReQL] [OptArg] ReduceFun
  
data ReduceFun = ReduceFun {
  _reduceMap :: ReQL -> ReQL,
  _reduceFun :: ReQL -> ReQL -> ReQL,
  _reduceFinally :: ReQL -> ReQL }

applyChain :: Chain -> (ReQL -> ReQL)
applyChain (None t) x = op FUNCALL (t, x)
applyChain (Map maps) s = applyMaps maps s
applyChain (MapReduceChain maps red fin _) s =
  fin $ applyReduce red $ applyMaps maps s

applyMaps :: [Map] -> ReQL -> ReQL
applyMaps maps s = foldr applyMap s maps

applyMap :: Map -> ReQL -> ReQL
applyMap (BuiltInMap tt a oa _) s = op' tt (s : a) oa

applyReduce :: Reduce -> ReQL -> ReQL
applyReduce (BuiltInReduce tt a oa _) s = op' tt (s : a) oa

-- | Rewrites the term in the second argument to merge all uses of the
-- variable whose id is given in the first argument.
toMapReduce :: Int -> BaseReQL -> Chain

-- Singletons are singled out
toMapReduce _ (BaseReQL DATUM (Just Datum.Datum{ Datum.type' = Just Datum.R_ARRAY,
                                                 Datum.r_array = arr }) _ _)
  | [datum] <- toList arr =
    SingletonArray . wrap $ BaseReQL DATUM (Just datum) [] []

-- A datum stays constant
toMapReduce _ t@(BaseReQL DATUM _ _ _) = None $ wrap t

-- The presence of the variable 
toMapReduce v (BaseReQL VAR _ w _) | sameVar v w = Map []
toMapReduce v t@(BaseReQL type' _ args optargs) = let
  
  -- Recursively convert all arguments
  args' = map (toMapReduce v) args
  optargs' = map (\(BaseAttribute k vv) -> (k, toMapReduce v vv)) optargs
  
  -- Count how many of the arguments have been rewritten
  count = length $ filter notConst $ args' ++ map snd optargs'
  
  -- Rewrite the current term. rewrite1 is optimised for
  -- the single count case
  rewrite = (if count == 1 then rewrite1 else rewritex) type' args' optargs'
  
  in case count of
    -- Special case for singleton arrays
    0 | Just sing <- singleton type' args' optargs -> SingletonArray sing
    
    -- Don't rewrite if there is nothing to rewrite
    0 -> None $ wrap t
    
    -- Don't rewrite an operation that can be chained
    _ | (arg1 : _) <- args', notConst arg1 -> do
      fromMaybe rewrite $ mrChain type' arg1 (tail args) optargs
         
    -- Default to rewriting the term
    _ -> rewrite

singleton :: TermType -> [Chain] -> [BaseAttribute] -> Maybe ReQL
singleton MAKE_ARRAY [None el] [] = Just el
singleton _ _ _ = Nothing

-- | Chain a ReQL command onto a MapReduce operation
mrChain :: TermType -> Chain -> [BaseReQL] -> [BaseAttribute]
        -> Maybe Chain

-- | The basics: map, reduce and reduce1
mrChain MAP (Map maps) [f] [] = Just . Map $ toMap f : maps
mrChain MAP (Map m) [f] [] = Just . Map False $ toFun1 f . m
mrChain REDUCE (Map m) [f] [] = Just $ MapReduce m (toFun2 f) Nothing
mrChain REDUCE (MapReduce (AddBase base (Map m)) _ _ _) [f] [] =
  Just $ MapReduce m (toFun2 f) (Just $ toFun2 f base)

-- | Special case for union with a singleton
mrChain UNION [SingletonArray sing, mr] [] = AddBase sing mr

-- | A built-in map
mrChain tt (Map maps) args optargs
    | Just mrf <- mapMRF tt args optargs =
        Just $ MapReduceChain tt (map wrap args)
               (map baseAttrToOptArg optargs) mrf

-- | A built-in reduction
mrChain tt (Map maps) args optargs
    | Just mrf <- reduceMRF tt args optargs =
        Just $ MapReduceChain tt (map wrap args)
               (map baseAttrToOptArg optargs) mrf

-- | These functions map over sequences
mrChain tt (Map m) args optargs
    | tt `elem` [PLUCK, WITHOUT, MERGE] =
        Just $ Map $ (. m) $ \x ->
            op' tt (expr x : map expr args)
            (noRecurse : map baseAttrToOptArg optargs)

mrChain _ _ _ _ = Nothing

-- | Convert some of the built-in operations into a map/reduce
--
-- TODO: test
reduceMRF :: TermType -> (ReQL -> ReQL) -> [BaseReQL] -> [BaseAttribute]
            -> Maybe MRF
reduceMRF COUNT _ [] [] = Just (const (num 1), \a b -> op ADD (a, b), Nothing)
reduceMRF AVG f [] [] =
    Just (\x -> expr [f x, 1],
          \a b -> expr [a R.!! 0 R.+ b R.!! 0, a R.!! 1 R.+ b R.!! 1],
          Just $ \x -> x R.!! 0 R./ x R.!! 1)
reduceMRF SUM f [] [] = Just (f, (R.+), Nothing)
reduceMRF SUM f [sel] [] = Just (sel . f, (R.+), Nothing)
reduceMRF MIN f [] [] = Just (f, \a b -> if' (a R.< b) a b, Nothing)
reduceMRF MIN f [sel] [] =
    Just (\x -> [f x, sel x],
          \a b -> if' (a R.!! 1 R.< b R.!! 1) a b,
          (R.!! 0))
reduceMRF MAX f [] [] = Just (f, \a b -> if' (a R.> b) a b, Nothing)
reduceMRF MAX f [sel] [] =
    Just (\x -> [f x, sel x],
          \a b -> if' (a R.!! 1 R.> b R.!! 1) a b,
          (R.!! 0))
reduceMRF DISTINCT _ _ _ = undefined
reduceMRF CONCATMAP _ _ _ = undefined
reduceMRF FILTER _ _ _ = undefined
reduceMRF GET_FIELD _ _ _ = undefined
reduceMRF HAS_FIELDS _ _ _ = undefined
reduceMRF WITH_FIELDS _ _ _ = undefined
reduceMRF _ _ _ _ = Nothing

-- | Convert from one representation to the other
baseAttrToOptArg :: BaseAttribute -> OptArg
baseAttrToOptArg (BaseAttribute k v) = k :== v

-- | This undocumented optional argument circumvents stream
-- polymorphism on some operations
noRecurse :: OptArg
noRecurse = "_NO_RECURSE_" :== True

-- | Rewrite a command into a map/reduce.
--
-- This is a special case for when only one of the arguments
-- is itself a map/reduce
rewrite1 :: TermType -> [Chain] -> [(T.Text, Chain)] -> Chain
rewrite1 ttype args optargs = MapReduce maps reduce finals where
  (finally2, [mr]) = extract Nothing ttype args optargs
  (maps, reduce, finally1) = mr
  finals = Just $ maybe finally2 (finally2 .) finally1

-- | Rewrite a command that combines the result of multiple map/reduce
-- operations into a single map/reduce operation
rewritex :: TermType -> [Chain] -> [(Key, Chain)] -> Chain
rewritex ttype args optargs = MapReduce maps reduces finallys where
  (finally, mrs) = extract (Just 0) ttype args optargs
  index = zip ([0..] :: [Int])
  maps x = expr $ map (($ x) . fst3) mrs
  reduces a b = expr $ map (uncurry $ mkReduce a b) . index $ map snd3 mrs
  finallys = let fs = map thrd3 mrs in
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

-- | Extract the inner map/reduce objects, also returning a function
-- which, given the result of all the map/reduce operations, returns
-- the result of the given command
extract ::
  Maybe Int -> TermType -> [Chain] -> [(Key, Chain)]
  -> (ReQL -> ReQL, [MRF])
extract st tt args optargs = fst $ flip runState st $ runWriterT $ do
  args' <- sequence $ map extractOne args
  optargvs' <- sequence $ map extractOne (map snd optargs)
  let optargks = map fst optargs
  return $ \v -> op' tt (map ($ v) args') (zipWith (:==) optargks $ map ($ v) optargvs')
    where
      extractOne (None _ term) = return $ const term
      extractOne (Map f) = go (\x -> op MAKE_ARRAY [f x], \a b -> op UNION (a,b), Nothing)
      extractOne (MapReduce _ m r f) = go (m, r, f)
      go mrf = do
        tell [mrf]
        st' <- get
        case st' of
          Nothing -> return id
          Just n -> do
            put $ Just $ n + 1
            return $ \v -> op NTH (v, n)