{-# LANGUAGE OverloadedStrings, PatternGuards, ImplicitParams #-}

module Database.RethinkDB.MapReduce where -- (termToMapReduce) where

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
import Database.RethinkDB.NoClash hiding (get, collect)

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
      _mrfReduceFun :: ReQL -> ReQL -> ReQL,
      _mrfBase :: Maybe ReQL,
      _mrfFinally :: ReQL -> ReQL }

-- | A Chain of ReQL expressions that can be transformed into
-- a map/reduce operation
data Chain =
  -- | A constant, not really a map/reduce operation
  None ReQL |
  -- | Just a map
  Map [Map] |
  -- | map/reduce operations represented as parts
  MapReduceChain [Map] Reduce |
  -- | A rewritten map/reduce
  MapReduce MRF |
  -- | Special cases for reduce with base
  SingletonArray ReQL |
  AddBase ReQL Chain

-- | A built-in map operation
data Map =
  BuiltInMap TermType [ReQL] [OptArg] MapFun

data MapFun =
  MapFun (ReQL -> ReQL) |
  ConcatMapFun (ReQL -> ReQL)

data Reduce =
  BuiltInReduce TermType [ReQL] [OptArg] MRF
  
applyChain :: Chain -> (ReQL -> ReQL)
applyChain (None t) x = op FUNCALL (t, x)
applyChain (Map maps) s = applyMaps maps s
applyChain (MapReduceChain maps red) s =
  applyReduce red $ applyMaps maps s
applyChain (MapReduce mrf) s = applyMRF mrf s
applyChain (SingletonArray x) s = op FUNCALL (op MAKE_ARRAY [x], s)
applyChain (AddBase b c) s = applyChain c s R.++ [b]

applyMRF :: MRF -> ReQL -> ReQL
applyMRF (MRF m r Nothing f) s = f . reduce1 r $ applyMapFun m s
applyMRF (MRF m r (Just b) f) s =
  f $
  apply (\x -> if' (isEmpty x) b (x R.! 0)) . return $
  reduce1 (\a b -> [a R.! 0 `r` b R.! 0]) $
  R.map (\x -> [x]) $
  applyMapFun m s

applyMaps :: [Map] -> ReQL -> ReQL
applyMaps maps s = foldr applyMap s maps

applyMap :: Map -> ReQL -> ReQL
applyMap (BuiltInMap tt a oa _) s = op' tt (s : a) oa

applyMapFun :: MapFun -> ReQL -> ReQL 
applyMapFun (MapFun f) = R.map f
applyMapFun (ConcatMapFun f) = R.concatMap f


applyReduce :: Reduce -> ReQL -> ReQL
applyReduce (BuiltInReduce tt a oa _) s = op' tt (s : a) oa

chainToMRF :: Chain -> Either ReQL MRF
chainToMRF (None t) = Left t
chainToMRF (Map maps) = Right $ maps `thenMRF` collect
chainToMRF (MapReduceChain maps red) = Right $ maps `thenReduce` red
chainToMRF (MapReduceBase maps _base red) =
  Right $ maps `thenReduce` red -- The base should already be set in red's mrf
chainToMRF (MapReduce mrf) = Right $ mrf
chainToMRF (SingletonArray x) = Left $ op MAKE_ARRAY [x]
chainToMRF (AddBase b c) = fmap (`thenFinally` \x -> op UNION [b, x]) $ chainToMRF c

thenFinally :: MRF -> (ReQL -> ReQL) -> MRF
thenFinally (MRF m r b f1) f2 = MRF m r b $ f2 . f1

thenMRF :: [Map] -> MRF -> MRF
thenMRF maps (MRF m r b f) =
  MRF (m `composeMapFun` composeMaps maps) r b f

composeMaps :: [Map] -> MapFun
composeMaps = foldr composeMapFun (MapFun id) . map getMapFun
  where getMapFun (BuiltInMap _ _ _ mf) = mf

composeMapFun :: MapFun -> MapFun -> MapFun
composeMapFun (MapFun       f) (MapFun       g) = MapFun (f . g)
composeMapFun (ConcatMapFun f) (MapFun       g) = ConcatMapFun (f . g)
composeMapFun (MapFun       f) (ConcatMapFun g) = ConcatMapFun (R.map f . g)
composeMapFun (ConcatMapFun f) (ConcatMapFun g) = ConcatMapFun (R.concatMap f . g)

thenReduce :: [Map] -> Reduce -> MRF
thenReduce maps (BuiltInReduce _ _ _ mrf) = maps `thenMRF` mrf

collect :: MRF
collect = MRF (MapFun $ \x -> expr [x]) (R.++) (Just []) id

-- | Rewrites the term in the second argument to merge all uses of the
-- variable whose id is given in the first argument.
toMapReduce :: Int -> BaseReQL -> Chain

-- Singletons are singled out
toMapReduce _ (BaseReQL DATUM (Just Datum.Datum{ Datum.type' = Just Datum.R_ARRAY,
                                                 Datum.r_array = arr' }) _ _)
  | [datum] <- toList arr' =
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
  rewrite = MapReduce $
            (if count == 1 then rewrite1 else rewritex) type' args' optargs'
  
  in case count of
    -- Special case for singleton arrays
    0 | Just sing <- singleton type' args' optargs -> SingletonArray sing
    
    -- Special case for snoc
    1 | UNION <- type', [x, SingletonArray s] <- args', [] <- optargs'
      -> AddBase s x

    -- Don't rewrite if there is nothing to rewrite
    0 -> None $ wrap t
    
    -- Don't rewrite an operation that can be chained
    1 | (arg1 : _) <- args', notConst arg1 -> do
      fromMaybe rewrite $ mrChain type' arg1 (tail args) optargs
         
    -- Default to rewriting the term
    _ -> rewrite

singleton :: TermType -> [Chain] -> [BaseAttribute] -> Maybe ReQL
singleton MAKE_ARRAY [None el] [] = Just el
singleton _ _ _ = Nothing

-- | Chain a ReQL command onto a MapReduce operation
mrChain :: TermType -> Chain -> [BaseReQL] -> [BaseAttribute]
        -> Maybe Chain

mrChain REDUCE (AddBase base (Map maps)) [f] [] =
  Just $
  MapReduceChain maps $
  BuiltInReduce REDUCE [wrap f] [] $
  MRF (MapFun id) (toFun2 f) (Just base) id

-- | A built-in map
mrChain tt (Map maps) args optargs
    | Just mrf <- mapMRF tt args optargs =
       Just . Map . (: maps) $
            BuiltInMap tt (map wrap args) (map baseAttrToOptArg optargs) mrf

-- | A built-in reduction
mrChain tt (Map maps) args optargs
    | Just mrf <- reduceMRF tt args optargs =
        Just . MapReduceChain maps $
             BuiltInReduce tt (map wrap args) (map baseAttrToOptArg optargs) mrf

mrChain _ _ _ _ = Nothing

-- | Convert some builtin operations into a map
mapMRF :: TermType -> [BaseReQL] -> [BaseAttribute]
            -> Maybe MapFun
mapMRF MAP [f] [] = Just . MapFun $ toFun1 f
mapMRF PLUCK ks [] =
  Just . MapFun $ \s -> op' PLUCK (s : map wrap ks) [noRecurse]
mapMRF WITHOUT ks [] =
    Just . MapFun $ \s -> op' WITHOUT (s : map wrap ks) [noRecurse]
mapMRF MERGE [b] [] =
  Just . MapFun $ \s -> op' MERGE [s,  wrap b] [noRecurse]
mapMRF CONCATMAP [f] [] = Just . ConcatMapFun $ toFun1 f
mapMRF FILTER [f] [] =
  Just . ConcatMapFun $ \x -> if' (toFun1 f x # handle (const False)) x nil
mapMRF FILTER [f] [BaseAttribute "default" defval] =
    Just . ConcatMapFun $ \x -> if' (toFun1 f x # handle (const defval)) x nil
mapMRF GET_FIELD [attr] [] =
  Just . ConcatMapFun $ \x ->
  if' (op' HAS_FIELDS (x, wrap attr) [noRecurse])
  [op' GET_FIELD (x, attr) [noRecurse]] nil
mapMRF HAS_FIELDS sel [] =
  Just . ConcatMapFun $ \x ->
  if' (op' HAS_FIELDS (x : map wrap sel) [noRecurse]) [x] nil
mapMRF WITH_FIELDS sel [] =
  Just . ConcatMapFun $ \x ->
  if' (op' HAS_FIELDS (x : map wrap sel) [noRecurse])
  [op' PLUCK (x : map wrap sel) [noRecurse]] nil
mapMRF _ _ _ = Nothing

-- | Convert some of the built-in operations into a map/reduce
--
-- TODO: test
reduceMRF :: TermType -> [BaseReQL] -> [BaseAttribute]
            -> Maybe MRF
reduceMRF REDUCE [f] [] = Just $ MRF (MapFun id) (toFun2 f) id
reduceMRF COUNT [] [] = Just $ MRF (MapFun $ const (num 1)) (\a b -> op ADD (a, b)) id
reduceMRF AVG [] [] =
    Just $ MRF (MapFun $ \x -> expr [x, 1])
             (\a b -> expr [a R.!! 0 R.+ b R.!! 0, a R.!! 1 R.+ b R.!! 1])
             (\x -> x R.!! 0 R./ x R.!! 1)
reduceMRF SUM [] [] = Just $ MRF (MapFun id) (R.+) id
reduceMRF SUM [sel] [] = Just $ MRF (MapFun $ toFun1 sel) (R.+) id
reduceMRF MIN [] [] = Just $ MRF (MapFun id) (\a b -> if' (a R.< b) a b) id
reduceMRF MIN [sel] [] =
    Just $ MRF (MapFun $ \x -> expr [x, toFun1 sel x])
             (\a b -> if' (a R.!! 1 R.< b R.!! 1) a b)
             (R.!! 0)
reduceMRF MAX [] [] = Just $ MRF (MapFun id) (\a b -> if' (a R.> b) a b) id
reduceMRF MAX [sel] [] =
    Just $ MRF (MapFun $ \x -> expr [x, toFun1 sel x])
             (\a b -> if' (a R.!! 1 R.> b R.!! 1) a b)
             (R.!! 0)
reduceMRF DISTINCT [] [] = Just $ MRF (MapFun $ \a -> expr [a]) (\a b -> nub (a R.++ b)) id
reduceMRF _ _ _ = Nothing

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
rewrite1 :: TermType -> [Chain] -> [(T.Text, Chain)] -> MRF
rewrite1 ttype args optargs = MRF maps red finals where
  (finally2, [mr]) = extract Nothing ttype args optargs
  MRF maps red fin1 = mr
  finals = finally2 . fin1

-- | Rewrite a command that combines the result of multiple map/reduce
-- operations into a single map/reduce operation
rewritex :: TermType -> [Chain] -> [(Key, Chain)] -> MRF
rewritex ttype args optargs = MRF maps reduces finallys where
  (finally, mrs) = extract (Just 0) ttype args optargs
  index = zip ([0..] :: [Int])
  maps = MapFun $ \x -> expr $ map (($ x) . getMapFun) mrs
  reduces a b = expr $ map (uncurry $ mkReduce a b) . index $ map _mrfReduceFun mrs
  finallys = let fs = map _mrfFinally mrs in
       \x -> finally . expr . map (uncurry $ mkFinally x) $ index fs
  mkReduce a b i f = f (op NTH (a, i)) (op NTH (b, i))
  mkFinally x i f = f (op NTH (x, i))
  getMapFun (MRF (MapFun f) _ _) = f
  getMapFun (MRF (ConcatMapFun f) _ _) = f
  getReduceFun (MRF (MapFun _) f _) = f
  getReduceFun (MRF (ConcatMapFun _) f _) =
    \a b -> flip apply [a R.++ b] $ \l ->
      if' (isEmpty l) ([] :: [()]) [reduce1 f l]
  getFinallyFun (MRF (MapFun _) _ f) = f
  getFinallyFun (MRF (ConcatMapFun _) _ f) =
    f . maybe (R.! 0) (\base s ->
                        flip apply [s] $ handle (const base) $ s R.! 0) ?mbase

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
      extractOne chain = either (return . const) go $ chainToMRF chain
      go mrf = do
        tell [mrf]
        st' <- get
        case st' of
          Nothing -> return id
          Just n -> do
            put $ Just $ n + 1
            return $ \v -> op NTH (v, n)