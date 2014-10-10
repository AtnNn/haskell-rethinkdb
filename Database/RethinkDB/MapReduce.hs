{-# LANGUAGE OverloadedStrings, PatternGuards #-}

module Database.RethinkDB.MapReduce where -- (termToMapReduce) where

import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Text as T
import Data.Maybe
import Data.Foldable (toList)

import Database.RethinkDB.Wire.Term
import Database.RethinkDB.ReQL
import Database.RethinkDB.Types

import qualified Database.RethinkDB.Functions as R
import Database.RethinkDB.NoClash hiding (get, collect, args)
import Database.RethinkDB.Datum

-- | Takes a function that takes a sequence as an argument, and
-- returns a function that only uses that sequence once, by merging
-- the map and reduce operations. This is used by groupBy.
termToMapReduce :: (ReQL -> ReQL) -> State QuerySettings (ReQL -> ReQL)
termToMapReduce f = do
  
  -- A variable is introduced to represent the sequence that f
  -- is being performed on. This variable is no longer present
  -- in the return value
  v <- newVarId
  body <- runReQL $ f (op VAR [v])
  
  return . applyChain $ toMapReduce v body

-- | Compares the two representations of a variable
sameVar :: Int -> [Term] -> Bool
sameVar x [Datum d] | Success y <- fromDatum d = x == y
sameVar _ _ = False

-- | notNone checks that it is a map/reduce and not a constant
notConst :: Chain -> Bool
notConst None{} = False
notConst SingletonArray{} = False
notConst _ = True

-- | Helper function for casting up from Term into ReQL
wrap :: Term -> ReQL
wrap = ReQL . return

-- | Build a single argument function from a constant ReQL expression
toFun1 :: Term -> (ReQL -> ReQL)
toFun1 f a = op FUNCALL (wrap f, a)

-- | Build a two argument function from a constant ReQL expression
toFun2 :: Term -> (ReQL -> ReQL -> ReQL)
toFun2 f a b = op FUNCALL (wrap f, a, b)

-- | Represents a map/reduce operation split into its map and reduce parts
data MRF = MRF {
      _mrfMapFun :: MapFun,
      _mrfReduceFun :: ReQL -> ReQL -> ReQL,
      _mrfBase :: Maybe ReQL,
      _mrfFinally :: ReQL -> ReQL }

-- | A Chain of ReQL expressions that might be transformed into
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

-- | Convert a Chain back into a ReQL function
applyChain :: Chain -> (ReQL -> ReQL)
applyChain (None t) x = op FUNCALL (t, x)
applyChain (Map maps) s = applyMaps maps s
applyChain (MapReduceChain maps red) s =
  applyReduce red $ applyMaps maps s
applyChain (MapReduce mrf) s = applyMRF mrf s
applyChain (SingletonArray x) s = op FUNCALL (op MAKE_ARRAY [x], s)
applyChain (AddBase b c) s = applyChain c s `union` [b]

-- | Convert an MRF into a ReQL function
applyMRF :: MRF -> ReQL -> ReQL
applyMRF (MRF m r Nothing f) s = f `apply` [reduce r (applyMapFun m s)]
applyMRF (MRF m r (Just base) f) s =
  f $
  apply (\x -> branch (isEmpty x) base (x R.! 0)) . return $
  reduce (\a b -> [a R.! 0 `r` b R.! 0]) $
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
collect = MRF (MapFun $ \x -> expr [x]) union (Just (expr ())) id

-- | Rewrites the term in the second argument to merge all uses of the
-- variable whose id is given in the first argument.
toMapReduce :: Int -> Term -> Chain

toMapReduce v (Note _ t) = toMapReduce v t -- TODO: keep notes

-- Singletons are singled out
toMapReduce _ (Datum (Array a))
  | [datum] <- toList a =
    SingletonArray . wrap $ Datum datum

-- A datum stays constant
toMapReduce _ t@(Datum _) = None $ wrap t

-- The presence of the variable 
toMapReduce v (Term VAR w _) | sameVar v w = Map []

-- An arbitrary term
toMapReduce v t@(Term type' args optargs) = let
  
  -- Recursively convert all arguments
  args' = map (toMapReduce v) args
  optargs' = map (\(TermAttribute k vv) -> (k, toMapReduce v vv)) optargs
  
  -- Count how many of the arguments have been rewritten
  nb = length $ filter notConst $ args' ++ map snd optargs'
  
  -- Rewrite the current term. rewrite1 is optimised for
  -- the single count case
  rewrite = MapReduce $
            (if nb == 1 then rewrite1 else rewritex) type' args' optargs'
  
  in case nb of
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

singleton :: TermType -> [Chain] -> [TermAttribute] -> Maybe ReQL
singleton MAKE_ARRAY [None el] [] = Just el
singleton _ _ _ = Nothing

-- | Chain a ReQL command onto a MapReduce operation
mrChain :: TermType -> Chain -> [Term] -> [TermAttribute] -> Maybe Chain

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
mapMRF :: TermType -> [Term] -> [TermAttribute]
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
  Just . ConcatMapFun $ \x -> branch (toFun1 f x # handle (const False)) x ()
mapMRF FILTER [f] [TermAttribute "default" defval] =
    Just . ConcatMapFun $ \x -> branch (toFun1 f x # handle (const defval)) x ()
mapMRF GET_FIELD [attr] [] =
  Just . ConcatMapFun $ \x ->
  branch (op' HAS_FIELDS (x, wrap attr) [noRecurse])
  [op' GET_FIELD (x, attr) [noRecurse]] ()
mapMRF HAS_FIELDS sel [] =
  Just . ConcatMapFun $ \x ->
  branch (op' HAS_FIELDS (x : map wrap sel) [noRecurse]) [x] ()
mapMRF WITH_FIELDS sel [] =
  Just . ConcatMapFun $ \x ->
  branch (op' HAS_FIELDS (x : map wrap sel) [noRecurse])
  [op' PLUCK (x : map wrap sel) [noRecurse]] ()
mapMRF _ _ _ = Nothing

-- | Convert some of the built-in operations into a map/reduce
--
-- TODO: these have not been tested
reduceMRF :: TermType -> [Term] -> [TermAttribute]
            -> Maybe MRF
reduceMRF REDUCE [f] [] = Just $ MRF (MapFun id) (toFun2 f) Nothing id
reduceMRF COUNT [] [] = Just $ MRF (MapFun $ const (num 1)) (\a b -> op ADD (a, b)) (Just 0) id
reduceMRF AVG [] [] =
    Just $ MRF (MapFun $ \x -> expr [x, 1])
             (\a b -> expr [a R.! 0 R.+ b R.! 0, a R.! 1 R.+ b R.! 1])
             Nothing
             (\x -> x R.! 0 R./ x R.! 1)
reduceMRF SUM [] [] = Just $ MRF (MapFun id) (R.+) (Just 0) id
reduceMRF SUM [sel] [] = Just $ MRF (MapFun $ toFun1 sel) (R.+) (Just 0) id
reduceMRF MIN [] [] = Just $ MRF (MapFun id) (\a b -> branch (a R.< b) a b) Nothing id
reduceMRF MIN [sel] [] =
    Just $ MRF (MapFun $ \x -> expr [x, toFun1 sel x])
             (\a b -> branch (a R.! 1 R.< b R.! 1) a b)
             Nothing
             (R.! 0)
reduceMRF MAX [] [] = Just $ MRF (MapFun id) (\a b -> branch (a R.> b) a b) Nothing id
reduceMRF MAX [sel] [] =
    Just $ MRF (MapFun $ \x -> expr [x, toFun1 sel x])
             (\a b -> branch (a R.! 1 R.> b R.! 1) a b)
             Nothing
             (R.! 0)
reduceMRF DISTINCT [] [] = Just $ MRF (MapFun $ \a -> expr [a]) (\a b -> distinct (a `union` b)) (Just (expr ())) id
reduceMRF _ _ _ = Nothing

-- | Convert from one representation to the other
baseAttrToOptArg :: TermAttribute -> OptArg
baseAttrToOptArg (TermAttribute k v) = k := v

-- | This undocumented optional argument circumvents stream
-- polymorphism on some operations
noRecurse :: OptArg
noRecurse = "_NO_RECURSE_" := True

-- | Rewrite a command into a map/reduce.
--
-- This is a special case for when only one of the arguments
-- is itself a map/reduce
rewrite1 :: TermType -> [Chain] -> [(T.Text, Chain)] -> MRF
rewrite1 ttype args optargs = MRF maps red mbase finals where
  (finally2, [mr]) = extract Nothing ttype args optargs
  MRF maps red mbase fin1 = mr
  finals = finally2 . fin1

-- | Rewrite a command that combines the result of multiple map/reduce
-- operations into a single map/reduce operation
rewritex :: TermType -> [Chain] -> [(Key, Chain)] -> MRF
rewritex ttype args optargs = MRF maps reduces Nothing finallys where
  (finally, mrs) = extract (Just 0) ttype args optargs
  index = zip $ map expr ([0..] :: [Int])
  maps = MapFun $ \x -> expr $ map (($ x) . getMapFun) mrs
  reduces a b = expr $ map (uncurry $ mkReduce a b) . index $ map getReduceFun mrs
  finallys = let fs = map getFinallyFun mrs in
       \x -> finally . expr . map (uncurry $ mkFinally x) $ index fs
  mkReduce a b i f = f (a!i) (b!i)
  mkFinally x i f = f (x!i)
  getMapFun (MRF (MapFun f) _ _ _) = f
  getMapFun (MRF (ConcatMapFun f) _ _ _) = f
  getReduceFun (MRF (MapFun _) f _ _) = f
  getReduceFun (MRF (ConcatMapFun _) f _ _) =
    \a b -> flip apply [a `union` b] $ \l ->
      branch (isEmpty l) () [reduce f l]
  getFinallyFun (MRF (MapFun _) _ _ f) = f
  getFinallyFun (MRF (ConcatMapFun _) _ mbase f) =
    f . maybe (R.! 0) (\base s ->
                        flip apply [s] $ handle (const base) $ s R.! 0) mbase

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
  return $ \v -> op' tt (map ($ v) args') (zipWith (:=) optargks $ map ($ v) optargvs')
    where
      extractOne chain = either (return . const) go $ chainToMRF chain
      go mrf = do
        tell [mrf]
        st' <- get
        case st' of
          Nothing -> return id
          Just n -> do
            put $ Just $ n + 1
            return $ \v -> v ! expr n
