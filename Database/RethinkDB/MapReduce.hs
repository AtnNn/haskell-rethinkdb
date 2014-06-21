{-# LANGUAGE OverloadedStrings, PatternGuards, ImplicitParams #-}

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
  
  -- A variable is introduced to represent the sequence that f
  -- is being performed on. This variable is no longer present
  -- in the return value
  v <- newVarId
  body <- baseReQL $ f (op VAR [v])
  
  return . mrFun $ toMapReduce v body

-- | Compares the two representations of a variable
sameVar :: Int -> BaseArray -> Bool
sameVar x [BaseReQL DATUM (Just (Datum.Datum{ Datum.r_num = Just y })) _ _] =
  fromIntegral x == y
sameVar _ _ = False

-- | notNone checks that it is a map/reduce and not a constant
notNone :: MapReduce -> Bool
notNone None{} = False
notNone _ = True

-- | Helper function for casting up from BaseReQL into ReQL
wrap :: BaseReQL -> ReQL
wrap = ReQL . return

-- | Build a single argument function from a constant ReQL expression
toFun1 :: BaseReQL -> (ReQL -> ReQL)
toFun1 f a = op FUNCALL (wrap f, a)

-- | Build a two argument function from a constant ReQL expression
toFun2 :: BaseReQL -> (ReQL -> ReQL -> ReQL)
toFun2 f a b = op FUNCALL (wrap f, a, b)

-- | Represents a map-reduce operation split into its map and reduce parts
data MapReduce =
  -- | A constant
  None Tag ReQL |
  -- | A map
  Map (ReQL -> ReQL) |
  -- | A map, a reduce and maybe a finalizer
  MapReduce Tag (ReQL -> ReQL) (ReQL -> ReQL -> ReQL) (Maybe (ReQL -> ReQL))

data Tag = 
  NotSpecial |
  SingletonArray |
  AddBase MapReduce

type MapReduceFinally = (ReQL -> ReQL, ReQL -> ReQL -> ReQL, Maybe (ReQL -> ReQL))

mrFun :: MapReduce -> (ReQL -> ReQL)
mrFun (None _ t) x = op FUNCALL (t, x)
mrFun (Map f) s = op MAP (s, f)
mrFun (MapReduce _ m r f) s = fromMaybe id f rs
    where
      rs = op REDUCE (ms, r)
      ms = op MAP (s, m)

-- | Rewrites the term in the second argument to merge all uses of the
-- variable whose id is given in the first argument.
toMapReduce :: Int -> BaseReQL -> MapReduce

-- A datum stays constant
toMapReduce _ t@(BaseReQL DATUM _ _ _) = None NotSpecial $ wrap t

-- The presence of the variable 
toMapReduce v (BaseReQL VAR _ w _) | sameVar v w = Map id
toMapReduce v t@(BaseReQL type' _ args optargs) = let
  
  -- Recursively convert all arguments
  args' = map (toMapReduce v) args
  optargs' = map (\(BaseAttribute k vv) -> (k, toMapReduce v vv)) optargs
  
  -- Count how many of the arguments have been rewritten
  count = length $ filter notNone $ args' ++ map snd optargs'
  
  -- Rewrite the current term. rewrite1 is optimised for
  -- the single count case
  rewrite = (if count == 1 then rewrite1 else rewritex) type' args' optargs'
  
  in case () of
    -- Don't rewrite if there is nothing to rewrite
    _ | 0 <- count -> None (genTag type' args' optargs') (wrap t)
    
    -- Don't rewrite an operation that can be chained
    _ | (arg1 : _) <- args', notNone arg1 -> do
      fromMaybe rewrite $ mrChain type' arg1 (tail args) optargs
         
    -- Default to rewriting the term
    _ -> rewrite

-- | Chain a ReQL command onto a MapReduce operation
mrChain :: TermType -> MapReduce -> [BaseReQL] -> [BaseAttribute]
        -> Maybe MapReduce
-- TODO: add special case for REDUCE seq (ADD (MAKEARRY [base]) seq)
-- TODO: FUNCALL
-- TODO: all other commands
mrChain MAP (Map m) [f] [] = Just . Map $ toFun1 f . m
mrChain REDUCE (Map m) [f] [] = Just $ MapReduce NotSpecial m (toFun2 f) Nothing
mrChain REDUCE (MapReduce (AddBase (Map m)) _ _ _) [f] [] =
  Just $ MapReduce NotSpecial m (toFun2 f) Nothing
mrChain COUNT (Map _) [] [] =
    Just $ MapReduce NotSpecial (const (num 1)) (\a b -> op ADD (a, b)) Nothing
mrChain tt (Map m) args optargs
    | tt `elem` mappableTypes =
        Just $ Map $ (. m) $ \x ->
            op' tt (expr x : map expr args)
            (noRecurse : map baseAttrToOptArg optargs) 
mrChain _ _ _ _ = Nothing

-- | Convert from one representation to the other
baseAttrToOptArg :: BaseAttribute -> OptArg
baseAttrToOptArg (BaseAttribute k v) = k :== v

-- | This undocumented optional argument circumvents stream
-- polymorphism on some operations
noRecurse :: OptArg
noRecurse = "_NO_RECURSE_" :== True

-- | These functions map over sequences
mappableTypes :: [TermType]
mappableTypes = [GET_FIELD, PLUCK, WITHOUT, MERGE, HAS_FIELDS]

-- | Rewrite a command into a map/reduce.
--
-- This is a special case for when only one of the arguments
-- is itself a map/reduce
rewrite1 :: TermType -> [MapReduce] -> [(T.Text, MapReduce)] -> MapReduce
rewrite1 ttype args optargs = MapReduce tag maps reduce finals where
  (finally2, [mr]) = extract Nothing ttype args optargs
  (maps, reduce, finally1) = mr
  finals = Just $ maybe finally2 (finally2 .) finally1
  tag = genTag ttype args optargs

-- | Rewrite a command that combines the result of multiple map/reduce
-- operations into a single map/reduce operation
rewritex :: TermType -> [MapReduce] -> [(Key, MapReduce)] -> MapReduce
rewritex ttype args optargs = MapReduce tag maps reduces finallys where
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
  tag = genTag ttype args optargs

-- | Generate a tag describing the contents of a map/reduce object
genTag :: TermType -> [MapReduce] -> [(Key, MapReduce)] -> Tag
genTag MAKE_ARRAY [_] [] = SingletonArray
genTag ADD [None SingletonArray _, mr] [] = AddBase mr
genTag _ _ _ = NotSpecial

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
  Maybe Int -> TermType -> [MapReduce] -> [(Key, MapReduce)]
  -> (ReQL -> ReQL, [MapReduceFinally])
extract st tt args optargs = fst $ flip runState st $ runWriterT $ do
  args' <- sequence $ map extractOne args
  optargvs' <- sequence $ map extractOne (map snd optargs)
  let optargks = map fst optargs
  return $ \v -> op' tt (map ($ v) args') (zipWith (:==) optargks $ map ($ v) optargvs')
    where
      extractOne (None _ term) = return $ const term
      extractOne (Map f) = go (\x -> op MAKE_ARRAY [f x], \a b -> op ADD (a,b), Nothing)
      extractOne (MapReduce _ m r f) = go (m, r, f)
      go mrf = do
        tell [mrf]
        st' <- get
        case st' of
          Nothing -> return id
          Just n -> do
            put $ Just $ n + 1
            return $ \v -> op NTH (v, n)