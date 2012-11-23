{-# LANGUAGE DataKinds, TypeFamilies, ConstraintKinds, FlexibleInstances, 
             FlexibleContexts, TypeOperators, ViewPatterns #-}

-- | Functions from the ReQL (RethinkDB Query Language)

module Database.RethinkDB.Functions where

import Database.RethinkDB.Types
import Database.RethinkDB.Driver

import Text.ProtocolBuffers.Basic hiding (Default)
import qualified Database.RethinkDB.Internal.Types as QL
import qualified Database.RethinkDB.Internal.Query_Language.Term as QLTerm
import qualified Database.RethinkDB.Internal.Query_Language.WriteQuery as QLWriteQuery
import qualified Database.RethinkDB.Internal.Query_Language.Builtin as QLBuiltin
import qualified Database.RethinkDB.Internal.Query_Language.Builtin.OrderBy as QLOrderBy
import qualified Database.RethinkDB.Internal.Query_Language.Reduction as QLReduction

import qualified Prelude as P
import Prelude (Bool(..), ($), Maybe(..), Int, String, flip, undefined, return, (.), mapM)

import Control.Monad (liftM)
import Data.Maybe
import Data.Functor
import Data.Aeson
import qualified Data.Sequence as Seq

-- * Numbers, Booleans and Comparisons

signum, signum' :: (ToValue e, ToValueType (ExprType e) ~ NumberType) => e -> NumberExpr
signum x = bind x (\n -> if' (n `lt` (0 :: NumberExpr)) (-1 :: Int)
                         (if' (n `lt` (0 :: NumberExpr)) (1 :: Int) (0 :: Int)))
signum' = signum

(+), add, (-), sub, (*), mul, (/), div', mod, mod'
  :: (HaveValueType a b NumberType) => a -> b -> NumberExpr

(+) a b = simpleOp QL.ADD [value a, value b]
(-) a b = simpleOp QL.SUBTRACT [value a, value b]
(*) a b = simpleOp QL.MULTIPLY [value a, value b]
(/) a b = simpleOp QL.DIVIDE [value a, value b]
add = (+)
sub = (-)
mul = (*)
div' = (/)

mod a b = simpleOp QL.MODULO [value a, value b]
mod' = mod

or, or', and, and' :: HaveValueType a b BoolType => a -> b -> BoolExpr
or a b = simpleOp QL.ANY [value a, value b]
and a b = simpleOp QL.ALL [value a, value b]
or' = or
and' = and

(==), (!=), eq, ne :: (HasValueType a x, HasValueType b y) => a -> b -> BoolExpr
eq a b = comparison QL.EQ [value a, value b]
ne a b = comparison QL.NE [value a, value b]
(==) = eq
(!=) = ne

(>), (>=), (<), (<=), gt, lt, ge, le
  :: (HaveValueType a b v, CanCompare v) => a -> b -> BoolExpr
gt a b = comparison QL.GT [value a, value b]
lt a b = comparison QL.LT [value a, value b]
ge a b = comparison QL.GE [value a, value b]
le a b = comparison QL.LE [value a, value b]
(>) = gt
(>=) = ge
(<) = lt
(<=) = le

not, not' :: HasValueType a BoolType => a -> BoolExpr
not a = simpleOp QL.NOT [value a]
not' = not

-- * Lists and Streams

length, count :: (ToExpr e, Sequence (ExprType e)) => e -> NumberExpr
count e = simpleOp QL.LENGTH [expr e]
length = count

(++), concat :: (HaveValueType a b v, CanConcat v) => a -> b -> Expr (ValueType v)
(++) a b = simpleOp QL.ADD [value a, value b]
concat = (++)

map, map' :: (ToMapping m, ToStream e, MappingFrom m `HasToStreamValueOf` e) =>
       m -> e -> Expr (StreamType False (MappingTo m))
map fun a = mkExpr $ do
  mapp <- mapping fun
  rapply [expr a] $ (op QL.MAP) {
    QL.map = Just $ QL.Map mapp }

map' = map

-- | Get all the documents for which the given predicate is true
filter', filter :: (ToMapping m, ToStream e,
                    MappingFrom m `HasToStreamValueOf` e) =>
                   m -> e -> Expr (ExprType e)
filter' fil e = Expr $ do
  mapp <- mapping fil
  (vw, term) <- exprV e
  withView vw $ rapply [return term] $ (op QL.FILTER) {
    QL.filter = Just $ QL.Filter $ mappingToPredicate mapp }

filter = filter'

-- | Get all documents between two primary keys (both keys are inclusive)
between :: (ToJSON a, ToStream e, ObjectType `HasToStreamValueOf` e) =>
           (Maybe a) -> (Maybe a) -> e -> Expr (ExprType e)
between a b e = Expr $ do
  (vw, term) <- exprV e
  withView vw $ rapply [return term] (op QL.RANGE) {
          QL.range = Just $ QL.Range (viewKeyAttr vw)
                     (fmap toJsonTerm a) (fmap toJsonTerm b) }

append :: (HasValueType a ArrayType, HasValueType b x) => a -> b -> ArrayExpr
append a b = simpleOp QL.ARRAYAPPEND [value a, value b]

concatMap, concatMap' ::
  (ToMapping m, (MappingTo m) ~ ArrayType,
   ToStream e, MappingFrom m `HasToStreamValueOf` e) =>
   m -> e -> Expr (StreamType False t)
concatMap fun e = mkExpr $ do
  mapp <- mapping fun
  rapply [stream e] (op QL.CONCATMAP) {
    QL.concat_map = Just $ QL.ConcatMap mapp }

concatMap' = concatMap

innerJoin :: (ToStream a, l `HasToStreamValueOf` a,
              ToStream b, r `HasToStreamValueOf` b) =>
             a -> b -> (ValueExpr l -> ValueExpr r -> BoolExpr) ->
             Expr (StreamType False ObjectType)
innerJoin self other p =
  flip concatMap self
  (\row -> flip concatMap other
           (\row2 -> if' (p row row2)
                     [obj ["left" := row, "right" := row2]]
                     nil))

outerJoin :: (ToStream a, l `HasToStreamValueOf` a,
              ToStream b, r `HasToStreamValueOf` b) =>
             a -> b -> (ValueExpr l -> ValueExpr r -> BoolExpr) ->
             Expr (StreamType False ObjectType)
outerJoin self other p =
  flip concatMap' self
  (\row -> bind (flip concatMap' other
                 (\row2 -> if' (p row row2)
                           [obj ["left" := row, "right" := row2]]
                           nil))
           (\matches ->
             if' (count matches `gt` (0 :: Int)) (asArray matches) [obj ["left" := row]]))

asArray :: ToExpr e => e -> ArrayExpr
asArray e = mkExpr $ expr e

eqJoin :: (ToStream a, ObjectType `HasToStreamValueOf` a,
           ToExpr b, ExprType b ~ ExprType Selection) =>
           a -> String -> b -> Expr (StreamType False ObjectType)
eqJoin this k1 other =
  flip concatMap this $ \row ->
    bind (get other (row ! k1)) $ \right ->
      if' (right != ()) [obj ["left" := row, "right" := right]] nil

drop, drop', skip :: (ToStream e, t `HasToStreamValueOf` e,
         ToExpr n, ExprType n ~ ValueType NumberType) =>
        e -> n -> Expr (StreamType (ExprIsView e) t)
drop e n = Expr $ do
  (vw, ex) <- exprV e
  withView vw $ rapply [return ex, expr (), expr n] (op QL.SLICE)
drop' = drop
skip = drop

take, take', limit :: (ToStream e, t `HasToStreamValueOf` e,
         ToExpr n, ExprType n ~ ValueType NumberType) =>
        e -> n -> Expr (StreamType (ExprIsView e) t)
take e n = Expr $ do
  (vw, ex) <- exprV e
  withView vw $ rapply [return ex, expr n, expr ()] (op QL.SLICE)
take' = take
limit  = take

slice :: (ToStream e, t `HasToStreamValueOf` e,
         ToExpr n, ExprType n ~ ValueType NumberType,
         ToExpr m, ExprType m ~ ValueType NumberType) =>
        e -> n -> m -> Expr (StreamType (ExprIsView e) t)
slice e n m = Expr $ do
  (vw, ex) <- exprV e
  withView vw $ rapply [return ex, expr n, expr m] (op QL.SLICE)

(!!), nth :: (ToStream e, t `HasToStreamValueOf` e,
        ToExpr n, ExprType n ~ ValueType NumberType) =>
        e -> n -> ValueExpr t
nth e n = Expr $ withView NoView $ rapply [stream e, expr n] (op QL.NTH)
(!!) = nth

-- | The empty list expression
nil :: ValueExpr ArrayType
nil = toExpr ([] :: [()])

union, union' :: (ToStream a, t `HasToStreamValueOf` a,
           ToStream b, t `HasToStreamValueOf` b) =>
          a -> b -> Expr (StreamType False t)
union a b = simpleOp QL.UNION [stream a, stream b]
union' = union

-- | A fold
-- 
-- >>> run h $ reduce [1,2,3] (0 :: Int) (+)
-- 6

fold :: (ToValue z, ToValueType (ExprType z) ~ a,
         ToStream e, b `HasToStreamValueOf` e,
         ToExpr c, ExprIsView c ~ False) =>
        (ValueExpr a -> ValueExpr b -> c) -> z -> e -> Expr (ExprType c)
fold f a e = Expr $ do
  v1 <- newVar
  v2 <- newVar
  aq <- value a
  result <- expr (f (var v1) (var v2))
  withView NoView $ rapply [stream e] (op QL.REDUCE) {
    QL.reduce = Just $ QL.Reduction {
       QL.base = aq,
       QL.var1 = uFromString v1,
       QL.var2 = uFromString v2, 
       QLReduction.body = result } }

reduce :: (ToValue z, ToValueType (ExprType z) ~ a,
         ToStream e, b `HasToStreamValueOf` e,
         ToExpr c, ExprIsView c ~ False) =>
        e -> z -> (ValueExpr a -> ValueExpr b -> c) -> Expr (ExprType c)
reduce this base f = fold f base this

distinct :: (ToStream e, v `HasToStreamValueOf` e) =>
            e -> Expr (StreamType False v)
distinct e = simpleOp QL.DISTINCT [stream e]

-- TODO: does not work
groupedMapReduce :: (ToValue group, ToValue value,
                     ToValue acc, ToValueType (ExprType acc) ~ b,
                     ToValue acc', ToValueType (ExprType acc') ~ b,
                     ToStream e, a `HasToStreamValueOf` e) =>
                    (ValueExpr a -> group) ->
                    (ValueExpr a -> value) ->
                    acc ->
                    (ValueExpr b -> ValueExpr v -> acc') ->
                    e -> 
                    Expr (StreamType False b)
groupedMapReduce group val base reduction e = Expr $ do
  g <- mapping group
  v <- mapping val
  v1 <- newVar
  v2 <- newVar
  b <- value base
  result <- value (reduction (var v1) (var v2))
  withView NoView $ rapply [stream e] (op QL.GROUPEDMAPREDUCE) {
    QL.grouped_map_reduce = Just $ QL.GroupedMapReduce {
       QL.group_mapping = g,
       QL.value_mapping = v,
       QL.reduction = QL.Reduction {
         QL.base = b, 
         QL.var1 = uFromString v1,
         QL.var2 = uFromString v2,
         QLReduction.body = result }} }

-- | Execute a write query for each element of the stream
-- 
-- >>> run h $ forEach [1,2,3::Int] (\x -> insert (table "fruits") (obj ["n" := x]))

forEach :: (ToStream a, v `HasToStreamValueOf` a) =>
           a -> (ValueExpr v -> WriteQuery b) -> WriteQuery ()
forEach s mkwq = WriteQuery (do
  arg <- newVar
  let wq = mkwq (var arg)
  qlwq <- writeQueryBuild wq
  as <- stream s
  return $ defaultValue {
    QLWriteQuery.type' = QL.FOREACH,
    QL.for_each = Just $ QL.ForEach as (uFromString arg) (Seq.singleton qlwq) })
  (whenSuccess_ ())

zip, zip' :: (ToStream e, ObjectType `HasToStreamValueOf` e) =>
             e -> Expr (StreamType False ObjectType)
zip = map (\row -> if' (row !? "right")
                   (merge (row ! "left") (row ! "right"))
                   (row ! "left"))
zip' = zip

data Order = Asc  { orderAttr :: String }
           | Desc { orderAttr :: String }

orderAscending :: Order -> Bool
orderAscending Asc  {} = True
orderAscending Desc {} = False

class ToOrder a where toOrder :: a -> Order
instance ToOrder String where toOrder = Asc
instance ToOrder Order where toOrder o = o

orderBy :: (ToOrder o, ToStream e, a `HasToStreamValueOf` e) =>
           [o] -> e -> Expr (StreamType (ExprIsView e) a)
orderBy o e = Expr $ do
  (vw, ex) <- exprV e
  withView vw $ rapply [return ex] (op QL.ORDERBY) {
    QL.order_by = Seq.fromList $ flip P.map o $ \(toOrder -> x) -> QL.OrderBy {
       QLOrderBy.attr = uFromString (orderAttr x), QL.ascending = Just $ orderAscending x }}

groupBy,groupBy' :: (ToStream e, ObjectType `HasToStreamValueOf` e) =>
                    [String] -> MapReduce ObjectType b c d -> e -> Expr (StreamType False d)
groupBy ks (MapReduce m b r f) e = map f (groupedMapReduce (pick ks) m b r e)
groupBy' = groupBy

data MapReduce a b c d = MapReduce (ValueExpr a -> ValueExpr b) (ValueExpr c)
                         (ValueExpr c -> ValueExpr b -> ValueExpr c)
                         (ValueExpr c -> ValueExpr d)

sum, sum' :: String -> MapReduce ObjectType NumberType NumberType NumberType
sum a = MapReduce (! a) 0 (+) P.id
sum' = sum

count' :: MapReduce ObjectType NoneType NumberType NumberType
count' = MapReduce (P.const (toExpr ())) 0 (\x _ -> x + (1 :: Int)) P.id

avg :: String -> MapReduce ObjectType ArrayType ArrayType NumberType
avg k = MapReduce (\x -> toExpr [x ! k :: NumberExpr, 1]) (toExpr [0,0 :: Int])
        (\a o -> toExpr [(a !! (0 :: Int)) + (o !! (0 :: Int)), (a !! (1 :: Int)) + (o !! (1 :: Int)) :: NumberExpr])
        (\a -> ((a !! (0 :: Int)) / (a !! (1 :: Int))))

-- * Accessors

-- | Get the value of the field of an object
-- 
-- When GHC thinks the result is ambiguous, it may have to be annotated.
-- 
-- >>> run h $ (get (table "tea") "black" ! "water_temperature" :: NumberExpr)
-- 95

(!) :: (ToExpr a, ExprValueType a ~ ObjectType) => a -> String -> ValueExpr t
(!) a b = mkExpr $ rapply [expr a] (op QL.GETATTR) {
  QLBuiltin.attr = Just (uFromString b) }

pick :: HasValueType e ObjectType => [String] -> e -> ObjectExpr
pick ks e = mkExpr $ rapply [value e] (op QL.PICKATTRS) {
  QL.attrs = Seq.fromList $ P.map uFromString ks }

unpick :: HasValueType e ObjectType => [String] -> e -> ObjectExpr
unpick ks e = mkExpr $ rapply [value e] (op QL.WITHOUT) {
  QL.attrs = Seq.fromList $ P.map uFromString ks }

(!?) :: (HasValueType a ObjectType) => a -> String -> BoolExpr 
(!?) a b = mkExpr $ rapply [value a] (op QL.HASATTR) {
  QLBuiltin.attr = Just $ uFromString b }

pluck :: (ToStream e, ObjectType `HasToStreamValueOf` e) =>
         [String] -> e -> Expr (StreamType False ObjectType)
pluck ks = map (pick ks)

without :: (ToStream e, ObjectType `HasToStreamValueOf` e) =>
           [String] -> e -> Expr (StreamType False ObjectType)
without ks = map (unpick ks)

merge :: (ToExpr a, ExprType a ~ ValueType ObjectType,
          ToExpr b, ExprType b ~ ValueType ObjectType) =>
         a -> b -> ObjectExpr
merge this other = simpleOp QL.MAPMERGE [expr this, expr other]

-- * Control Structures, Functions and Javascript

-- | A javascript expression
-- 
-- It is often necessary to specify the result type:
-- 
-- >>> run h $ (js "1 + 2" :: NumberExpr)
-- 3

js :: String -> Expr (ValueType any)
js s = mkExpr $ return defaultValue {
  QLTerm.type' = QL.JAVASCRIPT,
  QLTerm.javascript = Just $ uFromString ("return (" P.++ s P.++ ")") }

bind :: (ToValue e) => e -> (ValueExpr (ToValueType (ExprType e)) -> Expr t) -> Expr t
bind val f = Expr $ do
  arg <- value val
  v <- newVar
  (vw, body) <- exprV (f (var v))
  withView vw $ return defaultValue {
    QLTerm.type' = QL.LET,
    QLTerm.let' = Just $ QL.Let (Seq.singleton $ QL.VarTermTuple (uFromString v) arg)
                  body }

let' :: [Attribute] -> Expr t -> Expr t
let' pairs e = Expr $ do
  varTerms <- mapM (\(k := v) ->
                     (QL.VarTermTuple (uFromString k)) `liftM` value v) pairs
  (vw, body) <- exprV e
  withView vw $ return defaultValue {
    QLTerm.type' = QL.LET,
    QLTerm.let' = Just $ QL.Let (Seq.fromList $ varTerms) body }

var :: ExprIsView (Expr t) ~ False => String -> Expr t
var v = mkExpr $ return defaultValue { 
  QLTerm.type' = QL.VAR,
  QLTerm.var = Just $ uFromString v
  }

if' :: (ToValue e, ToValueType (ExprType e) ~ BoolType,
        ToExpr a, ExprTypeNoView (ExprType a) ~ x,
        ToExpr b, ExprTypeNoView (ExprType b) ~ x,
        ExprTypeIsView x ~ False) =>
          e -> a -> b -> Expr x
if' t a b = mkExpr $ do
  tq <- value t
  aq <- expr a
  bq <- expr b
  return defaultValue {
    QLTerm.type' = QL.IF, QL.if_ = Just $ QL.If tq aq bq }

-- | A javascript function
-- 
-- >>> let squareRoot = jsfun "Math.sqrt" :: NumberExpr -> NumberExpr
-- >>> run h $ squareRoot 5 :: IO Double
-- 2.23606797749979

jsfun :: ToValue e => String -> e -> Expr (ValueType y)
jsfun f e = mkExpr $ do 
  v <- newVar
  expr (let' [v := e] $ js $ f P.++ "(" P.++ v P.++ ")")

error, error' :: (ExprTypeIsView t ~ False) => String -> Expr t
error m = Expr $ withView NoView $ return defaultValue {
  QLTerm.type' = QL.ERROR, QL.error = Just $ uFromString m }
error' = error

class CanConcat (a :: ValueTypeKind)
instance CanConcat StringType
instance CanConcat ArrayType