{-# LANGUAGE DataKinds, TypeOperators, ConstraintKinds, FlexibleContexts, PolyKinds #-}

-- | Functions from the ReQL (RethinkDB Query Language)

module Database.RethinkDB.Functions where

import Database.RethinkDB.Term
import Database.RethinkDB.Type as T

import Database.RethinkDB.Protobuf.Ql2.Term2.TermType

import qualified Prelude as P

(+), add, (-), sub, (*), mul, (/), div, div', mod, mod'
  :: (a ~~ Number, b ~~ Number) => a -> b -> Term Number
(+) a b = op ADD (a, b) []
(-) a b = op SUB (a, b) []
(*) a b = op MUL (a, b) []
(/) a b = op DIV (a, b) []
add = (+)
sub = (-)
mul = (*)
div = (/)
div' = (/)
mod a b = op MOD (a, b) []
mod' = mod

or, or', and, and' :: (a ~~ T.Bool, b ~~ T.Bool) => a -> b -> Term Bool
or a b = op ANY (a, b) []
and a b = op ALL (a, b) []
or' = or
and' = and

(==), (!=), eq, ne :: (a ~~~ Datum, b ~~~ Datum) => a -> b -> Term Bool
eq a b = op EQ (a, b) []
ne a b = op NE (a, b) []
(==) = eq
(!=) = ne

(>), (>=), (<), (<=), gt, lt, ge, le
  :: (a ~~~ Datum, b ~~~ Datum) => a -> b -> Term Bool
gt a b = op GT (a, b) []
lt a b = op LT (a, b) []
ge a b = op GE (a, b) []
le a b = op LE (a, b) []
(>) = gt
(>=) = ge
(<) = lt
(<=) = le

not, not' :: (a ~~ Bool) => a -> Term Bool
not a = op NOT [a] []
not' = not

-- * Lists and Streams

count :: (a ~~ Sequence) => a -> Term Number
count e = op COUNT [e] []

(++), concat :: (a ~~ Sequence, b ~~ Sequence) => a -> b -> Term Sequence
(++) a b = op UNION (a, b) []
concat = (++)

map, map' :: (a ~~ Sequence, f ~~~ Function '[Datum] Datum) => f -> a -> Term Sequence
map f a = op MAP (a, f) []
map' = map

filter', filter :: (a ~~ Sequence, f ~~~ Function '[Datum] Bool) => f -> a -> Term Sequence
filter f a = op FILTER (a, f) []
filter' = filter

between :: (a ~~~ Datum, b ~~~ Datum) => a -> b -> Term Sequence -> Term Sequence
between a b e = op BETWEEN [e] ["left_bound" := a, "right_bound" := b]

append :: (a ~~~ Datum, b ~~ Sequence) => a -> b -> Term Sequence
append a b = op APPEND (b, a) []

concatMap, concatMap' :: (f ~~~ Function '[Datum] Datum, a ~~ Sequence)
  => f -> a -> Term Sequence
concatMap f e = op CONCATMAP (e, f) []
concatMap' = concatMap

innerJoin, outerJoin :: (f ~~~ Function '[T.Object, T.Object] Bool, a ~~ Sequence, b ~~ Sequence)
          => f -> a -> b -> Term Sequence
innerJoin f a b = op INNER_JOIN (a, b, f) []
outerJoin f a b = op OUTER_JOIN (a, b, f) []

eqJoin :: (a ~~ Sequence, b ~~ Sequence) => a -> Key -> b -> Term Sequence
eqJoin a k b = op EQ_JOIN (a, k, b) []

drop, drop' :: (a ~~ Number, b ~~ Sequence) => a -> b -> Term Sequence
drop a b = op SKIP (b, a) []
drop' = drop

take, take' :: (a ~~ Number, b ~~ Sequence) => a -> b -> Term Sequence
take a b = op LIMIT (a, b) []
take' = take

slice :: (a ~~ Number, b ~~ Number, c ~~ Sequence) => a -> b -> c -> Term Sequence
slice n m s = op SLICE (s, n, m) []

(!!), nth :: (a ~~ Sequence, b ~~ Number) => a -> b -> Term Datum
nth n s = op NTH (s, n) []
s !! n = op NTH (s, n) []

fold :: (f ~~~ Function '[base, x] Datum, b ~~ base, s ~~ Sequence) => f -> b -> a -> Term Datum
fold f b s = op REDUCE (f, s) ["base" := b]

fold1 :: (f ~~~ Function '[base, x] Datum, s ~~ Sequence) => f -> a -> Term Datum
fold1 f b s = op REDUCE (f, s) []

distinct :: (s ~~ Sequence) => s -> Term Sequence
distinct s = op DISTINCT [s] []
{-
groupedMapReduce ::
  (group ~~~ Function '[Datum] Datum,
   map ~~~ Function '[Object] x,
   reduce ~~~ Function

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
-}
