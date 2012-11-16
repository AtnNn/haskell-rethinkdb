{-# LANGUAGE DataKinds, TypeFamilies, ConstraintKinds, FlexibleInstances, 
             FlexibleContexts, TypeOperators #-}

-- | Functions from the RQL (RethinkDB Query Language)

module Database.RethinkDB.Functions where

import Database.RethinkDB.Types
import Database.RethinkDB.Driver

import Text.ProtocolBuffers.Basic hiding (Default)
import qualified Database.RethinkDB.Internal.Types as QL
import qualified Database.RethinkDB.Internal.Query_Language.Term as QLTerm
import qualified Database.RethinkDB.Internal.Query_Language.Builtin as QLBuiltin

import qualified Prelude as P
import Prelude (Bool(..), ($), Maybe(..), Int, String, flip, undefined, return, (.))

import Data.Maybe
import Data.Functor
import Data.Aeson
import qualified Data.Sequence as Seq

-- * Numbers, Booleans and Comparisons

signum, signum' :: (ToValue e, ToValueType (ExprType e) ~ NumberType) => e -> NumberExpr
signum x = bind x (\n -> if' (n `lt` (0 :: NumberExpr)) (-1 :: Int)
                         (if' (n `lt` (0 :: NumberExpr)) (1 :: Int) (0 :: Int)))
signum' = signum

(+), plus, (-), minus, (*), times, (/), divide, mod, mod'
  :: (HaveValueType a b NumberType) => a -> b -> NumberExpr

(+) a b = simpleOp QL.ADD [value a, value b]
(-) a b = simpleOp QL.SUBTRACT [value a, value b]
(*) a b = simpleOp QL.MULTIPLY [value a, value b]
(/) a b = simpleOp QL.DIVIDE [value a, value b]
plus = (+)
minus = (-)
times = (*)
divide = (/)

mod a b = simpleOp QL.MODULO [value a, value b]
mod' = mod

or, or', and, and' :: HaveValueType a b BoolType => a -> b -> BoolExpr
or a b = simpleOp QL.ANY [value a, value b]
and a b = simpleOp QL.ALL [value a, value b]
or' = or
and' = and

(==), (!=), eq, neq :: (HasValueType a x, HasValueType b y) => a -> b -> BoolExpr
eq a b = comparison QL.EQ [value a, value b]
neq a b = comparison QL.NE [value a, value b]
(==) = eq
(!=) = neq

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

filter', filter :: (ToMapping m, ToStream e,
                    MappingFrom m `HasToStreamValueOf` e) =>
                   m -> e -> Expr (ExprType e)
filter' fil e = Expr $ do
  mapp <- mapping fil
  (vw, term) <- exprV e
  withView vw $ rapply [return term] $ (op QL.FILTER) {
    QL.filter = Just $ QL.Filter $ mappingToPredicate mapp }

filter = filter'

between :: (ToJSON a, ToStream e, ObjectType `HasToStreamValueOf` e) =>
           (Maybe a) -> (Maybe a) -> e -> Expr (ExprType e)
between a b e = Expr $ do
  (vw, term) <- exprV e
  withView vw $ rapply [return term] (op QL.RANGE) {
          QL.range = Just $ QL.Range (viewKeyAttr vw)
                     (fmap toJsonTerm a) (fmap toJsonTerm b) }

slice :: (ToExpr c, ExprType c ~ StreamType w t, HaveValueType a b NumberType)
         => a -> b -> c -> ValueExpr x
slice a b c = simpleOp QL.SLICE [expr c, value a, value b]

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

eqJoin = undefined
{-
eqJoin :: (ToStream a, ObjectType `HasToStreamValueOf` a,
           ToStream b, ObjectType `HasToStreamValueOf` b) =>
           a -> String -> b -> String -> Expr (StreamType False ObjectType)
eqJoin this k1 other k2 = flip concatMap k1 $ \row ->
  bind (get (row ! k1)) $ \right ->
    if' (right != ()) [obj ["left" := row, "right" := right]] []  
-}
drop = undefined
drop' = drop
skip = drop

limit  = undefined

trim = undefined

nth = undefined

-- | The empty list expression
nil :: ValueExpr ArrayType
nil = toExpr ([] :: [()])

union = undefined
union' = union

reduce = undefined
fold = reduce

distinct = undefined

groupedMapReduce = undefined

forEach = undefined

zip = undefined
zip' = zip

orderBy = undefined
sortBy = orderBy

groupBy = undefined
groupBy' = groupBy

-- * Accessors

(!) :: (HasValueType a ObjectType) => a -> String -> ValueExpr t
(!) a b = mkExpr $ rapply [value a] (op QL.GETATTR) {
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

pluck ks = map (pick ks)

without ks = map (unpick ks)

merge = undefined

-- * Controld Structures, Functions and Javascript

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

let' :: (ToValue e) => String -> e -> Expr t -> Expr t
let' nam val e = Expr $ do
  arg <- value val
  (vw, body) <- exprV e
  withView vw $ return defaultValue {
    QLTerm.type' = QL.LET,
    QLTerm.let' = Just $ QL.Let (Seq.singleton $
                                 QL.VarTermTuple (uFromString nam) arg)
                  body }

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

jsfun :: ToValue e => String -> e -> Expr (ValueType y)
jsfun f e = mkExpr $ do 
  v <- newVar
  expr (let' v e $ js $ f P.++ "(" P.++ v P.++ ")")

error = undefined
error' = error

class CanConcat (a :: ValueTypeKind)
instance CanConcat StringType
instance CanConcat ArrayType