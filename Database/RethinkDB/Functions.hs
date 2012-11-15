{-# LANGUAGE DataKinds, TypeFamilies, ConstraintKinds, FlexibleInstances, 
             FlexibleContexts #-}

-- | Functions from the RQL (RethinkDB Query Language)

module Database.RethinkDB.Functions where

import Database.RethinkDB.Types
import Database.RethinkDB.Driver

import Text.ProtocolBuffers.Basic hiding (Default)
import qualified Database.RethinkDB.Internal.Types as QL
import qualified Database.RethinkDB.Internal.Query_Language.Term as QLTerm
import qualified Database.RethinkDB.Internal.Query_Language.Builtin as QLBuiltin

import qualified Prelude as P
import Prelude (Bool(..), ($), Maybe(..), (++), Int, String)

import Data.Maybe
import Data.Functor
import Data.Aeson
import qualified Data.Sequence as Seq

class CanConcat (a :: ValueTypeKind)
instance CanConcat StringType
instance CanConcat ArrayType

(++) :: (HaveValueType a b v, CanConcat v) => a -> b -> Expr (ValueType v)
(++) = binOp $ defaultValue { QLBuiltin.type' = QL.ADD}

(+) :: (HaveValueType a b NumberType) => a -> b -> Expr (ValueType v)
(+) = plus

(-) :: HaveValueType a b NumberType => a -> b -> NumberExpr
(-) = minus

(*) :: HaveValueType a b NumberType => a -> b -> NumberExpr
(*) = times

(/) :: HaveValueType a b NumberType => a -> b -> NumberExpr
(/) = divide

mod :: HaveValueType a b NumberType => a -> b -> NumberExpr
mod = binOp $ defaultValue { QLBuiltin.type' = QL.MODULO }

or :: HaveValueType a b BoolType => a -> b -> BoolExpr
or = binOp $ defaultValue { QLBuiltin.type' = QL.ANY }

and :: HaveValueType a b BoolType => a -> b -> BoolExpr
and = binOp $ defaultValue { QLBuiltin.type' = QL.ALL }

(==), (!=) :: (HasValueType a x, HasValueType b y) => a -> b -> BoolExpr
(==) = eq
(!=) = neq

(>), (>=), (<), (<=) :: (HaveValueType a b v, CanCompare v) => a -> b -> BoolExpr
(>) = gt
(>=) = ge
(<) = lt
(<=) = le

not :: HasValueType a BoolType => a -> BoolExpr
not = unaryOp $ defaultValue { QLBuiltin.type' = QL.NOT }

-- TODO: wrong?
attr :: HasValueType a StringType => a -> ValueExpr t
attr = unaryOp $ defaultValue { QLBuiltin.type' = QL.IMPLICIT_GETATTR }


(!?) :: (HasValueType a ObjectType) => a -> P.String -> BoolExpr 
(!?) a b = unaryOp (defaultValue { QLBuiltin.type' = QL.HASATTR, QLBuiltin.attr = Just $ uFromString b }) a

map :: (ToExpr e, ExprType e ~ StreamType x from) =>
       (Expr (ValueType from) -> Expr (ValueType to)) -> e -> Expr (StreamType False to)
map = map'

filter :: (ToExpr e, ExprType e ~ StreamType x out) =>
           (Expr (ValueType out) -> Expr (ValueType BoolType)) -> e -> Expr (ExprType e)
filter = filter'

concatMap :: (ToExpr e, ExprType e ~ StreamType x from) =>
             (Expr (ValueType from) -> Expr (ValueType ArrayType)) ->
             e -> Expr (StreamType False to)
concatMap = concatMap'

length :: (ToExpr e, Sequence (ExprType e)) => e -> NumberExpr
length = count

between :: (ToJSON a, ToExpr e, ExprType e ~ StreamType x ObjectType) =>
           (Maybe String) -> (Maybe a) -> (Maybe a) -> e -> Expr (ExprType e)
between k a b e = Expr $ \curdb vars -> defaultValue {
     QLTerm.type' = QL.CALL,
     QLTerm.call = Just $ QL.Call {
       QL.builtin = defaultValue {
          QLBuiltin.type' = QL.RANGE,
          QL.range = Just $ QL.Range (fromMaybe defaultPrimaryAttr $ fmap uFromString k)
                     (fmap toJsonTerm a) (fmap toJsonTerm b)
          },
       QL.args = Seq.singleton $ toTerm (toExpr e) curdb vars
       }
     }



js :: String -> Expr (ValueType any)
js s = Expr $ \_ _ -> defaultValue {
  QLTerm.type' = QL.JAVASCRIPT,
  QLTerm.javascript = Just $ uFromString ("return (" P.++ s P.++ ")")
  }

bind :: (ToValue e) => e -> (Expr (ExprType e) -> Expr t) -> Expr t
bind val f = Expr $ \curdb (v:vs) -> let (v1, v2) = splitVars vs in defaultValue {
  QLTerm.type' = QL.LET,
  QLTerm.let' = Just $ QL.Let (Seq.singleton $
                           QL.VarTermTuple (uFromString v) (toTerm (toValue val) curdb v1))
           (toTerm (toExpr (f $ var v)) curdb v2)
  }

let' :: (ToValue e) => String -> e -> Expr t -> Expr t
let' nam val e = Expr $ \curdb vars -> let (v1, v2) = splitVars vars in defaultValue {
  QLTerm.type' = QL.LET,
  QLTerm.let' = Just $ QL.Let (Seq.singleton $
                           QL.VarTermTuple (uFromString nam) (toTerm (toValue val) curdb v1))
           (toTerm (toExpr e) curdb v2)
  }

var :: String -> Expr t
var v = Expr $ \_ _ -> defaultValue { 
  QLTerm.type' = QL.VAR,
  QLTerm.var = Just $ uFromString v
  }


slice :: (HasValueType a x, HaveValueType b c y) => a -> b -> c -> ValueExpr x
slice = triOp $ defaultValue { QLBuiltin.type' = QL.SLICE }

append :: (HasValueType a ArrayType, HasValueType b x) => a -> b -> ArrayExpr
append = binOp $ defaultValue { QLBuiltin.type' = QL.ARRAYAPPEND }

-- TODO: wrong?
hasattr :: HasValueType a StringType => a -> BoolExpr
hasattr = unaryOp $ defaultValue { QLBuiltin.type' = QL.IMPLICIT_HASATTR }

-- TODO: wrong
pick :: HasValueType a StringType => [a] -> ObjectExpr
pick = opMany $ defaultValue { QLBuiltin.type' = QL.IMPLICIT_PICKATTRS }

pickFrom :: (HasValueType a ObjectType, HasValueType b StringType) => a -> [b] -> ObjectExpr
pickFrom = opOneMany $ defaultValue { QLBuiltin.type' = QL.PICKATTRS }

concatMap' :: (ToExpr e, ExprType e ~ StreamType x from) =>
             (Expr (ValueType from) -> Expr (ValueType ArrayType)) ->
             e -> Expr (StreamType False to)
concatMap' fun = unaryOp' $ \curdb vars -> defaultValue {
  QLBuiltin.type' = QL.CONCATMAP,
  QL.concat_map = Just $ QL.ConcatMap $ toQLMapping fun curdb vars
  }

branch :: (ToExpr e, ExprType e ~ (ValueType BoolType),
           ToExpr a, ExprType a ~ x,
           ToExpr b, ExprType b ~ x) =>
          e -> a -> b -> Expr x
branch t a b = Expr $ \curdb vars -> let (v1:v2:v3:_) = splitsVars vars in defaultValue {
  QLTerm.type' = QL.IF, QL.if_ = Just $ QL.If (toTerm (toExpr t) curdb v1)
                                          (toTerm (toExpr a) curdb v2)
                                          (toTerm (toExpr b) curdb v3) }

jsfun :: ToValue e => String -> e -> Expr (ValueType y)
jsfun f e = Expr $ \curdb (v:vars) -> toTerm (let' v e $ js $ f P.++ "(" P.++ v P.++ ")") curdb vars 

gt, lt, ge, le :: (HaveValueType a b v, CanCompare v) => a -> b -> BoolExpr
gt = comparison QL.GT
lt = comparison QL.LT
ge = comparison QL.GE
le = comparison QL.LE

signum' :: (ToValue e, ExprType e ~ ValueType NumberType) => e -> NumberExpr
signum' x = bind x (\n -> branch (n `lt` (0 :: NumberExpr)) (-1 :: Int)
                          (branch (n `lt` (0 :: NumberExpr)) (1 :: Int) (0 :: Int)))

plus :: (HaveValueType a b NumberType) => a -> b -> Expr (ValueType v)
plus = binOp $ defaultValue { QLBuiltin.type' = QL.ADD}

minus :: HaveValueType a b NumberType => a -> b -> NumberExpr
minus = binOp $ defaultValue { QLBuiltin.type' = QL.SUBTRACT }

times :: HaveValueType a b NumberType => a -> b -> NumberExpr
times = binOp $ defaultValue { QLBuiltin.type' = QL.MULTIPLY }

divide :: HaveValueType a b NumberType => a -> b -> NumberExpr
divide = binOp $ defaultValue { QLBuiltin.type' = QL.DIVIDE }

count :: (ToExpr e, Sequence (ExprType e)) => e -> NumberExpr
count = unaryOp $ defaultValue { QLBuiltin.type' = QL.LENGTH }

map' :: (ToExpr e, ExprType e ~ StreamType x from) =>
       (Expr (ValueType from) -> Expr (ValueType to)) -> e -> Expr (StreamType False to)
map' fun = unaryOp' $ \curdb vars -> defaultValue {
  QLBuiltin.type' = QL.MAP,
  QL.map = Just $ QL.Map $ toQLMapping fun curdb vars
  }

(!) :: (HasValueType a ObjectType) => a -> String -> ValueExpr t
(!) a b = unaryOp (defaultValue {
                      QLBuiltin.type' = QL.GETATTR, QLBuiltin.attr = Just (uFromString b) }) a


filter' :: (ToExpr e, Sequence (ExprType e), SequenceType (ExprType e) ~ t) =>
           (Expr (ValueType t) -> Expr (ValueType BoolType)) -> e -> Expr (StreamType w t)
filter' fil e = Expr $ \curdb vars -> let (v1, v2) = splitVars vars in defaultValue {
     QLTerm.type' = QL.CALL,
     QL.call = Just $ QL.Call {
       QL.builtin = defaultValue {
          QLBuiltin.type' = QL.FILTER,
          QL.filter = Just $ QL.Filter $ mappingToPredicate $ toQLMapping fil curdb v1
          },
       QL.args = Seq.singleton $ toTerm (toExpr e) curdb v2
       }
     }

outerJoin :: (ToExpr a, ExprType a ~ StreamType x l,
              ToExpr b, ExprType b ~ StreamType y r) =>
             a -> b -> (ValueExpr l -> ValueExpr r -> BoolExpr) ->
             Expr (StreamType False ObjectType)
outerJoin self other p =
  P.flip concatMap' self
  (\row -> bind (P.flip concatMap' other
                 (\row2 -> branch (p row row2)
                           [obj ["left" := row, "right" := row2]]
                           nil))
           (\matches ->
             branch (count matches `gt` (0 :: Int)) (asArray matches) [obj ["left" := row]]))

asArray :: (ToExpr e, ExprType e ~ StreamType w x) => e -> ArrayExpr
asArray e = Expr $ \curdb vars -> toTerm (toExpr e) curdb vars

nil :: ValueExpr ArrayType
nil = toExpr ([] :: [()])

eq, neq :: (HasValueType a x, HasValueType b y) => a -> b -> BoolExpr
eq = comparison QL.EQ
neq = comparison QL.NE

{- innerjoin
outerJoin :: (ToStream a, ToStream b) =>
             a -> b -> (ValueExpr l -> ValueExpr r -> BoolExpr) ->
             Expr (StreamType False ObjectType)
outerJoin self other p =
  flip concatMap' self
  (\row -> bind (flip concatMap' other
                 (\row2 -> branch (p row row2)
                           [obj ["left" := row, "right" := row2]]
                           nil))
           (\matches ->
             branch (count matches `gt` (0 :: Int)) (asArray matches) [obj ["left" := row]]))
-}
-- innerJoin
-- eqJoin
-- skip
-- limit
-- trim
-- nth
-- pluck
-- without
-- reduce
-- distinct
-- groupedMapReduce
-- pick
-- unpick
-- merge
-- contains
-- forEach

-- TOOD: batchquery
-- zip :: needs branch map lambda contains merge
-- orderBy
-- union
-- groupBy (count, sum, avg)
-- error
