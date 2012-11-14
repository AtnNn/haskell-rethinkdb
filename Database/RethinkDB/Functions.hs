{-# LANGUAGE DataKinds, TypeFamilies, ConstraintKinds, FlexibleInstances #-}

-- | RethinkDB funtions that clash with the Prelude.
-- 
-- To be imported qualified
module Database.RethinkDB.Operators where

import Text.ProtocolBuffers.Basic hiding (Default)

import Database.RethinkDB

import qualified Database.RethinkDB.Internal.Types as QL
import qualified Database.RethinkDB.Internal.Query_Language.Term as QLTerm
import qualified Database.RethinkDB.Internal.Query_Language.Builtin as QL (type', attr)

import qualified Prelude as P
import Prelude (Bool(..), ($), Maybe(..), (++), Int)

import qualified Data.Sequence as Seq

class CanAdd (a :: ValueTypeKind)
instance CanAdd NumberType
instance CanAdd StringType
instance CanAdd ArrayType

(+), plus :: (HaveValueType a b v, CanAdd v) => a -> b -> Expr (ValueType v)
(+) = binOp $ defaultValue { QL.type' = QL.ADD}
plus = (+)

(-), minus :: HaveValueType a b NumberType => a -> b -> NumberExpr
(-) = binOp $ defaultValue { QL.type' = QL.SUBTRACT }
minus = (-)

(*), times :: HaveValueType a b NumberType => a -> b -> NumberExpr
(*) = binOp $ defaultValue { QL.type' = QL.MULTIPLY }
times = (*)

(/), divide :: HaveValueType a b NumberType => a -> b -> NumberExpr
(/) = binOp $ defaultValue { QL.type' = QL.DIVIDE }
divide = (/)

mod :: HaveValueType a b NumberType => a -> b -> NumberExpr
mod = binOp $ defaultValue { QL.type' = QL.MODULO }

instance P.Num (Expr (ValueType NumberType)) where
  (+) = plus
  (-) = minus
  (*) = times
  abs = jsfun "abs"
  signum x = bind x (\n -> branch (n < (0 :: Int)) (-1 :: Int) (branch (n > (0 :: Int)) (1 :: Int) (0 :: Int)))
  fromInteger = toExpr

instance P.Fractional (Expr (ValueType NumberType)) where
  fromRational n = toExpr (P.fromRational n :: Double)
  (/) = divide

or :: HaveValueType a b BoolType => a -> b -> BoolExpr
or = binOp $ defaultValue { QL.type' = QL.ANY }

and :: HaveValueType a b BoolType => a -> b -> BoolExpr
and = binOp $ defaultValue { QL.type' = QL.ALL }

(==), (!=) :: (HasValueType a x, HasValueType b y) => a -> b -> BoolExpr
(==) = comparison QL.EQ
(!=) = comparison QL.NE

class CanCompare (a :: ValueTypeKind)
instance CanCompare NumberType
instance CanCompare StringType

(>), (>=), (<), (<=) :: (HaveValueType a b v, CanCompare v) => a -> b -> BoolExpr
(>) = comparison QL.GT
(>=) = comparison QL.GE
(<) = comparison QL.LT
(<=) = comparison QL.LE

not :: HasValueType a BoolType => a -> BoolExpr
not = unaryOp $ defaultValue { QL.type' = QL.NOT }

(!) :: (HasValueType a ObjectType) =>
       a -> P.String -> ValueExpr t
(!) a b = unaryOp (defaultValue { QL.type' = QL.GETATTR, QL.attr = Just (uFromString b) }) a

-- TODO: wrong?
attr :: HasValueType a StringType => a -> ValueExpr t
attr = unaryOp $ defaultValue { QL.type' = QL.IMPLICIT_GETATTR }


(!?) :: (HasValueType a ObjectType) => a -> P.String -> BoolExpr 
(!?) a b = unaryOp (defaultValue { QL.type' = QL.HASATTR, QL.attr = Just $ uFromString b }) a

map :: (ToExpr e, ExprType e ~ StreamType x from) =>
       (Expr (ValueType from) -> Expr (ValueType to)) -> e -> Expr (StreamType False to)
map fun = unaryOp' $ \curdb vars -> defaultValue {
  QL.type' = QL.MAP,
  QL.map = Just $ QL.Map $ toQLMapping fun curdb vars
  }

-- filter is implemented the long way because it was implemented first
filter :: (ToExpr e, ExprType e ~ StreamType x out) =>
           (Expr (ValueType out) -> Expr (ValueType BoolType)) -> e -> Expr (ExprType e)
filter fil e = Expr $ \curdb vars -> let (v1, v2) = splitVars vars in defaultValue {
     QLTerm.type' = QL.CALL,
     QL.call = Just $ QL.Call {
       QL.builtin = defaultValue {
          QL.type' = QL.FILTER,
          QL.filter = Just $ QL.Filter $ mappingToPredicate $ toQLMapping fil curdb v1
          },
       QL.args = Seq.singleton $ toTerm (toExpr e) curdb v2
       }
     }

-- zip :: needs branch map lambda contains merge
-- map
-- concatMap
-- orderBy
-- union
-- groupBy (count, sum, avg)
-- error
