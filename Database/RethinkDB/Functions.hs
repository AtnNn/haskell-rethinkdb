{-# LANGUAGE DataKinds, ConstraintKinds, TypeFamilies #-}

module Database.RethinkDB.Operators where

import Text.ProtocolBuffers.Basic hiding (Default)

import Database.RethinkDB

import qualified Database.RethinkDB.Internal.Types as QL
import qualified Database.RethinkDB.Internal.Query_Language.Builtin as QL (type') 

import qualified Prelude as P
import Prelude (Bool(..), ($))

type HasValueType a v = (ToExpr a, ExprType a ~ ValueType v)
type HaveValueType a b v = (HasValueType a v, HasValueType b v)
type NumberExpr = Expr False (ValueType NumberType)
type BoolExpr = Expr False (ValueType BoolType)
type ValueExpr t = Expr False (ValueType t)

class CanAdd (a :: ValueTypeKind)
instance CanAdd NumberType
instance CanAdd StringType
instance CanAdd ArrayType

(+) :: (HaveValueType a b v, CanAdd v) => a -> b -> Expr False (ValueType v)
(+) = binOp $ defaultValue { QL.type' = QL.ADD}

(-) :: HaveValueType a b NumberType => a -> b -> NumberExpr
(-) = binOp $ defaultValue { QL.type' = QL.SUBTRACT }

(*) :: HaveValueType a b NumberType => a -> b -> NumberExpr
(*) = binOp $ defaultValue { QL.type' = QL.MULTIPLY }

(/) :: HaveValueType a b NumberType => a -> b -> NumberExpr
(/) = binOp $ defaultValue { QL.type' = QL.DIVIDE }

mod :: HaveValueType a b NumberType => a -> b -> NumberExpr
mod = binOp $ defaultValue { QL.type' = QL.MODULO }

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

(!) :: (HasValueType a ObjectType, HasValueType b StringType) =>
       a -> b -> ValueExpr t
(!) = binOp $ defaultValue { QL.type' = QL.GETATTR }

attr :: HasValueType a StringType => a -> ValueExpr t
attr = unaryOp $ defaultValue { QL.type' = QL.IMPLICIT_GETATTR }

(!?) :: (HasValueType a ObjectType, HasValueType b StringType) => a -> b -> BoolExpr 
(!?) = binOp $ defaultValue { QL.type' = QL.HASATTR }

hasattr :: HasValueType a StringType => a -> BoolExpr
hasattr = unaryOp $ defaultValue { QL.type' = QL.IMPLICIT_HASATTR }