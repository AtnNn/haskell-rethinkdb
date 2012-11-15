{-# LANGUAGE DataKinds, TypeFamilies, ConstraintKinds, FlexibleInstances, 
             FlexibleContexts #-}

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

class CanConcat (a :: ValueTypeKind)
instance CanConcat StringType
instance CanConcat ArrayType

(++) :: (HaveValueType a b v, CanConcat v) => a -> b -> Expr (ValueType v)
(++) = binOp $ defaultValue { QL.type' = QL.ADD}

(+) :: (HaveValueType a b NumberType) => a -> b -> Expr (ValueType v)
(+) = plus

(-) :: HaveValueType a b NumberType => a -> b -> NumberExpr
(-) = minus

(*) :: HaveValueType a b NumberType => a -> b -> NumberExpr
(*) = times

(/) :: HaveValueType a b NumberType => a -> b -> NumberExpr
(/) = divide

mod :: HaveValueType a b NumberType => a -> b -> NumberExpr
mod = binOp $ defaultValue { QL.type' = QL.MODULO }

or :: HaveValueType a b BoolType => a -> b -> BoolExpr
or = binOp $ defaultValue { QL.type' = QL.ANY }

and :: HaveValueType a b BoolType => a -> b -> BoolExpr
and = binOp $ defaultValue { QL.type' = QL.ALL }

(==), (!=) :: (HasValueType a x, HasValueType b y) => a -> b -> BoolExpr
(==) = eq
(!=) = neq

(>), (>=), (<), (<=) :: (HaveValueType a b v, CanCompare v) => a -> b -> BoolExpr
(>) = gt
(>=) = ge
(<) = lt
(<=) = le

not :: HasValueType a BoolType => a -> BoolExpr
not = unaryOp $ defaultValue { QL.type' = QL.NOT }

-- TODO: wrong?
attr :: HasValueType a StringType => a -> ValueExpr t
attr = unaryOp $ defaultValue { QL.type' = QL.IMPLICIT_GETATTR }


(!?) :: (HasValueType a ObjectType) => a -> P.String -> BoolExpr 
(!?) a b = unaryOp (defaultValue { QL.type' = QL.HASATTR, QL.attr = Just $ uFromString b }) a

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

length :: (ToExpr e, StreamOrArray (ExprType e)) => e -> NumberExpr
length = count

-- zip :: needs branch map lambda contains merge
-- orderBy
-- union
-- groupBy (count, sum, avg)
-- error
