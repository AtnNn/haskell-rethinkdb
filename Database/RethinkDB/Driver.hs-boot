{-# LANGUAGE TypeFamilies, ConstraintKinds, DataKinds #-}
module Database.RethinkDB.Driver where
import Database.RethinkDB.Types
class ToExpr a
class ToValue e
type family ExprType a :: ExprTypeKind
type family ToValueType (t :: ExprTypeKind) :: ValueTypeKind
type HasValueType a v = (ToValue a, ToValueType (ExprType a) ~ v)
type HaveValueType a b v = (HasValueType a v, HasValueType b v)
data Expr (a :: ExprTypeKind)
type NumberExpr = Expr (ValueType NumberType)
type family ExprTypeIsView (expr :: ExprTypeKind) :: Bool
type ExprIsView e = ExprTypeIsView (ExprType e)
