{-# LANGUAGE TypeFamilies, ConstraintKinds, DataKinds #-}
module Database.RethinkDB.Driver where
import Database.RethinkDB.Types
class ToExpr a
class ToValue e
type family ExprType a :: ExprTypeKind
type HasValueType a v = (ToExpr a, ExprType a ~ ValueType v)
type HaveValueType a b v = (HasValueType a v, HasValueType b v)
data Expr (a :: ExprTypeKind)
type NumberExpr = Expr (ValueType NumberType)