{-# LANGUAGE ConstraintKinds, DataKinds, TypeFamilies #-}
module Database.RethinkDB.Functions where
import Database.RethinkDB.Types
import {-# SOURCE #-} Database.RethinkDB.Driver
plus, minus, times, divide
  :: (HaveValueType a b NumberType) => a -> b -> NumberExpr
jsfun :: ToValue e => String -> e -> Expr (ValueType y)
var :: ExprIsView (Expr t) ~ False => String -> Expr t
signum' :: (ToValue e, ToValueType (ExprType e) ~ NumberType) => e -> NumberExpr