{-# LANGUAGE ConstraintKinds, DataKinds, TypeFamilies #-}
module Database.RethinkDB.Functions where
import Database.RethinkDB.Types
import {-# SOURCE #-} Database.RethinkDB.Driver
plus, minus, times, divide
  :: (HaveValueType a b NumberType) => a -> b -> NumberExpr
jsfun :: ToValue e => String -> e -> Expr (ValueType y)
var :: String -> Expr t
signum' :: (ToValue e, ExprType e ~ ValueType NumberType) => e -> NumberExpr