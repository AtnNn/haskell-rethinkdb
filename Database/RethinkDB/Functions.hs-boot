{-# LANGUAGE ConstraintKinds, DataKinds, TypeFamilies #-}
module Database.RethinkDB.Functions where
import Database.RethinkDB.Types
import {-# SOURCE #-} Database.RethinkDB.Driver
plus :: (HaveValueType a b NumberType) => a -> b -> Expr (ValueType v)
minus :: HaveValueType a b NumberType => a -> b -> NumberExpr
times :: HaveValueType a b NumberType => a -> b -> NumberExpr
divide :: HaveValueType a b NumberType => a -> b -> NumberExpr
jsfun :: ToValue e => String -> e -> Expr (ValueType y)
var :: String -> Expr t
signum' :: (ToValue e, ExprType e ~ ValueType NumberType) => e -> NumberExpr