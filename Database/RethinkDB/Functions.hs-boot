module Database.RethinkDB.Functions where
import {-# SOURCE #-} Database.RethinkDB.ReQL
js :: ReQL -> ReQL
apply :: (Expr fun, Expr arg) => fun -> [arg] -> ReQL