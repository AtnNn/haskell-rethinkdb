module Database.RethinkDB.Functions where
import Database.RethinkDB.ReQL
map :: (Expr a, Expr b) => (ReQL -> b) -> a -> ReQL
sum :: (Expr s) => s -> ReQL

