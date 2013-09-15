module Database.RethinkDB.Functions where
import Database.RethinkDB.ReQL
map :: (Expr a) => (ReQL -> ReQL) -> a -> ReQL
sum :: (Expr s) => s -> ReQL
