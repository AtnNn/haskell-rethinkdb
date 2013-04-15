module Database.RethinkDB.Functions where
import Database.RethinkDB.Term
map :: (Expr a) => (Term -> Term) -> a -> Term
sum :: (Expr s) => s -> Term
