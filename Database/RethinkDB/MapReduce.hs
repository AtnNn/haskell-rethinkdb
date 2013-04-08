module Database.RethinkDB.MapReduce where

import Control.Monad.State

import Database.RethinkDB.Protobuf.Ql2.Term2.TermType

import Database.RethinkDB.Term

termToMapReduce :: (Term -> Term) -> State QuerySettings (Term, Term, Term)
termToMapReduce = undefined

data MapReduce =
    None Term |
    Map [Term] |
    MapReduce [Term] Term |
    MapReduceFinal [Term] Term [Term]

toReduce :: MapReduce -> MapReduce
toReduce (Map x) = MapReduce x idt
toReduce mr = mr

idt :: Term
idt = Term $ do
        v <- newVar
        baseTerm $ op FUNC ([v], v) ()

sameVar = undefined

notNone :: MapReduce -> Bool
notNone None{} = False
notNone _ = True

wrap :: BaseTerm -> Term
wrap = Term . return

toMapReduce :: BaseTerm -> BaseTerm -> MapReduce
toMapReduce _ t@(BaseTerm DATUM _ _ _) = None $ wrap t
toMapReduce v t@(BaseTerm VAR _ w _) | sameVar v w = Map []
toMapReduce v t@(BaseTerm type' _ args optargs) = let
    args' = map (toMapReduce v) args
    optargs' = map (\(BaseAttribute k vv) -> (k, toMapReduce v vv)) optargs
    count = length $ filter notNone $ args' ++ map snd optargs'
    rebuild = (if count == 1 then rebuild0 else rebuildx) type' args' optargs'
  in if count == 0 then None $ wrap t else
         case (type', args') of
           _ -> undefined

rebuild0 = undefined

rebuildx = undefined
