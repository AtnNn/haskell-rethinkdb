module Database.RethinkDB.MapReduce where

import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Text as T

import Database.RethinkDB.Protobuf.Ql2.Term.TermType

import Database.RethinkDB.ReQL
import Database.RethinkDB.Objects
import {-# SOURCE #-} qualified Database.RethinkDB.Functions as R

termToMapReduce :: (ReQL -> ReQL) -> State QuerySettings (ReQL, ReQL, ReQL)
termToMapReduce = undefined

toReduce :: MapReduce -> MapReduce
toReduce (Map x) = MapReduce x idt []
toReduce mr = mr

idt :: ReQL
idt = ReQL $ do
        v <- newVar
        baseReQL $ op FUNC ([v], v) ()

sameVar :: BaseReQL -> BaseArray -> Bool
sameVar (BaseReQL DATUM (Just x) _ _) [BaseReQL DATUM (Just y) _ _] = x == y
sameVar _ _ = False

notNone :: MapReduce -> Bool
notNone None{} = False
notNone _ = True

wrap :: BaseReQL -> ReQL
wrap = ReQL . return

toMapReduce :: BaseReQL -> BaseReQL -> MapReduce
toMapReduce _ t@(BaseReQL DATUM _ _ _) = None $ wrap t
toMapReduce v   (BaseReQL VAR _ w _) | sameVar v w = Map []
toMapReduce v t@(BaseReQL type' _ args optargs) = let
    args' = map (toMapReduce v) args
    optargs' = map (\(BaseAttribute k vv) -> (k, toMapReduce v vv)) optargs
    count = length $ filter notNone $ args' ++ map snd optargs'
    rebuild = (if count == 1 then rebuild0 else rebuildx) type' args' optargs'
  in if count == 0 then None $ wrap t
     else if not $ count == 1
          then rebuild else
              case (type', args') of
                (MAP, [Map m, None f]) -> Map (f : m)
                (REDUCE, [Map m, None f]) -> MapReduce m f []
                (COUNT, [Map _]) -> MapReduce [expr (const $ 1 :: ReQL -> ReQL)]
                                    (expr (R.sum :: ReQL -> ReQL)) []
                _ -> rebuild

data MapReduce =
    None ReQL |
    Map [ReQL] |
    MapReduce [ReQL] ReQL [ReQL]

-- (TERMTYPE a (mapreduce maps reduce finals)) -> mapreduce maps reduce ((\x -> TERMTYPE a x) : finals)

rebuild0 :: TermType -> [MapReduce] -> [(T.Text, MapReduce)] -> MapReduce
rebuild0 ttype args optargs = MapReduce maps reduce finals where
  ([(MapReduce maps reduce tailFinals)], headFinals) = extract False ttype args optargs
  finals = headFinals : tailFinals

rebuildx :: TermType -> [MapReduce] -> [(Key, MapReduce)] -> MapReduce
rebuildx ttype args optargs = MapReduce maps reduce finals where
  (mrs, headFinals) = extract True ttype args optargs
  maps = undefined mrs
  reduce = undefined mrs
  finals = undefined mrs

extract :: Bool -> TermType -> [MapReduce] -> [(Key, MapReduce)] -> ([MapReduce], ReQL)
extract = undefined

-- extractList :: [MapReduce] -> WriterT (State ) [MapReduce] ([ReQL] -> ReQL)
extractList = undefined