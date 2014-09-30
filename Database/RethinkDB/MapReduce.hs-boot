module Database.RethinkDB.MapReduce (termToMapReduce) where
import Control.Monad.State
import Database.RethinkDB.ReQL
termToMapReduce :: (ReQL -> ReQL) -> State QuerySettings (ReQL -> ReQL)