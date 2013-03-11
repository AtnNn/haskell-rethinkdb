{-# LANGUAGE UndecidableInstances, DataKinds, MultiParamTypeClasses, 
             KindSignatures, FlexibleInstances#-}

module Database.RethinkDB.Term.Undecidable where

import Database.RethinkDB.Type

class DatumOrInstance (a :: Type) (b :: Type)
instance DatumOrInstance x Datum
instance Instance a b => DatumOrInstance a b