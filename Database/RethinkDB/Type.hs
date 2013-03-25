{-# LANGUAGE DataKinds, KindSignatures, MultiParamTypeClasses,
             FlexibleInstances, TypeOperators, FlexibleContexts,
             PolyKinds, TypeFamilies #-}

-- | The RQL types
module Database.RethinkDB.Type where

import Prelude (Bool(..), Double, String)
import Database.RethinkDB.Objects (Datum, Table, Database)
import {-# SOURCE #-} Database.RethinkDB.Term (Object, Array)
import qualified Prelude as P

data Top
data Null
data SingleSelection
data Sequence
data Stream
data StreamSelection
data Function (a :: [*]) b

-- | The RQL type hierarchy
class Cast from to
instance Cast Top all
instance Cast Datum Datum
instance Cast Datum Null
instance Cast Datum Bool
instance Cast Datum Double
instance Cast Datum String
instance Cast Datum Object
instance Cast Datum SingleSelection
instance Cast Datum Array
instance Cast Null Null
instance Cast Bool Bool
instance Cast Double Double
instance Cast String String
instance Cast Object Object
instance Cast Object SingleSelection
instance Cast SingleSelection SingleSelection
instance Cast Array Array
instance Cast Sequence Sequence
instance Cast Sequence Array
instance Cast Sequence Stream
instance Cast Sequence StreamSelection
instance Cast Sequence Table
instance Cast Stream Stream
instance Cast Stream StreamSelection
instance Cast Stream Table
instance Cast StreamSelection StreamSelection
instance Cast StreamSelection Table
instance Cast Table Table
instance Cast Database Database

instance Cast ret ret2
         => Cast (Function '[] ret) (Function '[] ret2)
instance (Cast x2 x,
          Cast (Function xs ret) (Function xs2 ret2))
         => Cast (Function (x ': xs) ret) (Function (x2 ': xs2) ret2)

-- Also accept unknown datums
instance Cast Null Datum
instance Cast Bool Datum
instance Cast Double Datum
instance Cast String Datum
instance Cast Object Datum
instance Cast SingleSelection Datum
instance Cast Array Datum
instance Cast Sequence Datum

type CommonType a b = Top

type family Selection x :: P.Bool
type instance Selection Table = True
type instance Selection SingleSelection = True
type instance Selection StreamSelection = True
type instance Selection Top = False
type instance Selection Datum = False
type instance Selection Null = False
type instance Selection Bool = False
type instance Selection Double = False
type instance Selection String = False
type instance Selection Object = False
type instance Selection Array = False
type instance Selection Sequence = False
type instance Selection Stream = False
type instance Selection Database = False
type instance Selection (Function a b) = False
