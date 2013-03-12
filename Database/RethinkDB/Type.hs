{-# LANGUAGE DataKinds, KindSignatures, MultiParamTypeClasses,
             FlexibleInstances, TypeOperators, FlexibleContexts #-}

-- | The RQL types
module Database.RethinkDB.Type where

import Prelude ()
import qualified Prelude as P

-- | An RQL type
data Type = Top | Datum | Null | Bool | Number | String | Object |
            SingleSelection | Array | Sequence | Stream |
            StreamSelection | Table | Database |
            Function [Type] Type

-- | The RQL type hierarchy
class Instance (parent :: Type) (child :: Type)
instance Instance Top all
instance Instance Datum Datum
instance Instance Datum Null
instance Instance Datum Bool
instance Instance Datum Number
instance Instance Datum String
instance Instance Datum Object
instance Instance Datum SingleSelection
instance Instance Datum Array
instance Instance Null Null
instance Instance Bool Bool
instance Instance Number Number
instance Instance String String
instance Instance Object Object
instance Instance Object SingleSelection
instance Instance SingleSelection SingleSelection
instance Instance Array Array
instance Instance Sequence Sequence
instance Instance Sequence Array
instance Instance Sequence Stream
instance Instance Sequence StreamSelection
instance Instance Sequence Table
instance Instance Stream Stream
instance Instance Stream StreamSelection
instance Instance Stream Table
instance Instance StreamSelection StreamSelection
instance Instance StreamSelection Table
instance Instance Table Table
instance Instance Database Database

instance Instance ret ret2
         => Instance (Function '[] ret) (Function '[] ret2)
instance (Instance x2 x,
          Instance (Function xs ret) (Function xs2 ret2))
         => Instance (Function (x ': xs) ret) (Function (x2 ': xs2) ret2)

-- Also accept unknown datums
instance Instance Null Datum
instance Instance Bool Datum
instance Instance Number Datum
instance Instance String Datum
instance Instance Object Datum
instance Instance SingleSelection Datum
instance Instance Array Datum
instance Instance Sequence Datum
