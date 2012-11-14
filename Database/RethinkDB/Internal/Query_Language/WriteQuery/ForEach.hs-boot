{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Database.RethinkDB.Internal.Query_Language.WriteQuery.ForEach (ForEach) where
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data ForEach
 
instance P'.MessageAPI msg' (msg' -> ForEach) ForEach
 
instance Prelude'.Show ForEach
 
instance Prelude'.Eq ForEach
 
instance Prelude'.Ord ForEach
 
instance Prelude'.Typeable ForEach
 
instance Prelude'.Data ForEach
 
instance P'.Mergeable ForEach
 
instance P'.Default ForEach
 
instance P'.Wire ForEach
 
instance P'.GPB ForEach
 
instance P'.ReflectDescriptor ForEach