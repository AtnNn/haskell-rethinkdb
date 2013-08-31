{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Database.RethinkDB.Protobuf.Ql2.Term.AssocPair (AssocPair) where
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data AssocPair
 
instance P'.MessageAPI msg' (msg' -> AssocPair) AssocPair
 
instance Prelude'.Show AssocPair
 
instance Prelude'.Eq AssocPair
 
instance Prelude'.Ord AssocPair
 
instance Prelude'.Typeable AssocPair
 
instance Prelude'.Data AssocPair
 
instance P'.Mergeable AssocPair
 
instance P'.Default AssocPair
 
instance P'.Wire AssocPair
 
instance P'.GPB AssocPair
 
instance P'.ReflectDescriptor AssocPair