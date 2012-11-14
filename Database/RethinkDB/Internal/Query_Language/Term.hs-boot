{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Database.RethinkDB.Internal.Query_Language.Term (Term) where
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data Term
 
instance P'.MessageAPI msg' (msg' -> Term) Term
 
instance Prelude'.Show Term
 
instance Prelude'.Eq Term
 
instance Prelude'.Ord Term
 
instance Prelude'.Typeable Term
 
instance Prelude'.Data Term
 
instance P'.Mergeable Term
 
instance P'.Default Term
 
instance P'.Wire Term
 
instance P'.GPB Term
 
instance P'.ReflectDescriptor Term
 
instance P'.ExtendMessage Term