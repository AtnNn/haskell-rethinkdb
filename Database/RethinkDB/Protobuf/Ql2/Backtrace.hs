{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Database.RethinkDB.Protobuf.Ql2.Backtrace (Backtrace(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Database.RethinkDB.Protobuf.Ql2.Frame as Ql2 (Frame)
 
data Backtrace = Backtrace{frames :: !(P'.Seq Ql2.Frame)}
               deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable Backtrace where
  mergeAppend (Backtrace x'1) (Backtrace y'1) = Backtrace (P'.mergeAppend x'1 y'1)
 
instance P'.Default Backtrace where
  defaultValue = Backtrace P'.defaultValue
 
instance P'.Wire Backtrace where
  wireSize ft' self'@(Backtrace x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeRep 1 11 x'1)
  wirePut ft' self'@(Backtrace x'1)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutRep 10 11 x'1
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{frames = P'.append (frames old'Self) new'Field}) (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> Backtrace) Backtrace where
  getVal m' f' = f' m'
 
instance P'.GPB Backtrace
 
instance P'.ReflectDescriptor Backtrace where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Ql2.Backtrace\", haskellPrefix = [MName \"Database\",MName \"RethinkDB\",MName \"Protobuf\"], parentModule = [MName \"Ql2\"], baseName = MName \"Backtrace\"}, descFilePath = [\"Database\",\"RethinkDB\",\"Protobuf\",\"Ql2\",\"Backtrace.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Ql2.Backtrace.frames\", haskellPrefix' = [MName \"Database\",MName \"RethinkDB\",MName \"Protobuf\"], parentModule' = [MName \"Ql2\",MName \"Backtrace\"], baseName' = FName \"frames\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Ql2.Frame\", haskellPrefix = [MName \"Database\",MName \"RethinkDB\",MName \"Protobuf\"], parentModule = [MName \"Ql2\"], baseName = MName \"Frame\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"