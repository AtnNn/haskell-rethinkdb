{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Database.RethinkDB.Internal.Query_Language.TableRef (TableRef(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data TableRef = TableRef{db_name :: !P'.Utf8, table_name :: !P'.Utf8, use_outdated :: !(P'.Maybe P'.Bool)}
              deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable TableRef where
  mergeAppend (TableRef x'1 x'2 x'3) (TableRef y'1 y'2 y'3)
   = TableRef (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3)
 
instance P'.Default TableRef where
  defaultValue = TableRef P'.defaultValue P'.defaultValue (Prelude'.Just Prelude'.False)
 
instance P'.Wire TableRef where
  wireSize ft' self'@(TableRef x'1 x'2 x'3)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 9 x'1 + P'.wireSizeReq 1 9 x'2 + P'.wireSizeOpt 1 8 x'3)
  wirePut ft' self'@(TableRef x'1 x'2 x'3)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 10 9 x'1
             P'.wirePutReq 18 9 x'2
             P'.wirePutOpt 24 8 x'3
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{db_name = new'Field}) (P'.wireGet 9)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{table_name = new'Field}) (P'.wireGet 9)
             24 -> Prelude'.fmap (\ !new'Field -> old'Self{use_outdated = Prelude'.Just new'Field}) (P'.wireGet 8)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> TableRef) TableRef where
  getVal m' f' = f' m'
 
instance P'.GPB TableRef
 
instance P'.ReflectDescriptor TableRef where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [10, 18]) (P'.fromDistinctAscList [10, 18, 24])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Query_Language.TableRef\", haskellPrefix = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule = [MName \"Query_Language\"], baseName = MName \"TableRef\"}, descFilePath = [\"Database\",\"RethinkDB\",\"Internal\",\"Query_Language\",\"TableRef.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Query_Language.TableRef.db_name\", haskellPrefix' = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule' = [MName \"Query_Language\",MName \"TableRef\"], baseName' = FName \"db_name\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Query_Language.TableRef.table_name\", haskellPrefix' = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule' = [MName \"Query_Language\",MName \"TableRef\"], baseName' = FName \"table_name\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Query_Language.TableRef.use_outdated\", haskellPrefix' = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule' = [MName \"Query_Language\",MName \"TableRef\"], baseName' = FName \"use_outdated\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just \"false\", hsDefault = Just (HsDef'Bool False)}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"