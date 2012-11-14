{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Database.RethinkDB.Internal.Query_Language.WriteQuery.PointMutate (PointMutate(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Database.RethinkDB.Internal.Query_Language.Mapping as Query_Language (Mapping)
import qualified Database.RethinkDB.Internal.Query_Language.TableRef as Query_Language (TableRef)
import qualified Database.RethinkDB.Internal.Query_Language.Term as Query_Language (Term)
 
data PointMutate = PointMutate{table_ref :: !Query_Language.TableRef, attrname :: !P'.Utf8, key :: !Query_Language.Term,
                               mapping :: !Query_Language.Mapping}
                 deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable PointMutate where
  mergeAppend (PointMutate x'1 x'2 x'3 x'4) (PointMutate y'1 y'2 y'3 y'4)
   = PointMutate (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
 
instance P'.Default PointMutate where
  defaultValue = PointMutate P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire PointMutate where
  wireSize ft' self'@(PointMutate x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 11 x'1 + P'.wireSizeReq 1 9 x'2 + P'.wireSizeReq 1 11 x'3 + P'.wireSizeReq 1 11 x'4)
  wirePut ft' self'@(PointMutate x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 10 11 x'1
             P'.wirePutReq 18 9 x'2
             P'.wirePutReq 26 11 x'3
             P'.wirePutReq 34 11 x'4
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{table_ref = P'.mergeAppend (table_ref old'Self) (new'Field)})
                    (P'.wireGet 11)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{attrname = new'Field}) (P'.wireGet 9)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{key = P'.mergeAppend (key old'Self) (new'Field)}) (P'.wireGet 11)
             34 -> Prelude'.fmap (\ !new'Field -> old'Self{mapping = P'.mergeAppend (mapping old'Self) (new'Field)}) (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> PointMutate) PointMutate where
  getVal m' f' = f' m'
 
instance P'.GPB PointMutate
 
instance P'.ReflectDescriptor PointMutate where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [10, 18, 26, 34]) (P'.fromDistinctAscList [10, 18, 26, 34])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Query_Language.WriteQuery.PointMutate\", haskellPrefix = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule = [MName \"Query_Language\",MName \"WriteQuery\"], baseName = MName \"PointMutate\"}, descFilePath = [\"Database\",\"RethinkDB\",\"Internal\",\"Query_Language\",\"WriteQuery\",\"PointMutate.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Query_Language.WriteQuery.PointMutate.table_ref\", haskellPrefix' = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule' = [MName \"Query_Language\",MName \"WriteQuery\",MName \"PointMutate\"], baseName' = FName \"table_ref\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Query_Language.TableRef\", haskellPrefix = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule = [MName \"Query_Language\"], baseName = MName \"TableRef\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Query_Language.WriteQuery.PointMutate.attrname\", haskellPrefix' = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule' = [MName \"Query_Language\",MName \"WriteQuery\",MName \"PointMutate\"], baseName' = FName \"attrname\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Query_Language.WriteQuery.PointMutate.key\", haskellPrefix' = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule' = [MName \"Query_Language\",MName \"WriteQuery\",MName \"PointMutate\"], baseName' = FName \"key\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Query_Language.Term\", haskellPrefix = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule = [MName \"Query_Language\"], baseName = MName \"Term\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Query_Language.WriteQuery.PointMutate.mapping\", haskellPrefix' = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule' = [MName \"Query_Language\",MName \"WriteQuery\",MName \"PointMutate\"], baseName' = FName \"mapping\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Query_Language.Mapping\", haskellPrefix = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule = [MName \"Query_Language\"], baseName = MName \"Mapping\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"