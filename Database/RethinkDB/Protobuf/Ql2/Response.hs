{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Database.RethinkDB.Protobuf.Ql2.Response (Response(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Database.RethinkDB.Protobuf.Ql2.Backtrace as Ql2 (Backtrace)
import qualified Database.RethinkDB.Protobuf.Ql2.Datum as Ql2 (Datum)
import qualified Database.RethinkDB.Protobuf.Ql2.Response.ResponseType as Ql2.Response (ResponseType)
 
data Response = Response{type' :: !(P'.Maybe Ql2.Response.ResponseType), token :: !(P'.Maybe P'.Int64),
                         response :: !(P'.Seq Ql2.Datum), backtrace :: !(P'.Maybe Ql2.Backtrace)}
              deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable Response where
  mergeAppend (Response x'1 x'2 x'3 x'4) (Response y'1 y'2 y'3 y'4)
   = Response (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
 
instance P'.Default Response where
  defaultValue = Response P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire Response where
  wireSize ft' self'@(Response x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 14 x'1 + P'.wireSizeOpt 1 3 x'2 + P'.wireSizeRep 1 11 x'3 + P'.wireSizeOpt 1 11 x'4)
  wirePut ft' self'@(Response x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 8 14 x'1
             P'.wirePutOpt 16 3 x'2
             P'.wirePutRep 26 11 x'3
             P'.wirePutOpt 34 11 x'4
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{type' = Prelude'.Just new'Field}) (P'.wireGet 14)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{token = Prelude'.Just new'Field}) (P'.wireGet 3)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{response = P'.append (response old'Self) new'Field}) (P'.wireGet 11)
             34 -> Prelude'.fmap
                    (\ !new'Field -> old'Self{backtrace = P'.mergeAppend (backtrace old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> Response) Response where
  getVal m' f' = f' m'
 
instance P'.GPB Response
 
instance P'.ReflectDescriptor Response where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8, 16, 26, 34])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Ql2.Response\", haskellPrefix = [MName \"Database\",MName \"RethinkDB\",MName \"Protobuf\"], parentModule = [MName \"Ql2\"], baseName = MName \"Response\"}, descFilePath = [\"Database\",\"RethinkDB\",\"Protobuf\",\"Ql2\",\"Response.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Ql2.Response.type\", haskellPrefix' = [MName \"Database\",MName \"RethinkDB\",MName \"Protobuf\"], parentModule' = [MName \"Ql2\",MName \"Response\"], baseName' = FName \"type'\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".Ql2.Response.ResponseType\", haskellPrefix = [MName \"Database\",MName \"RethinkDB\",MName \"Protobuf\"], parentModule = [MName \"Ql2\",MName \"Response\"], baseName = MName \"ResponseType\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Ql2.Response.token\", haskellPrefix' = [MName \"Database\",MName \"RethinkDB\",MName \"Protobuf\"], parentModule' = [MName \"Ql2\",MName \"Response\"], baseName' = FName \"token\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Ql2.Response.response\", haskellPrefix' = [MName \"Database\",MName \"RethinkDB\",MName \"Protobuf\"], parentModule' = [MName \"Ql2\",MName \"Response\"], baseName' = FName \"response\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Ql2.Datum\", haskellPrefix = [MName \"Database\",MName \"RethinkDB\",MName \"Protobuf\"], parentModule = [MName \"Ql2\"], baseName = MName \"Datum\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Ql2.Response.backtrace\", haskellPrefix' = [MName \"Database\",MName \"RethinkDB\",MName \"Protobuf\"], parentModule' = [MName \"Ql2\",MName \"Response\"], baseName' = FName \"backtrace\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Ql2.Backtrace\", haskellPrefix = [MName \"Database\",MName \"RethinkDB\",MName \"Protobuf\"], parentModule = [MName \"Ql2\"], baseName = MName \"Backtrace\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"