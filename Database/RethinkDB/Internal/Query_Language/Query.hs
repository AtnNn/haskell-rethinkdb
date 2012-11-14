{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Database.RethinkDB.Internal.Query_Language.Query (Query(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Database.RethinkDB.Internal.Query_Language.MetaQuery as Query_Language (MetaQuery)
import qualified Database.RethinkDB.Internal.Query_Language.Query.QueryType as Query_Language.Query (QueryType)
import qualified Database.RethinkDB.Internal.Query_Language.ReadQuery as Query_Language (ReadQuery)
import qualified Database.RethinkDB.Internal.Query_Language.WriteQuery as Query_Language (WriteQuery)
 
data Query = Query{type' :: !Query_Language.Query.QueryType, token :: !P'.Int64, read_query :: !(P'.Maybe Query_Language.ReadQuery),
                   write_query :: !(P'.Maybe Query_Language.WriteQuery), meta_query :: !(P'.Maybe Query_Language.MetaQuery)}
           deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable Query where
  mergeAppend (Query x'1 x'2 x'3 x'4 x'5) (Query y'1 y'2 y'3 y'4 y'5)
   = Query (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
 
instance P'.Default Query where
  defaultValue = Query P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire Query where
  wireSize ft' self'@(Query x'1 x'2 x'3 x'4 x'5)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeReq 1 14 x'1 + P'.wireSizeReq 1 3 x'2 + P'.wireSizeOpt 1 11 x'3 + P'.wireSizeOpt 1 11 x'4 +
             P'.wireSizeOpt 1 11 x'5)
  wirePut ft' self'@(Query x'1 x'2 x'3 x'4 x'5)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 8 14 x'1
             P'.wirePutReq 16 3 x'2
             P'.wirePutOpt 26 11 x'3
             P'.wirePutOpt 34 11 x'4
             P'.wirePutOpt 42 11 x'5
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{type' = new'Field}) (P'.wireGet 14)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{token = new'Field}) (P'.wireGet 3)
             26 -> Prelude'.fmap
                    (\ !new'Field -> old'Self{read_query = P'.mergeAppend (read_query old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             34 -> Prelude'.fmap
                    (\ !new'Field -> old'Self{write_query = P'.mergeAppend (write_query old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             42 -> Prelude'.fmap
                    (\ !new'Field -> old'Self{meta_query = P'.mergeAppend (meta_query old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> Query) Query where
  getVal m' f' = f' m'
 
instance P'.GPB Query
 
instance P'.ReflectDescriptor Query where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [8, 16]) (P'.fromDistinctAscList [8, 16, 26, 34, 42])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Query_Language.Query\", haskellPrefix = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule = [MName \"Query_Language\"], baseName = MName \"Query\"}, descFilePath = [\"Database\",\"RethinkDB\",\"Internal\",\"Query_Language\",\"Query.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Query_Language.Query.type\", haskellPrefix' = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule' = [MName \"Query_Language\",MName \"Query\"], baseName' = FName \"type'\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".Query_Language.Query.QueryType\", haskellPrefix = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule = [MName \"Query_Language\",MName \"Query\"], baseName = MName \"QueryType\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Query_Language.Query.token\", haskellPrefix' = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule' = [MName \"Query_Language\",MName \"Query\"], baseName' = FName \"token\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Query_Language.Query.read_query\", haskellPrefix' = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule' = [MName \"Query_Language\",MName \"Query\"], baseName' = FName \"read_query\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Query_Language.ReadQuery\", haskellPrefix = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule = [MName \"Query_Language\"], baseName = MName \"ReadQuery\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Query_Language.Query.write_query\", haskellPrefix' = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule' = [MName \"Query_Language\",MName \"Query\"], baseName' = FName \"write_query\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Query_Language.WriteQuery\", haskellPrefix = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule = [MName \"Query_Language\"], baseName = MName \"WriteQuery\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Query_Language.Query.meta_query\", haskellPrefix' = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule' = [MName \"Query_Language\",MName \"Query\"], baseName' = FName \"meta_query\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 42}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Query_Language.MetaQuery\", haskellPrefix = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule = [MName \"Query_Language\"], baseName = MName \"MetaQuery\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"