{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Database.RethinkDB.Protobuf.Ql2.Query (Query(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Database.RethinkDB.Protobuf.Ql2.Query.AssocPair as Ql2.Query (AssocPair)
import qualified Database.RethinkDB.Protobuf.Ql2.Query.QueryType as Ql2.Query (QueryType)
import qualified Database.RethinkDB.Protobuf.Ql2.Term as Ql2 (Term)
 
data Query = Query{type' :: !(P'.Maybe Ql2.Query.QueryType), query :: !(P'.Maybe Ql2.Term), token :: !(P'.Maybe P'.Int64),
                   oBSOLETE_noreply :: !(P'.Maybe P'.Bool), global_optargs :: !(P'.Seq Ql2.Query.AssocPair)}
           deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable Query where
  mergeAppend (Query x'1 x'2 x'3 x'4 x'5) (Query y'1 y'2 y'3 y'4 y'5)
   = Query (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
 
instance P'.Default Query where
  defaultValue = Query P'.defaultValue P'.defaultValue P'.defaultValue (Prelude'.Just Prelude'.False) P'.defaultValue
 
instance P'.Wire Query where
  wireSize ft' self'@(Query x'1 x'2 x'3 x'4 x'5)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 14 x'1 + P'.wireSizeOpt 1 11 x'2 + P'.wireSizeOpt 1 3 x'3 + P'.wireSizeOpt 1 8 x'4 +
             P'.wireSizeRep 1 11 x'5)
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
             P'.wirePutOpt 8 14 x'1
             P'.wirePutOpt 18 11 x'2
             P'.wirePutOpt 24 3 x'3
             P'.wirePutOpt 32 8 x'4
             P'.wirePutRep 50 11 x'5
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{type' = Prelude'.Just new'Field}) (P'.wireGet 14)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{query = P'.mergeAppend (query old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             24 -> Prelude'.fmap (\ !new'Field -> old'Self{token = Prelude'.Just new'Field}) (P'.wireGet 3)
             32 -> Prelude'.fmap (\ !new'Field -> old'Self{oBSOLETE_noreply = Prelude'.Just new'Field}) (P'.wireGet 8)
             50 -> Prelude'.fmap (\ !new'Field -> old'Self{global_optargs = P'.append (global_optargs old'Self) new'Field})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> Query) Query where
  getVal m' f' = f' m'
 
instance P'.GPB Query
 
instance P'.ReflectDescriptor Query where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8, 18, 24, 32, 50])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Ql2.Query\", haskellPrefix = [MName \"Database\",MName \"RethinkDB\",MName \"Protobuf\"], parentModule = [MName \"Ql2\"], baseName = MName \"Query\"}, descFilePath = [\"Database\",\"RethinkDB\",\"Protobuf\",\"Ql2\",\"Query.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Ql2.Query.type\", haskellPrefix' = [MName \"Database\",MName \"RethinkDB\",MName \"Protobuf\"], parentModule' = [MName \"Ql2\",MName \"Query\"], baseName' = FName \"type'\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".Ql2.Query.QueryType\", haskellPrefix = [MName \"Database\",MName \"RethinkDB\",MName \"Protobuf\"], parentModule = [MName \"Ql2\",MName \"Query\"], baseName = MName \"QueryType\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Ql2.Query.query\", haskellPrefix' = [MName \"Database\",MName \"RethinkDB\",MName \"Protobuf\"], parentModule' = [MName \"Ql2\",MName \"Query\"], baseName' = FName \"query\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Ql2.Term\", haskellPrefix = [MName \"Database\",MName \"RethinkDB\",MName \"Protobuf\"], parentModule = [MName \"Ql2\"], baseName = MName \"Term\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Ql2.Query.token\", haskellPrefix' = [MName \"Database\",MName \"RethinkDB\",MName \"Protobuf\"], parentModule' = [MName \"Ql2\",MName \"Query\"], baseName' = FName \"token\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Ql2.Query.OBSOLETE_noreply\", haskellPrefix' = [MName \"Database\",MName \"RethinkDB\",MName \"Protobuf\"], parentModule' = [MName \"Ql2\",MName \"Query\"], baseName' = FName \"oBSOLETE_noreply\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 32}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just \"false\", hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Ql2.Query.global_optargs\", haskellPrefix' = [MName \"Database\",MName \"RethinkDB\",MName \"Protobuf\"], parentModule' = [MName \"Ql2\",MName \"Query\"], baseName' = FName \"global_optargs\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 50}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Ql2.Query.AssocPair\", haskellPrefix = [MName \"Database\",MName \"RethinkDB\",MName \"Protobuf\"], parentModule = [MName \"Ql2\",MName \"Query\"], baseName = MName \"AssocPair\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"