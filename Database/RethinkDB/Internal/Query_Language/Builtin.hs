{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Database.RethinkDB.Internal.Query_Language.Builtin (Builtin(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Database.RethinkDB.Internal.Query_Language.Builtin.BuiltinType as Query_Language.Builtin (BuiltinType)
import qualified Database.RethinkDB.Internal.Query_Language.Builtin.Comparison as Query_Language.Builtin (Comparison)
import qualified Database.RethinkDB.Internal.Query_Language.Builtin.ConcatMap as Query_Language.Builtin (ConcatMap)
import qualified Database.RethinkDB.Internal.Query_Language.Builtin.Filter as Query_Language.Builtin (Filter)
import qualified Database.RethinkDB.Internal.Query_Language.Builtin.GroupedMapReduce as Query_Language.Builtin (GroupedMapReduce)
import qualified Database.RethinkDB.Internal.Query_Language.Builtin.Map as Query_Language.Builtin (Map)
import qualified Database.RethinkDB.Internal.Query_Language.Builtin.OrderBy as Query_Language.Builtin (OrderBy)
import qualified Database.RethinkDB.Internal.Query_Language.Builtin.Range as Query_Language.Builtin (Range)
import qualified Database.RethinkDB.Internal.Query_Language.Reduction as Query_Language (Reduction)
 
data Builtin = Builtin{type' :: !Query_Language.Builtin.BuiltinType, attr :: !(P'.Maybe P'.Utf8), attrs :: !(P'.Seq P'.Utf8),
                       comparison :: !(P'.Maybe Query_Language.Builtin.Comparison),
                       filter :: !(P'.Maybe Query_Language.Builtin.Filter), map :: !(P'.Maybe Query_Language.Builtin.Map),
                       concat_map :: !(P'.Maybe Query_Language.Builtin.ConcatMap),
                       order_by :: !(P'.Seq Query_Language.Builtin.OrderBy), reduce :: !(P'.Maybe Query_Language.Reduction),
                       grouped_map_reduce :: !(P'.Maybe Query_Language.Builtin.GroupedMapReduce),
                       range :: !(P'.Maybe Query_Language.Builtin.Range)}
             deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable Builtin where
  mergeAppend (Builtin x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11) (Builtin y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8 y'9 y'10 y'11)
   = Builtin (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
      (P'.mergeAppend x'7 y'7)
      (P'.mergeAppend x'8 y'8)
      (P'.mergeAppend x'9 y'9)
      (P'.mergeAppend x'10 y'10)
      (P'.mergeAppend x'11 y'11)
 
instance P'.Default Builtin where
  defaultValue
   = Builtin P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
 
instance P'.Wire Builtin where
  wireSize ft' self'@(Builtin x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeReq 1 14 x'1 + P'.wireSizeOpt 1 9 x'2 + P'.wireSizeRep 1 9 x'3 + P'.wireSizeOpt 1 14 x'4 +
             P'.wireSizeOpt 1 11 x'5
             + P'.wireSizeOpt 1 11 x'6
             + P'.wireSizeOpt 1 11 x'7
             + P'.wireSizeRep 1 11 x'8
             + P'.wireSizeOpt 1 11 x'9
             + P'.wireSizeOpt 1 11 x'10
             + P'.wireSizeOpt 1 11 x'11)
  wirePut ft' self'@(Builtin x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11)
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
             P'.wirePutOpt 18 9 x'2
             P'.wirePutRep 26 9 x'3
             P'.wirePutOpt 32 14 x'4
             P'.wirePutOpt 42 11 x'5
             P'.wirePutOpt 50 11 x'6
             P'.wirePutOpt 58 11 x'7
             P'.wirePutRep 66 11 x'8
             P'.wirePutOpt 74 11 x'9
             P'.wirePutOpt 82 11 x'10
             P'.wirePutOpt 90 11 x'11
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{type' = new'Field}) (P'.wireGet 14)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{attr = Prelude'.Just new'Field}) (P'.wireGet 9)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{attrs = P'.append (attrs old'Self) new'Field}) (P'.wireGet 9)
             32 -> Prelude'.fmap (\ !new'Field -> old'Self{comparison = Prelude'.Just new'Field}) (P'.wireGet 14)
             42 -> Prelude'.fmap (\ !new'Field -> old'Self{filter = P'.mergeAppend (filter old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             50 -> Prelude'.fmap (\ !new'Field -> old'Self{map = P'.mergeAppend (map old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             58 -> Prelude'.fmap
                    (\ !new'Field -> old'Self{concat_map = P'.mergeAppend (concat_map old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             66 -> Prelude'.fmap (\ !new'Field -> old'Self{order_by = P'.append (order_by old'Self) new'Field}) (P'.wireGet 11)
             74 -> Prelude'.fmap (\ !new'Field -> old'Self{reduce = P'.mergeAppend (reduce old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             82 -> Prelude'.fmap
                    (\ !new'Field ->
                      old'Self{grouped_map_reduce = P'.mergeAppend (grouped_map_reduce old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             90 -> Prelude'.fmap (\ !new'Field -> old'Self{range = P'.mergeAppend (range old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> Builtin) Builtin where
  getVal m' f' = f' m'
 
instance P'.GPB Builtin
 
instance P'.ReflectDescriptor Builtin where
  getMessageInfo _
   = P'.GetMessageInfo (P'.fromDistinctAscList [8]) (P'.fromDistinctAscList [8, 18, 26, 32, 42, 50, 58, 66, 74, 82, 90])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Query_Language.Builtin\", haskellPrefix = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule = [MName \"Query_Language\"], baseName = MName \"Builtin\"}, descFilePath = [\"Database\",\"RethinkDB\",\"Internal\",\"Query_Language\",\"Builtin.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Query_Language.Builtin.type\", haskellPrefix' = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule' = [MName \"Query_Language\",MName \"Builtin\"], baseName' = FName \"type'\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".Query_Language.Builtin.BuiltinType\", haskellPrefix = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule = [MName \"Query_Language\",MName \"Builtin\"], baseName = MName \"BuiltinType\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Query_Language.Builtin.attr\", haskellPrefix' = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule' = [MName \"Query_Language\",MName \"Builtin\"], baseName' = FName \"attr\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Query_Language.Builtin.attrs\", haskellPrefix' = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule' = [MName \"Query_Language\",MName \"Builtin\"], baseName' = FName \"attrs\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Query_Language.Builtin.comparison\", haskellPrefix' = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule' = [MName \"Query_Language\",MName \"Builtin\"], baseName' = FName \"comparison\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 32}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".Query_Language.Builtin.Comparison\", haskellPrefix = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule = [MName \"Query_Language\",MName \"Builtin\"], baseName = MName \"Comparison\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Query_Language.Builtin.filter\", haskellPrefix' = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule' = [MName \"Query_Language\",MName \"Builtin\"], baseName' = FName \"filter\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 42}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Query_Language.Builtin.Filter\", haskellPrefix = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule = [MName \"Query_Language\",MName \"Builtin\"], baseName = MName \"Filter\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Query_Language.Builtin.map\", haskellPrefix' = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule' = [MName \"Query_Language\",MName \"Builtin\"], baseName' = FName \"map\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 50}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Query_Language.Builtin.Map\", haskellPrefix = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule = [MName \"Query_Language\",MName \"Builtin\"], baseName = MName \"Map\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Query_Language.Builtin.concat_map\", haskellPrefix' = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule' = [MName \"Query_Language\",MName \"Builtin\"], baseName' = FName \"concat_map\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 58}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Query_Language.Builtin.ConcatMap\", haskellPrefix = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule = [MName \"Query_Language\",MName \"Builtin\"], baseName = MName \"ConcatMap\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Query_Language.Builtin.order_by\", haskellPrefix' = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule' = [MName \"Query_Language\",MName \"Builtin\"], baseName' = FName \"order_by\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 66}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Query_Language.Builtin.OrderBy\", haskellPrefix = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule = [MName \"Query_Language\",MName \"Builtin\"], baseName = MName \"OrderBy\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Query_Language.Builtin.reduce\", haskellPrefix' = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule' = [MName \"Query_Language\",MName \"Builtin\"], baseName' = FName \"reduce\"}, fieldNumber = FieldId {getFieldId = 9}, wireTag = WireTag {getWireTag = 74}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Query_Language.Reduction\", haskellPrefix = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule = [MName \"Query_Language\"], baseName = MName \"Reduction\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Query_Language.Builtin.grouped_map_reduce\", haskellPrefix' = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule' = [MName \"Query_Language\",MName \"Builtin\"], baseName' = FName \"grouped_map_reduce\"}, fieldNumber = FieldId {getFieldId = 10}, wireTag = WireTag {getWireTag = 82}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Query_Language.Builtin.GroupedMapReduce\", haskellPrefix = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule = [MName \"Query_Language\",MName \"Builtin\"], baseName = MName \"GroupedMapReduce\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Query_Language.Builtin.range\", haskellPrefix' = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule' = [MName \"Query_Language\",MName \"Builtin\"], baseName' = FName \"range\"}, fieldNumber = FieldId {getFieldId = 11}, wireTag = WireTag {getWireTag = 90}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Query_Language.Builtin.Range\", haskellPrefix = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule = [MName \"Query_Language\",MName \"Builtin\"], baseName = MName \"Range\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"