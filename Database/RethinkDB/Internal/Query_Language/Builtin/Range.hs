{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Database.RethinkDB.Internal.Query_Language.Builtin.Range (Range(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import {-# SOURCE #-} qualified Database.RethinkDB.Internal.Query_Language.Term as Query_Language (Term)
 
data Range = Range{attrname :: !P'.Utf8, lowerbound :: !(P'.Maybe Query_Language.Term),
                   upperbound :: !(P'.Maybe Query_Language.Term)}
           deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable Range where
  mergeAppend (Range x'1 x'2 x'3) (Range y'1 y'2 y'3)
   = Range (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3)
 
instance P'.Default Range where
  defaultValue = Range P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire Range where
  wireSize ft' self'@(Range x'1 x'2 x'3)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 9 x'1 + P'.wireSizeOpt 1 11 x'2 + P'.wireSizeOpt 1 11 x'3)
  wirePut ft' self'@(Range x'1 x'2 x'3)
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
             P'.wirePutOpt 18 11 x'2
             P'.wirePutOpt 26 11 x'3
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{attrname = new'Field}) (P'.wireGet 9)
             18 -> Prelude'.fmap
                    (\ !new'Field -> old'Self{lowerbound = P'.mergeAppend (lowerbound old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             26 -> Prelude'.fmap
                    (\ !new'Field -> old'Self{upperbound = P'.mergeAppend (upperbound old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> Range) Range where
  getVal m' f' = f' m'
 
instance P'.GPB Range
 
instance P'.ReflectDescriptor Range where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [10]) (P'.fromDistinctAscList [10, 18, 26])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Query_Language.Builtin.Range\", haskellPrefix = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule = [MName \"Query_Language\",MName \"Builtin\"], baseName = MName \"Range\"}, descFilePath = [\"Database\",\"RethinkDB\",\"Internal\",\"Query_Language\",\"Builtin\",\"Range.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Query_Language.Builtin.Range.attrname\", haskellPrefix' = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule' = [MName \"Query_Language\",MName \"Builtin\",MName \"Range\"], baseName' = FName \"attrname\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Query_Language.Builtin.Range.lowerbound\", haskellPrefix' = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule' = [MName \"Query_Language\",MName \"Builtin\",MName \"Range\"], baseName' = FName \"lowerbound\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Query_Language.Term\", haskellPrefix = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule = [MName \"Query_Language\"], baseName = MName \"Term\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Query_Language.Builtin.Range.upperbound\", haskellPrefix' = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule' = [MName \"Query_Language\",MName \"Builtin\",MName \"Range\"], baseName' = FName \"upperbound\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Query_Language.Term\", haskellPrefix = [MName \"Database\",MName \"RethinkDB\",MName \"Internal\"], parentModule = [MName \"Query_Language\"], baseName = MName \"Term\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"