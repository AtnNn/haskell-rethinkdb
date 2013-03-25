{-# LANGUAGE KindSignatures, DataKinds, ExistentialQuantification, GADTs,
             TypeFamilies, TypeOperators, PolyKinds, ConstraintKinds, RankNTypes,
             MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
             FlexibleContexts, RecordWildCards, GeneralizedNewtypeDeriving,
             ViewPatterns, OverloadedStrings, ScopedTypeVariables #-}

-- | Building RQL queries in Haskell
module Database.RethinkDB.Term where

import qualified Data.Vector as V
import qualified Data.HashMap.Lazy as M
import Data.Maybe
import Data.String
import Data.List
import qualified Data.Sequence as S
import Control.Monad.State
import Control.Applicative
import Data.Default
import qualified Data.Text as T
import qualified Data.Aeson as J

import Text.ProtocolBuffers hiding (Key, cons, Default)
import Text.ProtocolBuffers.Basic hiding (Default)

import Database.RethinkDB.Protobuf.Ql2.Term2 as Term2
import Database.RethinkDB.Protobuf.Ql2.Term2.TermType as TermType
import Database.RethinkDB.Protobuf.Ql2.Term2.AssocPair
import Database.RethinkDB.Protobuf.Ql2.Query2 as Query2
import Database.RethinkDB.Protobuf.Ql2.Query2.QueryType
import Database.RethinkDB.Protobuf.Ql2.Datum as Datum
import Database.RethinkDB.Protobuf.Ql2.Datum.DatumType

import Database.RethinkDB.Type
import Database.RethinkDB.Objects as O

type F1 = Term O.Datum -> Term O.Datum
type F2 = Term O.Datum -> Term O.Datum -> Term O.Datum

-- | An RQL term
data Term (t :: *) where
  Term :: Selection t ~ False => {
    baseTerm :: State QuerySettings BaseTerm } -> Term t
  Selection :: Selection t ~ True => {
    selectionTable :: Table,
    baseTerm :: State QuerySettings BaseTerm } -> Term t

data BaseTerm = BaseTerm {
    termType :: TermType,
    termDatum :: Maybe Datum.Datum,
    termArgs :: BaseArray,
    termOptArgs :: [BaseAttribute] }

data QuerySettings = QuerySettings {
  queryToken :: Int64,
  queryDefaultDatabase :: Database,
  queryVarIndex :: Int,
  queryUseOutdated :: Maybe Bool
  }

instance Default QuerySettings where
  def = QuerySettings 0 (Database "") 0 Nothing

withQuerySettings :: (Selection x ~ False) =>
                     (QuerySettings -> Term x) -> Term x
withQuerySettings f = Term $ (baseTerm . f) =<< get

newVar :: State QuerySettings (Term Double)
newVar = do
  QuerySettings {..} <- get
  let n = queryVarIndex + 1
  put QuerySettings {queryVarIndex = n, ..}
  return $ expr n

instance Show BaseTerm where
  show (BaseTerm DATUM (Just dat) _ _) = showD dat
  show (BaseTerm fun _ args optargs) =
    show fun ++ " (" ++ concat (intersperse ", " (map show args ++ map show optargs)) ++ ")"

showD :: Datum.Datum -> String
showD d = case Datum.type' d of
  R_NUM -> show' $ r_num d
  R_BOOL -> show' $ r_bool d
  R_STR -> show' $ r_str d
  R_ARRAY -> show $ r_array d
  R_OBJECT -> show $ r_object d
  R_NULL -> "null"
  where show' Nothing = "Nothing"
        show' (Just a) = show a

-- | Convert other types to terms
class Expr e where
  type Type e
  expr :: e -> Term (Type e)

instance Expr (Term t) where
  type (Type (Term t)) = t
  expr t = t

-- | A list of terms
data Array = Array { baseArray :: State QuerySettings BaseArray }

type BaseArray = [BaseTerm]

-- | Build arrays of exprs
class Arr a where
  arr :: a -> Array

cons :: Expr e => e -> Array -> Array
cons x xs = Array $ do
  bt <- baseTerm (expr x)
  xs' <- baseArray xs
  return $ bt : xs'

instance Arr () where
  arr () = Array $ return []

instance Expr a => Arr [a] where
  arr [] = Array $ return []
  arr (x:xs) = cons x (arr xs)

instance (Expr a, Expr b) => Arr (a, b) where
  arr (a,b) = cons a $ cons b $ arr ()

instance (Expr a, Expr b, Expr c) => Arr (a, b, c) where
  arr (a,b,c) = cons a $ cons b $ cons c $ arr ()

instance (Expr a, Expr b, Expr c, Expr d) => Arr (a, b, c, d) where
  arr (a,b,c,d) = cons a $ cons b $ cons c $ cons d $ arr ()

instance Arr Array where
  arr = id

-- | A list of String/Expr pairs
data Object = Object { baseObject :: State QuerySettings [BaseAttribute] }

data Attribute = forall e . (Expr e) => Key := e

data BaseAttribute = BaseAttribute Key BaseTerm

instance Show BaseAttribute where
  show (BaseAttribute a b) = T.unpack a ++ ": " ++ show b

class Obj o where
  obj :: o -> Object

instance Obj [Attribute] where
  obj = Object . mapM base
    where base (k := e) = BaseAttribute k <$> baseTerm (expr e)

instance Obj Object where
  obj = id

instance Obj () where
  obj _ = Object $ return []

-- | Build a term
op :: (Arr a, Obj o, Selection t ~ False) => TermType -> a -> o -> Term t
op t a b = Term $ do
  a' <- baseArray (arr a)
  b' <- baseObject (obj b)
  return $ BaseTerm t Nothing a' b'

-- | Refine a selection
refine :: (Arr a, Obj o, Expr s, Selection (Type s) ~ Selection t) =>
          TermType -> a -> o -> s -> Term t
refine t a b s = term $ do
  a' <- baseArray (s `cons` arr a)
  b' <- baseObject (obj b)
  return $ BaseTerm t Nothing a' b'
  where term = case expr s of
          Term _ -> Term
          Selection t _ -> Selection t

type a ~~ b = (Expr a, Cast b (Type a))

class Expr f => f ~~~ t
instance (Cast a' a, Cast b b')
  => (Term a -> Term b) ~~~ Function '[a'] b'
instance (Cast a' a, Cast b' b, Cast c c')
  => (Term a -> Term b -> Term c) ~~~ Function '[a', b'] c'

datumTerm :: (Selection t ~ False) => DatumType -> Datum.Datum -> Term t
datumTerm t d = Term $ return $ BaseTerm DATUM (Just d { Datum.type' = t }) [] []

str :: String -> Term String
str s = datumTerm R_STR defaultValue { r_str = Just (uFromString s) }


instance Expr Int64 where
  type Type Int64 = Double
  expr i = datumTerm R_NUM defaultValue { r_num = Just (fromIntegral i) }

instance Expr Int where
  type Type Int = Double
  expr i = datumTerm R_NUM defaultValue { r_num = Just (fromIntegral i) }

instance Num (Term Double) where
  fromInteger x = datumTerm R_NUM defaultValue { r_num = Just (fromInteger x) }
  a + b = op ADD (a, b) ()
  a * b = op MUL (a, b) ()
  abs n = op BRANCH (op TermType.LT (n, 0 :: Double) (), negate n, n) ()
  signum n = op BRANCH (op TermType.LT (n, 0 :: Double) (),
                        -1 :: Double,
                        op BRANCH (op TermType.EQ (n, 0 :: Double) (), 0 :: Double, 1 :: Double) ()) ()

instance Expr T.Text where
  type Type T.Text = String
  expr t = datumTerm R_STR defaultValue { r_str = Just (uFromString $ T.unpack t) }

instance Expr Bool where
  type Type Bool = Bool
  expr b = datumTerm R_BOOL defaultValue { r_bool = Just b }

instance Expr () where
  type Type () = Null
  expr _ = datumTerm R_NULL defaultValue

instance IsString (Term String) where
  fromString s = datumTerm R_STR defaultValue { r_str = Just (uFromString $ s) }

instance (Selection a ~ False) =>
         Expr (Term a -> Term b) where
  type Type (Term a -> Term b) = Function '[a] b
  expr f = Term $ do
    v <- newVar
    baseTerm $ op FUNC ([v], f (op VAR [v] ())) ()

instance Expr (Term a -> Term b -> Term c) where
  type Type (Term a -> Term b -> Term c) = Function '[a, b] c
  expr f = Term $ do
    a <- newVar
    b <- newVar
    baseTerm $ op FUNC ([a, b], f (op VAR [a] ()) (op VAR [b] ())) ()

instance Expr Table where
  type Type Table = Table
  expr (Table mdb name _) = withQuerySettings $ \QuerySettings {..} ->
    op TABLE (fromMaybe queryDefaultDatabase mdb, name) $ catMaybes [
      fmap ("use_outdated" :=) queryUseOutdated ]

instance Expr Database where
  type Type Database = Database
  expr (Database name) = op DB [name] ()

newtype MaybeDatabase = MaybeDatabase (Maybe Database)

instance Expr MaybeDatabase where
  type Type MaybeDatabase = Database
  expr (MaybeDatabase Nothing) = withQuerySettings $ \QuerySettings {..} ->
    expr queryDefaultDatabase
  expr (MaybeDatabase (Just db)) = expr db

instance Expr Document where
  type Type Document = Object
  expr doc@(Document table key) =
    op FUNCALL (\(x :: Term a) -> op BRANCH (op TermType.EQ (x, ()) (),
                                 op ERROR [str $ "The document " ++ show doc ++
                                           " does not exist"] (),
                                 x) (),
                op GET (table, key) ()) ()

instance Expr J.Value where
  type Type J.Value = O.Datum
  expr J.Null = cast $ expr ()
  expr (J.Bool b) = cast $ expr b
  expr (J.Number n) = cast $ expr (fromRational (toRational n) :: Double)
  expr (J.String t) = cast $ expr t
  expr (J.Array a) = cast $ expr a
  expr (J.Object o) = cast $ expr o

instance Expr Double where
  type Type Double = Double
  expr d = datumTerm R_NUM defaultValue { r_num = Just d }

instance Expr x => Expr (V.Vector x) where
  type Type (V.Vector x) = Array
  expr v = expr (V.toList v)

instance Expr a => Expr [a] where
  type Type [a] = Array
  expr a = expr $ arr a

instance Expr Array where
  type Type Array = Array
  expr a = op MAKE_ARRAY a ()

instance Expr e => Expr (M.HashMap T.Text e) where
  type Type (M.HashMap T.Text e) = Object
  expr m = expr $ obj $ map (uncurry (:=)) $ M.toList m

instance Expr Object where
  type Type Object = Object
  expr o = op MAKE_OBJ () o

cast :: Cast a b => Term a -> Term b
cast (Term x) = (Term x)

buildTerm :: Term t -> State QuerySettings Term2
buildTerm = fmap buildBaseTerm . baseTerm

buildBaseTerm :: BaseTerm -> Term2
buildBaseTerm BaseTerm {..} = defaultValue {
    Term2.type' = termType,
    datum = termDatum,
    args = buildBaseArray termArgs,
    optargs = buildTermAssoc termOptArgs }

buildBaseArray :: BaseArray -> Seq Term2
buildBaseArray [] = S.empty
buildBaseArray (x:xs) = buildBaseTerm x S.<| buildBaseArray xs

buildTermAssoc :: [BaseAttribute] -> Seq AssocPair
buildTermAssoc = S.fromList . map buildTermAttribute

buildTermAttribute :: BaseAttribute -> AssocPair
buildTermAttribute (BaseAttribute k v) = AssocPair (uFromString $ T.unpack k) (buildBaseTerm v)

buildQuery :: Term t -> Int64 -> Database -> Query2
buildQuery term token db = defaultValue {
  Query2.type' = START,
  query = Just $ fst $ runState (buildTerm term) (def {queryToken = token,
                                                       queryDefaultDatabase = db }) }

type SequenceType s = SequenceType' (Type s)
type family SequenceType' t
type instance SequenceType' StreamSelection = StreamSelection
type instance SequenceType' Table = StreamSelection
type instance SequenceType' Sequence = Sequence
type instance SequenceType' Stream = Stream
type instance SequenceType' Array = Array

type ElemType s = ElemType' (Type s)
type family ElemType' t
type instance ElemType' StreamSelection = SingleSelection
type instance ElemType' Table = SingleSelection
type instance ElemType' Sequence = O.Datum
type instance ElemType' Stream = O.Datum
type instance ElemType' Array = O.Datum
