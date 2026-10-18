-- | The typed half: the variables a program needs, named and typed once as a
-- | record, read from an environment in one go, with every missing or
-- | malformed one reported together.
-- |
-- |     type Variables = ( "PORT" :: Maybe Int, "DB_PATH" :: String )
-- |
-- |     Env.Typed.read (Proxy :: _ (Record Variables)) env
-- |       :: Either Errors { "PORT" :: Maybe Int, "DB_PATH" :: String }
module Env.Typed
  ( read
  , Errors
  , EnvError(..)
  , printErrors
  , class ParseValue
  , parseValue
  , class ReadVariable
  , readVariable
  , class ReadRecord
  , readRecord
  ) where

import Prelude

import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString) as Int
import Data.Foldable (foldMap)
import Data.List (List(..))
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..))
import Data.Number (fromString) as Number
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (toCharArray) as String
import Data.String.Common (toLower)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Foreign.Object (Object, lookup)
import Prim.Row (class Cons, class Lacks) as Row
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Record.Builder (Builder) as Record
import Record.Builder as RB
import Type.Proxy (Proxy(..))
import Type.RowList (class ListToRow)

-- | The variables the record names, from an environment. Every field is a
-- | variable: `"PORT" :: Int` has to be there and parse, `"PORT" :: Maybe Int`
-- | may be missing but has to parse if it is there. A field of a type of your
-- | own needs a `ParseValue` instance. All problems are reported together.
read
  :: ∀ r rl
   . RowToList r rl
  => ReadRecord rl r
  => Proxy (Record r)
  -> Object String
  -> Either Errors (Record r)
read _ env = RB.buildFromScratch <$> readRecord (Proxy :: _ rl) env

-- | What stopped a record from being read: for each variable, that it is
-- | missing (`EnvLookupError`) or does not parse (`EnvParseError`), by name.
-- | At least one, or the record would have been read.
type Errors = NonEmptyList EnvError

-- | What went wrong with one variable: it is not set, or its text does not
-- | parse, with the name and then the instance's reason.
data EnvError
  = EnvLookupError String
  | EnvParseError String String

derive instance Eq EnvError

derive instance Generic EnvError _

instance Show EnvError where
  show = genericShow

-- | One message naming every variable in question, for a log or a stderr
-- | line before exiting.
printErrors :: Errors -> String
printErrors errors = case NEL.uncons errors of
  { head, tail: Nil } -> message head
  _ -> "Multiple environment errors: " <> foldMap (\x -> "\n* " <> message x) errors
  where
  message = case _ of
    EnvLookupError name -> "The required variable \"" <> name <> "\" is not set."
    EnvParseError name reason -> "The variable \"" <> name <> "\" " <> reason <> "."

-- | How a variable's text becomes a value of the field's type. Write an
-- | instance for a type of your own. A `Left` is the reason the text does not
-- | parse, and completes the sentence `The variable "NAME" …`: "is not an
-- | integer", "is not one of console, resend", "is empty".
class ParseValue ty where
  parseValue :: String -> Either String ty

instance ParseValue String where
  parseValue = Right

instance ParseValue Char where
  parseValue = String.toCharArray >>> case _ of
    [ c ] -> Right c
    _ -> Left "is not a single character"

-- | `true`, `yes`, `on`, `enabled` and `1`, or `false`, `no`, `off`, `disabled`
-- | and `0`, in any case.
instance ParseValue Boolean where
  parseValue = toLower >>> case _ of
    "true" -> Right true
    "yes" -> Right true
    "on" -> Right true
    "enabled" -> Right true
    "1" -> Right true
    "false" -> Right false
    "no" -> Right false
    "off" -> Right false
    "disabled" -> Right false
    "0" -> Right false
    _ -> Left "is not one of true, yes, on, enabled, 1, false, no, off, disabled, 0"

instance ParseValue Int where
  parseValue = Int.fromString >>> note "is not an integer"

instance ParseValue Number where
  parseValue = Number.fromString >>> note "is not a number"

-- | Looks a variable up and parses it: a `Maybe` field may be missing, any
-- | other has to be there. Exported because the compiler solves it where
-- | `read` is called; there is nothing to write an instance of it for.
class ReadVariable ty where
  readVariable :: String -> Object String -> Either EnvError ty

instance ParseValue a => ReadVariable (Maybe a) where
  readVariable name env = case lookup name env of
    Nothing -> Right Nothing
    Just value -> bimap (EnvParseError name) Just (parseValue value)
else instance ParseValue a => ReadVariable a where
  readVariable name env = do
    value <- note (EnvLookupError name) (lookup name env)
    lmap (EnvParseError name) (parseValue value)

-- | Builds the record field by field from the row's list, collecting the
-- | errors of every field instead of stopping at the first.
class
  ReadRecord (rl :: RowList Type) (rout :: Row Type)
  | rl -> rout where
  readRecord :: Proxy rl -> Object String -> Either Errors (Record.Builder {} { | rout })

instance
  ( IsSymbol name
  , ReadVariable ty
  , ListToRow tail tailRout
  , ReadRecord tail tailRout
  , Row.Cons name ty tailRout rout
  , Row.Lacks name tailRout
  ) =>
  ReadRecord (Cons name ty tail) rout where
  readRecord _ env = insert value tail
    where
    nameP = Proxy :: _ name
    value = readVariable (reflectSymbol nameP) env :: Either EnvError ty
    tail = readRecord (Proxy :: _ tail) env

    insert (Right val) (Right builder) = Right $ RB.insert nameP val <<< builder
    insert (Right _) (Left errs) = Left errs
    insert (Left err) (Right _) = Left $ NEL.singleton err
    insert (Left err) (Left errs) = Left $ NEL.cons err errs

instance ReadRecord Nil () where
  readRecord _ _ = pure $ RB.union {}