-- | Configuration from the process environment, as one typed record: the
-- | variables a program needs are named and typed once, read in one go, and
-- | every missing or malformed one is reported together. A `.env` file in the
-- | working directory is read first, for development; a variable the
-- | environment already has keeps its value.
-- |
-- |     type Variables = ( "PORT" :: Maybe Int, "DB_PATH" :: String )
-- |
-- |     main = Env.load (Proxy :: _ (Record Variables)) >>= case _ of
-- |       Left errors -> throw (Env.printErrors errors)
-- |       Right vars -> start vars
-- |
-- | The two halves are modules of their own and are re-exported from here:
-- | `Env.Dot` folds the file into the environment, `Env.Typed` reads the
-- | record. `load` is the one call that does both.
-- |
-- | Meant to be imported qualified: `Env.load`, `Env.read`, `Env.printErrors`.
module Env
  ( load
  , module Env.Dot
  , module Env.Typed
  ) where

import Prelude

import Data.Either (Either)
import Effect (Effect)
import Env.Dot (environment, environmentFrom)
import Env.Typed (Errors, EnvError(..), class ParseValue, parseValue, class ReadRecord, class ReadVariable, printErrors, read, readRecord, readVariable)
import Prim.RowList (class RowToList)
import Type.Proxy (Proxy)

-- | The `.env` file in the working directory, if there is one, folded into
-- | the process environment, and the variables the record names read from
-- | it: `Env.Dot.environment` and `Env.Typed.read` in one.
load
  :: ∀ r rl
   . RowToList r rl
  => ReadRecord rl r
  => Proxy (Record r)
  -> Effect (Either Errors (Record r))
load proxy = read proxy <$> environment
