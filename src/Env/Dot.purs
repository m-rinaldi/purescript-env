-- | The `.env` half: a file of `KEY=value` lines folded into the process
-- | environment, for development. A variable the environment already has
-- | keeps its value, so a deployment's own environment always wins, and no
-- | file is no error.
-- |
-- | Node does the loading (`process.loadEnvFile`, through `node-process`), so
-- | there is no parser here: the format is what Node accepts, `#` comments
-- | and quoted values included, `${VAR}` expansion not.
module Env.Dot
  ( environment
  , environmentFrom
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Exception (Error, catchException, throwException)
import Foreign (readString, unsafeToForeign)
import Foreign.Index (readProp)
import Foreign.Object (Object)
import Node.Process as Process

-- | The process environment after the `.env` file in the working directory
-- | was folded into it.
environment :: Effect (Object String)
environment = environmentFrom ".env"

-- | Like `environment`, with the file named. A file that is not there is no
-- | error; one that cannot be read (a directory, no permission) is thrown.
environmentFrom :: String -> Effect (Object String)
environmentFrom path = do
  loadIfPresent path
  Process.getEnv

loadIfPresent :: String -> Effect Unit
loadIfPresent path = catchException rethrowUnlessMissing (Process.loadEnvFile' path)
  where
  rethrowUnlessMissing err
    | isMissingFile err = pure unit
    | otherwise = throwException err

-- | Node reports a file that is not there with `code: "ENOENT"` on the error.
isMissingFile :: Error -> Boolean
isMissingFile err =
  case runExcept (readProp "code" (unsafeToForeign err) >>= readString) of
    Right "ENOENT" -> true
    _ -> false
