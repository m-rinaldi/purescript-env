module Test.Env.Main (main) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (elem, for_)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, bracket)
import Effect.Class (liftEffect)
import Env (EnvError(..), class ParseValue)
import Env as Env
import Env.Dot as Dot
import Env.Typed as Typed
import Foreign.Object as Object
import Node.Process as Process
import Test.Spec (describe, it)
import Test.Spec.Assertions (expectError, fail, shouldEqual, shouldSatisfy)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Type.Proxy (Proxy(..))

type Variables = ("ENV_TEST_PORT" :: Int, "ENV_TEST_NAME" :: String, "ENV_TEST_DEBUG" :: Maybe Boolean)

data Provider = Console | Resend

derive instance Eq Provider

instance Show Provider where
  show = case _ of
    Console -> "Console"
    Resend -> "Resend"

instance ParseValue Provider where
  parseValue = case _ of
    "console" -> Right Console
    "resend" -> Right Resend
    _ -> Left "is not one of console, resend"

fixture :: String
fixture = "test/fixtures/sample.env"

-- | Runs the test with these variables unset, and puts back whatever they
-- | were, so that one test cannot see what another left behind.
withoutVariables :: Array String -> Aff Unit -> Aff Unit
withoutVariables names test = bracket save restore (const test)
  where
  save = liftEffect do
    saved <- traverse (\name -> { name, value: _ } <$> Process.lookupEnv name) names
    for_ names Process.unsetEnv
    pure saved
  restore saved = liftEffect $ for_ saved \{ name, value } -> case value of
    Nothing -> Process.unsetEnv name
    Just v -> Process.setEnv name v

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Env.Typed.read" do
    it "reads the variables a record names, typed" do
      let env = Object.fromHomogeneous { "ENV_TEST_PORT": "8080", "ENV_TEST_NAME": "x", "ENV_TEST_DEBUG": "true", "OTHER": "ignored" }
      Typed.read (Proxy :: _ (Record Variables)) env `shouldEqual` Right { "ENV_TEST_PORT": 8080, "ENV_TEST_NAME": "x", "ENV_TEST_DEBUG": Just true }

    it "lets a Maybe field be missing, and nothing else" do
      let env = Object.fromHomogeneous { "ENV_TEST_PORT": "8080", "ENV_TEST_NAME": "x" }
      Typed.read (Proxy :: _ (Record Variables)) env `shouldEqual` Right { "ENV_TEST_PORT": 8080, "ENV_TEST_NAME": "x", "ENV_TEST_DEBUG": Nothing }
      Typed.read (Proxy :: _ (Record Variables)) (Object.delete "ENV_TEST_NAME" env) `shouldEqual` Left (NEL.singleton (EnvLookupError "ENV_TEST_NAME"))

    it "reports every missing or malformed variable together, by name and reason" do
      let env = Object.fromHomogeneous { "ENV_TEST_PORT": "many", "ENV_TEST_DEBUG": "maybe" }
      case Typed.read (Proxy :: _ (Record Variables)) env of
        Right _ -> fail "read a record from a broken environment"
        Left errors -> do
          -- The order is the record's, which is not something to rely on.
          NEL.length errors `shouldEqual` 3
          for_ [ EnvParseError "ENV_TEST_PORT" "is not an integer", EnvLookupError "ENV_TEST_NAME", EnvParseError "ENV_TEST_DEBUG" "is not one of true, yes, on, enabled, 1, false, no, off, disabled, 0" ] \expected ->
            errors `shouldSatisfy` elem expected
          let message = Typed.printErrors errors
          for_ [ "The variable \"ENV_TEST_PORT\" is not an integer.", "The required variable \"ENV_TEST_NAME\" is not set.", "The variable \"ENV_TEST_DEBUG\" is not one of true, yes, on, enabled, 1, false, no, off, disabled, 0." ] \sentence ->
            message `shouldSatisfy` contains (Pattern sentence)

    it "reads a Boolean from true, yes, on, enabled, 1 and false, no, off, disabled, 0, in any case" do
      let flag = Proxy :: _ (Record ("ENV_TEST_FLAG" :: Boolean))
      for_ [ "true", "yes", "on", "enabled", "1", "TRUE", "Yes" ] \text ->
        Typed.read flag (Object.singleton "ENV_TEST_FLAG" text) `shouldEqual` Right { "ENV_TEST_FLAG": true }
      for_ [ "false", "no", "off", "disabled", "0", "FALSE", "Off" ] \text ->
        Typed.read flag (Object.singleton "ENV_TEST_FLAG" text) `shouldEqual` Right { "ENV_TEST_FLAG": false }
      for_ [ "", "y", "ja", "2" ] \text ->
        Typed.read flag (Object.singleton "ENV_TEST_FLAG" text)
          `shouldEqual` Left (NEL.singleton (EnvParseError "ENV_TEST_FLAG" "is not one of true, yes, on, enabled, 1, false, no, off, disabled, 0"))

    it "reads a Char from one character and nothing else" do
      let char = Proxy :: _ (Record ("ENV_TEST_CHAR" :: Char))
      Typed.read char (Object.singleton "ENV_TEST_CHAR" "x") `shouldEqual` Right { "ENV_TEST_CHAR": 'x' }
      for_ [ "", "xy" ] \text ->
        Typed.read char (Object.singleton "ENV_TEST_CHAR" text)
          `shouldEqual` Left (NEL.singleton (EnvParseError "ENV_TEST_CHAR" "is not a single character"))

    it "takes a ParseValue instance for a type of one's own, with its reason" do
      Typed.read (Proxy :: _ (Record ("ENV_TEST_MAIL" :: Provider))) (Object.singleton "ENV_TEST_MAIL" "resend")
        `shouldEqual` Right { "ENV_TEST_MAIL": Resend }
      Typed.read (Proxy :: _ (Record ("ENV_TEST_MAIL" :: Provider))) (Object.singleton "ENV_TEST_MAIL" "pigeon")
        `shouldEqual` Left (NEL.singleton (EnvParseError "ENV_TEST_MAIL" "is not one of console, resend"))
      Typed.printErrors (NEL.singleton (EnvParseError "ENV_TEST_MAIL" "is not one of console, resend"))
        `shouldEqual` "The variable \"ENV_TEST_MAIL\" is not one of console, resend."

  describe "Env.Dot.environmentFrom" do
    it "folds the file into the environment" do
      withoutVariables [ "ENV_TEST_FROM_FILE", "ENV_TEST_KEPT" ] do
        env <- liftEffect (Dot.environmentFrom fixture)
        Object.lookup "ENV_TEST_FROM_FILE" env `shouldEqual` Just "from-file"

    it "keeps a variable the environment already has" do
      withoutVariables [ "ENV_TEST_FROM_FILE", "ENV_TEST_KEPT" ] do
        liftEffect (Process.setEnv "ENV_TEST_KEPT" "from-environment")
        env <- liftEffect (Dot.environmentFrom fixture)
        Object.lookup "ENV_TEST_KEPT" env `shouldEqual` Just "from-environment"

    it "is fine without a file" do
      withoutVariables [ "ENV_TEST_FROM_FILE" ] do
        env <- liftEffect (Dot.environmentFrom "test/fixtures/missing.env")
        Object.lookup "ENV_TEST_FROM_FILE" env `shouldEqual` Nothing

    it "throws for a file that cannot be read" do
      expectError (liftEffect (Dot.environmentFrom "test/fixtures"))

  describe "Env.load" do
    it "is Env.Dot.environment and Env.Typed.read in one" do
      withoutVariables [ "ENV_TEST_FROM_FILE" ] do
        -- The workspace has no `.env` in tests; the record asks for nothing
        -- that is not there, so the result is what the environment has.
        result <- liftEffect (Env.load (Proxy :: _ (Record ("ENV_TEST_FROM_FILE" :: Maybe String))))
        result `shouldEqual` Right { "ENV_TEST_FROM_FILE": Nothing }
