# purescript-env

Configuration from the process environment, as one typed record. The variables a program needs are named and typed once, read in one go, and every missing or malformed one is reported together, so a server that is started half-configured says so at once instead of failing on the first variable it happens to touch.

Until it is on the registry, add it to `spago.yaml` as a git package, pinned to a commit:

```yaml
workspace:
  extraPackages:
    env:
      git: https://github.com/m-rinaldi/purescript-env.git
      ref: <commit>
```

```purescript
import Env as Env
import Type.Proxy (Proxy(..))

type Variables =
  ( "PORT" :: Maybe Int        -- may be missing, has to parse if it is there
  , "DB_PATH" :: String        -- has to be there
  , "MAIL_PROVIDER" :: Provider -- a type of your own, with a ParseValue instance
  )

main :: Effect Unit
main = Env.load (Proxy :: _ (Record Variables)) >>= case _ of
  Left errors -> throw (Env.printErrors errors)
  Right vars -> start vars
```

A `.env` file in the working directory is read first, for development. A variable the environment already has keeps its value, so a deployment's own environment always wins; no file is no error, a file that cannot be read is.

No JavaScript of its own and no npm dependency. It runs on Node 20.12 or later, which is where `process.loadEnvFile` appeared. It is not on the registry yet.

## Two halves and the module that joins them

- `Env.Dot` folds a `.env` file into the process environment: `environment` (the file in the working directory) and `environmentFrom path`. The loading is Node's own `process.loadEnvFile` (Node 20.12 and later), through [`node-process`](https://github.com/purescript-node/purescript-node-process), so there is no parser in PureScript: the format is whatever Node accepts, which means `KEY=value` lines, `#` comments and quoted values, and no `${VAR}` expansion. The binding came with [pull request 47](https://github.com/purescript-node/purescript-node-process/pull/47) and is in `node-process` 11.3.0 and later.
- `Env.Typed` reads the record from an `Object String`: `read proxy object`, pure, so what is done with the variables can be tested without touching the real environment. `Errors` is a non-empty list of `EnvError` (`EnvLookupError name`, `EnvParseError name reason`); `printErrors` makes one message of it, one sentence per variable: `The variable "PORT" is not an integer.` A field of a type of your own needs a `ParseValue` instance, `String -> Either String ty`, whose `Left` is the reason and completes that sentence ("is empty", "is not one of console, resend"); `Int`, `Number`, `Boolean` (`true`, `yes`, `on`, `enabled`, `1` or `false`, `no`, `off`, `disabled`, `0`, in any case), `Char` and `String` have one. The module is [`typedenv`](https://github.com/nsaunders/purescript-typedenv) by Nick Saunders copied in (MIT, `LICENSE-THIRD-PARTY`), with its `ReadEnv` class simplified as in [pull request 13](https://github.com/nsaunders/purescript-typedenv/pull/13): the builder's input row, always `()`, is no longer a parameter, the classes renamed (`ReadEnv` is `ReadRecord`, `ReadValue` is `ReadVariable`), and `parseValue` returning `Either String` instead of `Maybe`, so that a failed parse says why. There is no dependency on `typedenv`.
- `Env` re-exports both and adds `load proxy`, the two in one call. Most programs import this module alone, qualified.

`Node.Process` does not have to be imported beside these.

## Tests

`spago test` reads a fixture file, checks that the environment wins over it, that a missing file is fine and an unreadable one is not, and the ways a record can fail to read.

## Licence

MIT, see `LICENSE`. `Env.Typed` derives from [`typedenv`](https://github.com/nsaunders/purescript-typedenv) by Nick Saunders, also MIT; its notice is in `LICENSE-THIRD-PARTY`.
