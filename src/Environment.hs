module Environment (InterpretM, Env, Value (..), envlookup) where
import Syntax (Decl)
import Control.Concurrent.Chan (Chan)
import Control.Monad.Reader (ReaderT, ask)
import Language.Javascript.JSaddle (JSM)

-- | the interpretation monad
type InterpretM = ReaderT Env JSM Value

-- | maps identifiers to Values of expressions
type Env = [EnvEntry]
type EnvEntry = (String, Value)

envlookup :: String -> InterpretM
envlookup id = do
    identifiers <- ask
    case lookup id identifiers of
        Nothing -> fail ("No Value for identifier " ++ id ++ " in Environment")
        Just val -> return val

-- | (Unit, Label, Int, Values of self-declared Data Types), Channels
data Value = VUnit
      | VLabel String
      | VInt Int
      -- we have two channels, one for reading and one for writing to the other
      -- end, so we do not read our own written values, but block when reading while the other side didnt write anything
      | VChan (Chan Value) (Chan Value)
      | VPair Value Value -- pair of ids that map to two values
      | VDecl Decl -- when an identifier maps to another function we have not yet interpreted
      | VFun (Value -> InterpretM) -- Function Type

instance Show Value where
    show v = case v of
        VUnit -> "VUnit"
        VLabel s -> "VLabel " ++ s
        VInt i -> "VInt " ++ show i
        VChan _ _ -> "VChan"
        VPair a b -> "(" ++ show a ++ "," ++ show b ++ ")"
        VDecl d -> "VDecl " ++ show d
        VFun _ -> "VFunction "
