module ProcessEnvironment where
import Syntax as S
import qualified Config as D
import Control.Concurrent.Chan as C
import Control.Monad.Reader as R
import Language.Javascript.JSaddle

-- | the interpretation monad
type InterpretM = R.ReaderT Env JSM Value

-- | maps identifiers to Values of expressions and stores
type Env = [EnvEntry]
type EnvEntry = (String, Value)

-- | create a new entry (requires identifier to be unique)
--createEntry :: EnvEntry -> R.ReaderT Env IO ()
--createEntry entry = do
--    liftIO $ D.traceIO $ "Creating Environment entry " ++ show entry
--    modify (\s1 -> entry : s1)

extendEnv :: EnvEntry -> Env -> Env
extendEnv e env = e:env

envlookup :: String -> InterpretM
envlookup id = do
    identifiers <- ask
    case lookup id identifiers of
        Nothing -> fail ("No Value for identifier " ++ id ++ " in ProcessEnvironment")
        Just val -> do
                    liftIO $ D.traceIO $ "Looked up " ++ id ++ " and found " ++ show val
                    >> pure val


printEnv :: Env -> String
printEnv [] = ""
printEnv (x:xs) = show x ++ "\n" ++ printEnv xs

-- | (Unit, Label, Int, Values of self-declared Data Types), Channels
data Value = VUnit
      | VLabel String
      | VInt Int
      -- we have two channels, one for reading and one for writing to the other
      -- end, so we do not read our own written values
      | VChan (C.Chan Value) (C.Chan Value)
      | VPair Value Value -- pair of ids that map to two values
      | VDecl S.Decl -- when an identifier maps to another function we have not yet interpreted
      | VType S.Type
      | VFun (Value -> InterpretM) -- Function Type

instance Show Value where
    show v = case v of
        VUnit -> "VUnit"
        VLabel s -> "VLabel " ++ s
        VInt i -> "VInt " ++ show i
        VChan _ _ -> "VChan"
        VPair a b -> "(" ++ show a ++ "," ++ show b ++ ")"
        VDecl d -> "VDecl " ++ show d
        VType t -> "VType " ++ show t
        VFun _ -> "VFunction "
