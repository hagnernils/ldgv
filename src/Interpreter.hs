{-# LANGUAGE OverloadedStrings #-}
module Interpreter (interpret) where
import qualified Config as C
import qualified Control.Monad as M
import qualified Grammar as G
import qualified Control.Concurrent.Chan as Chan
import Control.Concurrent (forkIO)
import Control.Monad.Reader (ask, local, runReaderT, liftIO)
import Environment
import Syntax
import Tokens (alexScanTokens)

-- | interpret the "main" value in an ldgv file given over stdin
interpret :: String -> IO Value
interpret s = do
    let tokens = alexScanTokens s
    let parsed = G.parseCalc tokens
  
    -- gather Type and Function definitions
    let env = map makeEntry $ filter isInterestingDecl parsed where
        isInterestingDecl (DFun _ _ _ _) = True
        isInterestingDecl _ = False
        makeEntry d@(DFun str args e mt) = (str, VDecl d)

    -- find the main DFun
    case lookup "main" env of
                          Nothing -> fail "No 'main' value declaration found, exiting"
                          Just (VDecl decl) -> runReaderT (evalDFun decl) env

-- | interpret a DFun (Function declaration)
evalDFun :: Decl -> InterpretM
evalDFun decl@(DFun name [] expression _) = interpret' expression  -- a Declaration without free variables can be just interpreted
evalDFun decl@(DFun name ((_, id, _):binds) e mty) = do
                          env <- ask
                          let inner = DFun name binds e mty
                          let f = \arg -> do
                                 liftIO $ runReaderT (evalDFun inner) ((id, arg):env) 
                          return $ VFun f

-- | interpret a single Expression
interpret' :: Exp ->  InterpretM
interpret' e =
  M.ap
  (return (\val -> C.trace ("Leaving interpretation of " ++ show e ++ " with value " ++ show val) val)) $
  case C.trace ("Invoking interpretation on " ++ show e) e of
  Unit -> return VUnit
  Var s -> envlookup s
  Lab s -> return $ VLabel s
  Int i -> return $ VInt i
  Nat i -> return $ VInt i
  Plus e1 e2 -> mathHelper (+) e1 e2
  Minus e1 e2 ->  mathHelper (-) e1 e2
  Times e1 e2 ->  mathHelper (*) e1 e2
  Div e1 e2 ->  mathHelper quot e1 e2
  Negate e1 ->  mathHelper (-) (Int 0) e1
  Succ e -> mathHelper (+) (Int 1) e
  Let s e1 e2 -> do
      v  <- interpret' e1
      local ((s, v):) $ interpret' e2
  LetPair s1 s2 e1 e2 -> do
      -- simple pair unpacking
      v <- interpret' e1
      case v of
        (VPair v1 v2) -> local (\env -> (s1, v1):(s2, v2):env) $ interpret' e2
  Pair mul s e1 e2 -> do
      -- dependent pair: use the result of e1 in the interpretation of e2
      v1 <- interpret' e1
      v2 <- local ((s, v1) :) $ interpret' e2
      return $ VPair v1 v2
  Fst e -> do
      v <- interpret' e
      case v of
          (VPair s1 s2) -> return s1
  Snd e -> do
      v <- interpret' e
      case v of
          (VPair s1 s2) -> return s2
  Lam m i t e -> do
                 env <- ask 
                 let f = \arg -> do
                                 liftIO $ runReaderT (interpret' e) ((i, arg):env)
                 return $ VFun f
  App e1 e2 -> do
      -- interpret e1 first, because the innermost application
      -- is the function with its first argument
      arg <- interpret' e2
      v <- interpret' e1
      -- check if the variable refers to a function label
      case v of
          VDecl d -> do
            res <- evalDFun d
            case res of
              (VFun f) -> do
                f arg
          VFun f -> do  -- if not, we can apply the function directly
            f arg
          _ -> do
            fail $ "Trying to Apply " ++ show e2 ++ " to " ++ show e1
  Fork e -> do
      penv <- ask
      liftIO $ forkIO (do
                      res <- runReaderT (interpret' e) penv
                      C.traceIO "Ran a forked operation")
      return VUnit
  New t -> do
    r <- liftIO Chan.newChan
    w <- liftIO Chan.newChan
    return $ VPair (VChan r w) (VChan w r)
  Send e -> do
      v <- interpret' e
      case v of
        (VChan _ c) -> return $ VFun $ \arg -> do
                                        liftIO $ C.traceIO $ "Sending Value " ++ show arg ++ " on Channel " ++ show v
                                        liftIO (Chan.writeChan c arg)
                                        return v -- Because writeChan does not change the channel, we give back the old one
  Recv e -> do
      v <- interpret' e
      case v of
        (VChan c _) -> do
          val <- liftIO $ Chan.readChan c
          liftIO $ C.traceIO $ "Read " ++ show val ++ " from Chan "
          return $ VPair val v
  Case e cases -> do
      v <- interpret' e
      case v of
        (VLabel s) -> do
          let e1 = lookup s cases
          case e1 of
            Just e' -> interpret' e'
            Nothing -> fail $ "No case found for label " ++ show v ++ " in cases " ++ show cases
  exp@(NatRec e1 e2 i1 t1 i2 t2 e3) -> do
  -- returns a function indexed over e1 (should be a variable pointing to a Nat)
  -- e1 should evaluate to the recursive variable which gets decreased each time the
  -- non-zero case is evaluated
  -- e2 is the zero case
  -- e3 is the nonzero case
         i <- interpret' e1
         case i of
                 VInt 0 -> interpret' e2
                 VInt 1 -> do
                             zero <- interpret' e2
                             local (\env -> (i1, VInt 1):(i2, zero):env) $ interpret' e3
                 VInt n -> do
                        -- interpret the n-1 case i2 and add it to the env
                        -- together with n before interpreting the body e3
                        let newexp = NatRec (Var i1) e2 i1 t1 i2 t2 e3
                        lower <- local ((i1, VInt (n-1)):) $ interpret' newexp
                        local (\env -> (i1, VInt n):(i2, lower):env) $ interpret' e3
  e -> do fail $ "Expression " ++ show e ++ " not implemented"


-- | helper function for mathematical operations
mathHelper op e1 e2 = do
    v1 <- interpret' e1
    v2 <- interpret' e2
    return $ case (v1, v2) of
      (VInt a, VInt b) -> VInt (op a b)


