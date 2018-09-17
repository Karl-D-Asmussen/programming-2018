{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
module SubsInterpreter
       (
         Value(..)
       , runExpr

       -- to test
       , SubsM(runSubsM)
       , modifyEnv 
       , putVar 
       , getVar 
       , getFunction 
       , evalExpr 
       , evalCompr
        
       -- aliases
       , Error 
       , Env 
       , Primitive 
       , PEnv 
       , Context 
      
       -- primitives
       , mkArray
       , compEql 
       , compLt 
       , opAdd 
       , opSub 
       , opMul 
       , opMod 
       )
       where

import SubsAst

-- You might need the following imports
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Arrow ( (+++) )
import qualified Data.Map as Map
import Data.Map(Map)
import Data.List

-- | A value is either an integer, the special constant undefined,
--   true, false, a string, or an array of values.
-- Expressions are evaluated to values.
data Value = IntVal Int
           | UndefinedVal
           | TrueVal | FalseVal
           | StringVal String
           | ArrayVal [Value]
           deriving (Eq, Show)

type Error = String
type Env = Map Ident Value
type Primitive = [Value] -> SubsM Value
type PEnv = Map FunName Primitive
type Context = (Env, PEnv)

initialContext :: Context
initialContext = (Map.empty, initialPEnv)

--- This is almost an RWS.
newtype SubsM a = SubsM {runSubsM :: Context -> Either Error (a, Env)}

instance Monad SubsM where
  return x = SubsM $ \(env, _) -> Right (x, env)
  m >>= f = SubsM & \ctx@(env, penv) -> runSubsM m ctx >>= \(x, env') -> runSubsM (f x) (env', penv)

  -- fail should be deprecated in favor of MonadFail
  fail s = SubsM $ const (Left s)

-- Utility
instance MonadState Env SubsM where
  get = SubsM $ \(env, _) -> return (env, env)
  put env = SubsM $ \(_, _) -> return ((), env)

-- More utility
instance MonadReader PEnv SubsM where
  reader f = SubsM $ \(env, penv) -> Right (f penv, env)
  local f m = SubsM $ \(env, penv) -> runSubsM m (env, f penv)

-- Even more utility
instance MonadError Error SubsM where
  throwError e = SubsM $ \_ -> Left e
  catchError m h = SubsM $ \ctx -> runSubsM m ctx `catchError` \err -> runSubsM (h err) ctx

-- You may modify these if you want, but it shouldn't be necessary
instance Functor SubsM where
  fmap = liftM
instance Applicative SubsM where
  pure = return
  (<*>) = ap


modifyEnv :: (Env -> Env) -> SubsM ()
modifyEnv = modify

putVar :: Ident -> Value -> SubsM ()
putVar i v = modify (Map.insert i v)

getVar :: Ident -> SubsM Value
getVar i = do i' <- gets (Map.lookup i)
              case i' of
                   Just x -> return x
                   Nothing -> throwError ("variable not in scope: " ++ i)

getFunction :: FunName -> SubsM Primitive
getFunction f = do i' <- reader (Map.lookup f)
                   case i' of
                        Just x -> return x
                        Nothing -> throwError ("function not found: " ++ f)

runExpr :: Expr -> Either Error Value
runExpr expr = (id +++ fst) $ runSubsM (evalExpr expr) initialContext

evalExpr :: Expr -> SubsM Value
evalExpr Undefined = return UndefinedVal
evalExpr TrueConst = return TrueVal
evalExpr FalseConst = return FalseVal
evalExpr (Number i) = return (IntVal i)
evalExpr (String s) = return (StringVal s)

evalExpr (Array vs) = liftM ArrayVal (mapM evalExpr vs)
evalExpr (Compr ac) = liftM ArrayVal (evalCompr ac

evalExpr (Call f as) = getFunction f `ap` mapM evalExpr as
evalExpr (Assign x v) = evalExpr v `tap` putVar

evalExpr (Comma a b) = evalExpr a >> evalExpr b


evalCompr :: ArrayCompr -> SubsM [Value]
evalCompr (ACBody expr) = liftM (:[]) (evalExpr expr)
evalCompr (ACIf c ac) = evalExpr c >>= body
  where body TrueVal = evalCompr ac
        body FalseVal = return []
        body _ = throwError "Non-boolean passed to 'if' in array comprehension"

evalCompr (ACFor i vs ac) = evalExpr vs >>= body
  where body (ArrayVal vs) =
          concatMap ((>> evalCompr ac) . putVar i) vs
        body _ = throwError "Non-array passed to 'for' in array comprehension"
                           
initialPEnv :: PEnv
initialPEnv = Map.fromList [ ("===", compEql)
                           , ("<", compLt) 
                           , ("+", opAdd)
                           , ("*", opMul)
                           , ("-", opSub)
                           , ("%", opMod)
                           , ("Array", mkArray)
                           ]

mkArray :: Primitive
mkArray [IntVal n] | n >= 0 = return $ ArrayVal (replicate n UndefinedVal)
                   | n < 0 = throwError "Value Error: negative length passed to Array()"
mkArray _ = throwError "Argument Error: wrong number of arguments passed to Array()"

compEql :: Primitive
compEql [a, b] = return (a == b) -- (==) compares for structural equality already
compEql _ = throwError "Argument Error: wrong number of arguments passed to ==="

compLt :: Primitive
compLt [a, b] = comp a b
  where comp (IntVal a) (IntVal b) = to (a < b)
        comp (StringVal a) (StringVal b) = to (a < b)
        comp _ _ = throwError "Type Error: mismatched or unsupported types passed to <"

        to True = return TrueVal
        to False = return FalseVal

compLt _ = throwError "Argument Error: wrong number of arguments passed to <"

opAdd :: Primitive
opAdd [a, b] = add a b
  where add (IntVal a) (IntVal b) = return $ IntVal (a + b)
        add (StringVal a) (StringVal b) = return $ StringVal (a ++ b)
        add (ArrayVal a) (ArrayVal b) = return $ ArrayVal (a ++ b)
        
        -- why not xor?
        add TrueVal FalseVal = return TrueVal
        add FalseVal TrueVal = return TrueVal
        add FalseVal FalseVal = return FalseVal
        add TrueVal TrueVal = return FalseVal

        add _ _ = throwError "Type Error : mismatched or unsupported types passed to +"
opAdd _ = throwError "Argument Error: wrong number of arguments passed to +"

opSub :: Primitive
opSub [a, b] = sub a b
  where sub (IntVal a) (IntVal b) = return $ IntVal (a - b)

        -- why not set difference
        sub (ArrayVal a) (ArrayVal b) = return $ ArrayVal (filter (`elem` b) a)
        
        sub _ _ = throwError "Type Error : mismatched or unsupported types passed to -"
opSub _ = throwError "Argument Error: wrong number of arguments passed to -"

opMul :: Primitive
opMul [a, b] = mul a b
  where mul (IntVal a) (IntVal b) = return $ IntVal (a * b)
        
        -- why not and?
        mul TrueVal TrueVal = return TrueVal
        mul TrueVal FalseVal = return FalseVal
        mul FalseVal TrueVal = return FalseVal
        mul FalseVal FalseVal = return FalseVal

        -- why not array extension?
        mul (ArrayVal a) (IntVal b) = return $ ArrayVal (concat $ replicate b a)

        -- why not string extension?
        mul (StringVal a) (IntVal b) = return $ StringVal (concat $ replicate b a)
        
        mul _ _ = throwError "Type Error : mismatched or unsupported types passed to *"

opMul _ = throwError "Argument Error: wrong number of arguments passed to *"

opMod :: Primitive
opMod [a, b] = mod a b
  where mod (IntVal a) (IntVal b) = return $ IntVal (a `rem` b)
        
        -- why not printf?
        mod (StringVal a) (ArrayVal b) = liftM StringVal (myPrintF a b)

        myPrintF ('%':'%':ss) vs = liftM ('%':) (myPrintF ss vs)
        myPrintF ('%':'i':ss) (IntVal i:vs) = liftM (shows i) (myPrintF ss vs)
        myPrintF ('%':'s':ss) (StringVal s:vs) = liftM (shows i) (myPrintF ss vs)
        myPrintF ('%':_) _ = throwError "Format Error: malformed format spec passed to %"
        myPrintF (c:ss) vs = liftM (c:) (myPrintF ss vs)
opMod _ = throwError "Argument Error: wrong number of arguments passed to %"
        
-- missing combinator, inspired by the ruby method of the same name
tap :: Monad m => m a -> (a -> m ()) -> m a
tap m f = do x <- m; f x; return *
