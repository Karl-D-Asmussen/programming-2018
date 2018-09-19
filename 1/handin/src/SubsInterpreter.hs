{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
module SubsInterpreter
       ( Value(..)
       , runExpr

       -- to test
       , SubsM(runSubsM)
       , modifyEnv 
       , putVar 
       , dropVar
       , isVar
       , getVar 
       , getFunction 
       , evalExpr 
       , evalCompr
       
       , Error
       , ErrorKind (..)
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

-- | A value is either an integer, the special constant undefined,
--   true, false, a string, or an array of values.
-- Expressions are evaluated to values.
data Value = IntVal Int
           | UndefinedVal
           | TrueVal | FalseVal
           | StringVal String
           | ArrayVal [Value]
           deriving (Eq, Show)

-- Error type with human readable reason and machine readable reason
type Error = (ErrorKind, Reason)

-- Machine readable error reason
data ErrorKind = EName
               | EType   
               | EArgument
               | EValue
               | EOther
               deriving (Eq, Show)

-- human readable reason
type Reason = String

-- as given
type Env = Map Ident Value
type PEnv = Map FunName Primitive
type Context = (Env, PEnv)

-- altered to allow full range of interpreter interaciton in primitives
type Primitive = [Value] -> SubsM Value

-- as given
initialContext :: Context
initialContext = (Map.empty, initialPEnv)

-- This is almost an RWS.
newtype SubsM a = SubsM {runSubsM :: Context -> Either Error (a, Env)}

-- Straightforward implementation of Reader-State-Error construction
instance Monad SubsM where
  return x = SubsM $ \(env, _) -> Right (x, env)
  m >>= f = SubsM $ \ctx@(env, penv) -> runSubsM m ctx >>= \(x, env') -> runSubsM (f x) (env', penv)

  -- fail should be deprecated in favor of MonadFail
  fail s = SubsM $ const (Left (EOther, s))

-- State instance
instance MonadState Env SubsM where
  get = SubsM $ \(env, _) -> return (env, env)
  put env = SubsM $ \(_, _) -> return ((), env)

-- Reader instance
instance MonadReader PEnv SubsM where
  reader f = SubsM $ \(env, penv) -> Right (f penv, env)
  local f m = SubsM $ \(env, penv) -> runSubsM m (env, f penv)

-- Error instance, delegating to Either
instance MonadError Error SubsM where
  throwError e = SubsM $ \_ -> Left e
  catchError m h = SubsM $ \ctx -> runSubsM m ctx `catchError` \err -> runSubsM (h err) ctx

-- as given
instance Functor SubsM where
  fmap = liftM
instance Applicative SubsM where
  pure = return
  (<*>) = ap

-- trivial, part of C.M.State.Class
modifyEnv :: (Env -> Env) -> SubsM ()
modifyEnv = modify
-- modifyEnv f = SubsM $ \(env, penv) -> Right ((), f env)

-- changed to return assigned value
putVar :: Ident -> Value -> SubsM Value
putVar i v = modifyEnv (Map.insert i v) >> return v

-- pops value from environment, undefining variable
dropVar :: Ident -> SubsM Value
dropVar i = getVar i <* modifyEnv (Map.delete i)

-- retrieves value of variable from state if it exists
getVar :: Ident -> SubsM Value
getVar i = maybe (throwError (EName, "variable not in scope " ++ i)) return =<< gets (Map.lookup i)
-- use `maybe` instead of pattern matching

isVar :: Ident -> SubsM Bool
isVar i = gets (Map.member i)

-- retrieves function from reader configuration
getFunction :: FunName -> SubsM Primitive
getFunction f = maybe (throwError (EName, "function not found " ++ f)) return =<< reader (Map.lookup f)

-- runs with default config/state
runExpr :: Expr -> Either Error Value
-- arrow combinator for terseness, apply id to Left's, fst to Right's
runExpr expr = (id +++ fst) $ runSubsM (evalExpr expr) initialContext

-- main evaluator
evalExpr :: Expr -> SubsM Value

-- trivial cases
evalExpr Undefined = return UndefinedVal
evalExpr TrueConst = return TrueVal
evalExpr FalseConst = return FalseVal

-- almost trivial cases
evalExpr (Number i) = return (IntVal i)
evalExpr (String s) = return (StringVal s)

-- mapM for recursion
evalExpr (Array vs) = liftM ArrayVal (mapM evalExpr vs)

-- delegate to subfunction
evalExpr (Compr ac) = liftM ArrayVal (evalCompr ac)

-- hack with join to keep it one-lined
evalExpr (Call f as) = join (getFunction f `ap` mapM evalExpr as)

-- direct translation
evalExpr (Assign x v) = putVar x =<< evalExpr v
evalExpr (Var x) = getVar x
evalExpr (Comma a b) = evalExpr a >> evalExpr b

-- dedicated subfunction evaluator of array comprehensions, separate for testing
evalCompr :: ArrayCompr -> SubsM [Value]

-- base case
evalCompr (ACBody expr) = liftM (:[]) (evalExpr expr)

-- conditional, identicaly behavior to `guard` from MonadPlus
evalCompr (ACIf c ac) = body =<< evalExpr c
  where -- subfunction for pattern matching
        body TrueVal = evalCompr ac
        body FalseVal = return []
        body _ = throwError (EType, "non-boolean passed to 'if' in array comprehension")

-- looping construct
evalCompr (ACFor i vs ac) = ifM (isVar i) (run shadow) (run noShadow)
  where -- handle protection of extant variables
        shadow = save (getVar i) (void . putVar i)
        noShadow = save (return ()) (const $ void $ dropVar i) 

        run x = x . body =<< evalExpr vs
        
        -- subfunction for pattern matching
        body (ArrayVal vs) = concat `liftM` mapM ((>> evalCompr ac) . putVar i) vs
        body (StringVal vs) = concat `liftM` mapM ((>> evalCompr ac) . putVar i . StringVal . (:"")) vs
        body _ = throwError (EType, "non-array passed to 'for' in array comprehension")


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
                   | n < 0 = throwError (EValue, "negative length passed to Array()")
mkArray _ = throwError (EArgument, "wrong number of arguments passed to Array()")

compEql :: Primitive
compEql [a, b] | a == b = return TrueVal -- (==) compares for structural equality already
               | otherwise = return FalseVal
compEql _ = throwError (EArgument, "wrong number of arguments passed to ===")

-- straightfoward implementation
compLt :: Primitive
compLt [a, b] = comp a b
  where -- some issues with type checker necessitate explicit signature
        comp :: Value -> Value -> SubsM Value
        comp (IntVal a) (IntVal b) = to (a < b)
        comp (StringVal a) (StringVal b) = to (a < b)
        comp _ _ = throwError (EType, "mismatched or unsupported types passed to <")
        
        -- gold standard for helper functions, right here
        to True = return TrueVal
        to False = return FalseVal

compLt _ = throwError (EArgument, "wrong number of arguments passed to <")

-- nothing interesting here
opAdd :: Primitive
opAdd [a, b] = add a b
  where add :: Value -> Value -> SubsM Value
        -- direct translation
        add (IntVal a) (IntVal b) = return $ IntVal (a + b)
        add (StringVal a) (StringVal b) = return $ StringVal (a ++ b)
        add (StringVal a) (IntVal b) = return $ StringVal (a ++ show b)
        add (IntVal a) (StringVal b) = return $ StringVal (show a ++ b)
        
        -- why not array concat
        add (ArrayVal a) (ArrayVal b) = return $ ArrayVal (a ++ b)
        
        -- why not xor?
        add TrueVal FalseVal = return TrueVal
        add FalseVal TrueVal = return TrueVal
        add FalseVal FalseVal = return FalseVal
        add TrueVal TrueVal = return FalseVal

        add _ _ = throwError (EType, "mismatched or unsupported types passed to +")
opAdd _ = throwError (EArgument, "wrong number of arguments passed to +")

opSub :: Primitive
opSub [a, b] = sub a b
  where sub :: Value -> Value -> SubsM Value
        sub (IntVal a) (IntVal b) = return $ IntVal (a - b)

        -- why not set difference
        sub (ArrayVal a) (ArrayVal b) = return $ ArrayVal (filter (`notElem` b) a)
        
        sub _ _ = throwError (EType, "mismatched or unsupported types passed to -")
opSub _ = throwError (EArgument, "wrong number of arguments passed to -")

opMul :: Primitive
opMul [a, b] = mul a b
  where mul :: Value -> Value -> SubsM Value
        mul (IntVal a) (IntVal b) = return $ IntVal (a * b)
        
        -- why not and?
        mul TrueVal TrueVal = return TrueVal
        mul TrueVal FalseVal = return FalseVal
        mul FalseVal TrueVal = return FalseVal
        mul FalseVal FalseVal = return FalseVal

        -- why not array extension?
        mul (ArrayVal a) (IntVal b) = return $ ArrayVal (concat $ replicate b a)

        -- why not string extension?
        mul (StringVal a) (IntVal b) = return $ StringVal (concat $ replicate b a)
        
        mul _ _ = throwError (EType, "mismatched or unsupported types passed to *")

opMul _ = throwError (EArgument, "wrong number of arguments passed to *")

opMod :: Primitive
opMod [a, b] = mod a b
  where mod (IntVal _) (IntVal 0) = throwError (EValue, "division by zero in %")
        mod (IntVal a) (IntVal b) = return $ IntVal (a `rem` b)
        
        -- why not printf?
        mod (StringVal a) (ArrayVal b) = liftM StringVal (myPrintF a b)

        mod _ _ = throwError (EType, "mismatched or unsupported types passed to %")

        myPrintF :: String -> [Value] -> SubsM String
        myPrintF ('%':'%':ss) vs = liftM ('%':) (myPrintF ss vs)
        myPrintF ('%':'i':ss) (IntVal i:vs) = liftM (shows i) (myPrintF ss vs)
        myPrintF ('%':'s':ss) (StringVal s:vs) = liftM (s ++) (myPrintF ss vs)
        myPrintF ('%':_) _ = throwError (EValue, "malformed format spec passed to %")
        myPrintF (c:ss) vs = liftM (c:) (myPrintF ss vs)
        myPrintF "" [] = return ""
        myPrintF "" _ = throwError (EValue, "malformed format spec passed to %")

opMod _ = throwError (EArgument, "wrong number of arguments passed to %")

-- missing combinator, for saving and restoring e.g. parts of state
save :: Monad m => m a -> (a -> m ()) -> m b -> m b
save ma am mb = do a <- ma; b <- mb; am a; return b

-- monadic if
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM c t e = c >>= \c' -> if c' then t else e
