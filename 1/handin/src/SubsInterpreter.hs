{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
module SubsInterpreter
       ( Value(..)
       , runExpr

       -- to test
       , SubsM(runSubsM)
       , modifyEnv 
       , putVar 
       , dropVar
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
import Data.List
import Data.Maybe

-- | A value is either an integer, the special constant undefined,
--   true, false, a string, or an array of values.
-- Expressions are evaluated to values.
data Value = IntVal Int
           | UndefinedVal
           | TrueVal | FalseVal
           | StringVal String
           | ArrayVal [Value]
           deriving (Eq, Show)

type Error = (ErrorKind, String)
data ErrorKind = EName
               | EType   
               | EArgument
               | EValue
               | EOther
               deriving (Eq, Show)

type Reason = String
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
  m >>= f = SubsM $ \ctx@(env, penv) -> runSubsM m ctx >>= \(x, env') -> runSubsM (f x) (env', penv)

  -- fail should be deprecated in favor of MonadFail
  fail s = SubsM $ const (Left (EOther, s))

instance MonadState Env SubsM where
  get = SubsM $ \(env, _) -> return (env, env)
  put env = SubsM $ \(_, _) -> return ((), env)

instance MonadReader PEnv SubsM where
  reader f = SubsM $ \(env, penv) -> Right (f penv, env)
  local f m = SubsM $ \(env, penv) -> runSubsM m (env, f penv)

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
-- modifyEnv f = SubsM $ \(env, penv) -> Right ((), f env)

putVar :: Ident -> Value -> SubsM Value
putVar i v = modifyEnv (Map.insert i v) >> return v

dropVar :: Ident -> SubsM Value
dropVar i = getVar i <* modifyEnv (Map.delete i)

getVar :: Ident -> SubsM Value
getVar i = maybe (throwError (EName, "variable not in scope " ++ i)) return =<< gets (Map.lookup i)

isVar :: Ident -> SubsM Bool
isVar i = gets (Map.member i)

getFunction :: FunName -> SubsM Primitive
getFunction f = do i' <- reader (Map.lookup f)
                   case i' of
                        Just x -> return x
                        Nothing -> throwError (EName, "function not found " ++ f)

runExpr :: Expr -> Either Error Value
runExpr expr = (id +++ fst) $ runSubsM (evalExpr expr) initialContext

evalExpr :: Expr -> SubsM Value
evalExpr Undefined = return UndefinedVal
evalExpr TrueConst = return TrueVal
evalExpr FalseConst = return FalseVal

evalExpr (Number i) = return (IntVal i)
evalExpr (String s) = return (StringVal s)

evalExpr (Array vs) = liftM ArrayVal (mapM evalExpr vs)
evalExpr (Compr ac) = liftM ArrayVal (evalCompr ac)

evalExpr (Call f as) = join (getFunction f `ap` mapM evalExpr as)
evalExpr (Assign x v) = putVar x =<< evalExpr v
evalExpr (Var x) = getVar x

evalExpr (Comma a b) = evalExpr a >> evalExpr b


evalCompr :: ArrayCompr -> SubsM [Value]

evalCompr (ACBody expr) = liftM (:[]) (evalExpr expr)
evalCompr (ACIf c ac) = evalExpr c >>= body
  where body TrueVal = evalCompr ac
        body FalseVal = return []
        body _ = throwError (EType, "non-boolean passed to 'if' in array comprehension")

evalCompr (ACFor i vs ac) = ifM (isVar i) (run shadow) (run noShadow)
  where shadow = save (getVar i) (void . putVar i)
        noShadow = save (return ()) (const $ void $ dropVar i) 

        run x = x . body =<< evalExpr vs

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

compLt :: Primitive
compLt [a, b] = comp a b
  where comp :: Value -> Value -> SubsM Value
        comp (IntVal a) (IntVal b) = to (a < b)
        comp (StringVal a) (StringVal b) = to (a < b)
        comp _ _ = throwError (EType, "mismatched or unsupported types passed to <")

        to True = return TrueVal
        to False = return FalseVal

compLt _ = throwError (EArgument, "wrong number of arguments passed to <")

opAdd :: Primitive
opAdd [a, b] = add a b
  where add :: Value -> Value -> SubsM Value
        add (IntVal a) (IntVal b) = return $ IntVal (a + b)
        add (StringVal a) (StringVal b) = return $ StringVal (a ++ b)
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
        myPrintF :: String -> [Value] -> SubsM String
        myPrintF ('%':'%':ss) vs = liftM ('%':) (myPrintF ss vs)
        myPrintF ('%':'i':ss) (IntVal i:vs) = liftM (shows i) (myPrintF ss vs)
        myPrintF ('%':'s':ss) (StringVal s:vs) = liftM (s ++) (myPrintF ss vs)
        myPrintF ('%':_) _ = throwError (EValue, "malformed format spec passed to %")
        myPrintF (c:ss) vs = liftM (c:) (myPrintF ss vs)
        myPrintF "" [] = return ""
        myPrintF "" _ = throwError (EValue, "malformed format spec passed to %")

opMod _ = throwError (EArgument, "wrong number of arguments passed to %")
        
-- missing combinator, inspired by the ruby method of the same name
tap :: Monad m => m a -> (a -> m ()) -> m a
tap m f = do x <- m; f x; return x

-- missing combinator, for saving and restoring e.g. parts of state
save :: Monad m => m a -> (a -> m ()) -> m b -> m b
save ma am mb = do a <- ma; b <- mb; am a; return b

-- monadic version of when
whenM :: Monad m => m Bool -> m () -> m ()
whenM c m = c >>= \c' -> if c' then m else return ()

-- monadic if
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM c t e = c >>= \c' -> if c' then t else e
