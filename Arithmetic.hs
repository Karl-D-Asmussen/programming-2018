-- | Module      : Arithmetic
--   Description : Solution to Assignment 1
--   Copyright   : Karl D. Asmussen © Sep. 2018
--   License     : Public Domain
module Arithmetic ( Exp(..), ArithError(..), VName, showExp, ) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

-- | Expression data type as given
data Exp
   -- | Integer constant
   = Cst Integer
   -- | Addition
   | Add Exp Exp
   -- | Subtraction
   | Sub Exp Exp
   -- | Multiplication
   | Mul Exp Exp
   -- | Division
   | Div Exp Exp
   -- | Raising to power
   | Pow Exp Exp
   -- | Conditional, zero for no
   | If { test, yes, no :: Exp }
   -- | Variable reference
   | Var VName
   -- | Variable specification
   | Let { var :: VName, aux, body :: Exp }
   -- | Capital-sigma summation operator 
   | Sum { var :: VName, from, to, body :: Exp }

-- | Variable name alias
type VName = String

-- | Arithmetic error type as given
data ArithError
   -- | Variable not found
   = EBadVar VName
   -- | Division by zero
   | EDivZero
   -- | Negative power
   | ENegPower
   -- | Other
   | EOther String

-- | Simple rendering for 'ArithError'
showErr (EBadVar n) = "Variable not found: `" ++ n ++ "'"
showErr EDivZero = "Division by zero"
showErr ENegPower = "Negative exponent"
showErr (EOther e) = if null e then "Error" else "Error: " ++ e

-- | Simple rendering for 'Exp'
showExp :: Exp -> String
showExp e = showsExp e ""

  where -- | Using `ShowS` for convenience
        showsExp :: Exp -> ShowS
        showsExp (Cst i) = shows i
        showsExp (Add l r) = parInfixsExp l "+" r
        showsExp (Sub l r) = parInfixsExp l "-" r
        showsExp (Mul l r) = parInfixsExp l "*" r
        showsExp (Div l r) = parInfixsExp l "/" r
        showsExp (Pow l r) = parInfixsExp l "^" r
        showsExp _ = error "unsupported Exp constructor in showExp"
        
        -- | 'ShowS' conbinator for enclosing in parentheses
        parens :: ShowS -> ShowS
        parens s = ('(':) . s . (')':)

        -- | 'ShowS' combinator for infix operators
        infixs :: ShowS -> String -> ShowS -> ShowS
        infixs l o r = l . (' ':) . (o ++) . (' ':) . r

        -- | Utility function converting 'Exp' to 'ShowS'
        infixsExp :: Exp -> String -> Exp -> ShowS
        infixsExp l o r = infixs (showsExp l) o (showsExp r)
        
        -- | Utility function combining 'infixsExp' and 'parens'
        parInfixsExp :: Exp -> String -> Exp -> ShowS
        parInfixsExp l o r = parens (infixsExp l o r)

-- | Simple evaluator for 'Exp'
evalSimple :: Exp -> Integer
evalSimple (Cst i) = i
evalSimple (Add l r) = evalSimple l + evalSimple r
evalSimple (Sub l r) = evalSimple l - evalSimple r
evalSimple (Mul l r) = evalSimple l * evalSimple r
evalSimple (Div l r) = evalSimple l `div` evalSimple r
evalSimple (Pow l r) = evalSimple l ^ evalSimple r
evalSimple _ = error "unsupported Exp constructor in showExp"

-- | Variable environment alias as given
type Env = VName -> Maybe Integer

-- | Empty environment
initEnv :: Env
initEnv = \_ -> Nothing

-- | Utility function for extending environment with single variable
extendEnv :: VName -> Integer -> Env -> Env
extendEnv v i e = \v' -> if v == v' then Just i else e v'

-- | Monadic evaluator
evalM :: Exp -> ReaderT Env (Either ArithError) Integer
evalM (Cst i) = return i
evalM (Add l r) = liftM2 (+) (evalM l) (evalM r)
evalM (Sub l r) = liftM2 (-) (evalM l) (evalM r)
evalM (Mul l r) = liftM2 (*) (evalM l) (evalM r)
evalM (Div l r) = do n <- evalM l
                     d <- evalM r
                     if 0 == d
                       then throwError EDivZero
                       else return (n `div` d)
evalM (Pow l r) = do b <- evalM l
                     e <- evalM r
                     if 0 > e
                       then throwError ENegPower
                       else return (b ^ e)
evalM (If c y n) = do c' <- evalM c
                      if 0 /= c'
                        then evalM y
                        else evalM n
evalM (Var n) = do x <- reader ($ n)
                   case x of
                        Just i -> return i
                        Nothing -> throwError (EBadVar n)
evalM (Let n a b) = do a' <- evalM a
                       local (extendEnv n a') (evalM b)
evalM (Sum n f t b) = do f' <- evalM f
                         t' <- evalM t
                         xs <- forM [f' .. t'] $ \i -> local (extendEnv n i) (evalM b)
                         return (sum xs)

-- | Evaluate 'Exp' using variables
evalFull :: Exp -> Env -> Integer
evalFull exp env = case evalErr exp env of
                        Left err -> error (showErr err)
                        Right res -> res

-- | Evaluate 'Exp' in an exception-safe manner
evalErr :: Exp -> Env -> Either ArithError Integer
evalErr exp env = runReaderT (evalM exp) env

