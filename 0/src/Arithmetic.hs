{-# OPTIONS_GHC -W #-}
{-# LANGUAGE FlexibleContexts #-}

module Arithmetic
  (
  showExp,
  evalSimple,
  extendEnv,
  evalFull,
  evalErr,
  showCompact,
  evalEager,
  evalLazy
  )

where

import Definitions
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader

showExp :: Exp -> String
showExp e = showsExp e ""
  where -- Using `ShowS` for convenience
        showsExp :: Exp -> ShowS
        showsExp (Cst i)
          | i < 0 = parens (shows i)
          | otherwise = shows i
        showsExp (Add l r) = parInfixsExp l "+" r
        showsExp (Sub l r) = parInfixsExp l "-" r
        showsExp (Mul l r) = parInfixsExp l "*" r
        showsExp (Div l r) = parInfixsExp l "/" r
        showsExp (Pow l r) = parInfixsExp l "^" r
        showsExp _ = error "unsupported Exp constructor in showExp"

        -- 'ShowS' conbinator for enclosing in parentheses
        parens :: ShowS -> ShowS
        parens s = ('(':) . s . (')':)

        -- 'ShowS' combinator for infix operators
        infixs :: ShowS -> String -> ShowS -> ShowS
        infixs l o r = l . (o ++) . r

        -- Utility function converting 'Exp' to 'ShowS'
        infixsExp :: Exp -> String -> Exp -> ShowS
        infixsExp l o r = infixs (showsExp l) o (showsExp r)

        -- Utility function combining 'infixsExp' and 'parens'
        parInfixsExp :: Exp -> String -> Exp -> ShowS
        parInfixsExp l o r = parens (infixsExp l o r)


evalSimple :: Exp -> Integer
evalSimple e = evalFull e (error "unsupported Exp constructor in showExp")

extendEnv :: VName -> Integer -> Env -> Env
extendEnv v i e = e `seq` \v' -> if v == v' then Just i else e v'

evalFull :: Exp -> Env -> Integer
evalFull exp env = case evalErr exp env of
                        Left err -> error (showErr err)
                        Right res -> res

evalErr :: Exp -> Env -> Either ArithError Integer
evalErr = evalEager

-- optional parts (if not attempted, leave them unmodified)

showCompact :: Exp -> String
showCompact exp = showsCompact exp 0 ""
  where showsCompact :: Exp -> Integer -> ShowS
        showsCompact (Cst i) prec
          | i < 0, prec > 10 = ('(':) . shows i . (')':)
          | otherwise = shows i

        showsCompact Let {} _ = undefined
        showsCompact (Var _) _ = undefined
        showsCompact Sum {} _ = undefined

        showsCompact exp prec = pars main
          where (le, re) = leftRight exp
                (lp, rp) = precedence exp
                ls = showsCompact le lp
                rs = showsCompact re rp
                lrp = min lp rp
                main = (ls . operator exp . rs)
                pars | prec > lrp = parens
                     | otherwise = id


        leftRight :: Exp -> (Exp, Exp)
        leftRight (Add l r) = (l, r)
        leftRight (Sub l r) = (l, r)
        leftRight (Mul l r) = (l, r)
        leftRight (Div l r) = (l, r)
        leftRight (Pow l r) = (l, r)
        leftRight _ = undefined

        precedence :: Exp -> (Integer, Integer)
        precedence (Cst i) | i < 0 = (10, 10)
                           | otherwise = (100, 100)
        precedence (Add _ _) = (10, 11)
        precedence (Sub _ _) = (10, 11)
        precedence (Mul _ _) = (30, 31)
        precedence (Div _ _) = (20, 21)
        precedence (Pow _ _) = (41, 40)
        precedence _ = undefined

        parens :: ShowS -> ShowS
        parens s = ('(':) . s . (')':)

        operator :: Exp -> ShowS
        operator (Add _ _) = ('+':)
        operator (Sub _ _) = ('-':)
        operator (Mul _ _) = ('*':)
        operator (Div _ _) = ('/':)
        operator (Pow _ _) = ('^':)
        operator _ = undefined

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = runReaderT . evalM

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy (Cst i) _env = return i
evalLazy (Add l r) env = liftM2 (+) (evalLazy l env) (evalLazy r env)
evalLazy (Sub l r) env = liftM2 (-) (evalLazy l env) (evalLazy r env)
evalLazy (Mul l r) env = liftM2 (*) (evalLazy l env) (evalLazy r env)
evalLazy (Div l r) env = do l' <- evalLazy l env
                            r' <- evalLazy r env
                            if 0 == r'
                               then throwError EDivZero
                               else return (l' `div` r')
evalLazy (Pow l r) env = do l' <- evalLazy l env
                            r' <- evalLazy r env
                            if r' < 0
                               then throwError ENegPower
                               else return (l' ^ r')
evalLazy (If c t f) env = do c' <- evalLazy c env
                             if 0 /= c'
                                then evalLazy t env
                                else evalLazy f env
evalLazy (Let n a b) env = do -- a' <- evalLazy a env
                              evalLazy b (extendEnv n (evalFull a env) env)
evalLazy (Var n) env = case env n of
                            Just i -> return i
                            Nothing -> throwError (EBadVar n)
evalLazy (Sum n f t b) env = do f' <- evalLazy f env
                                t' <- evalLazy t env
                                xs <- forM [f' .. t'] $ \i ->
                                      evalLazy b (extendEnv n i env)
                                return (sum xs)

-- utility functions

-- Simple rendering for 'ArithError'
showErr :: ArithError -> String
showErr (EBadVar n) = "Variable not found: `" ++ n ++ "'"
showErr EDivZero = "Division by zero"
showErr ENegPower = "Negative exponent"
showErr (EOther e) = if null e then "Error" else "Error: " ++ e

-- Monadic evaluator
evalM :: (MonadError ArithError m, MonadReader Env m) => Exp -> m Integer
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
                         xs <- forM [f' .. t'] $ \i ->
                               local (extendEnv n i) (evalM b)
                         return (sum xs)
