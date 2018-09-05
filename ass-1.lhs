
Full definition of the `data Exp` type:

```haskell

> data Exp
>   = Cst Integer
>   | Add Exp Exp
>   | Sub Exp Exp
>   | Mul Exp Exp
>   | Div Exp Exp
>   | Pow Exp Exp
>   | If { test, yes, no :: Exp }
>   | Var VName
>   | Let { var :: VName, aux, body :: Expr }
>   | Sum { var :: VName, from, to, body :: Expr }

> type VName = String

```

Implementation of `showExp :: Exp -> String` via `ShowS`:

```haskell

> showExp :: Exp -> String
> showExp e = showsExp e ""
> 
>   where showsExp :: Exp -> ShowS
>         showsExp (Cst i) = shows i
>         showsExp (Add l r) = parInfixsExp l "+" r
>         showsExp (Sub l r) = parInfixsExp l "-" r
>         showsExp (Mul l r) = parInfixsExp l "*" r
>         showsExp (Div l r) = parInfixsExp l "/" r
>         showsExp (Pow l r) = parInfixsExp l "^" r
>         showsExp _ = error "unsupported Exp constructor in showExp"
> 
>         parens :: ShowS -> ShowS
>         parens s = ('(':) . s . (')':)
> 
>         infixs :: ShowS -> String -> ShowS -> Shows
>         infixs l o r = l . (' ':) . (o ++) . (' ':) . r
> 
>         infixsExp :: Exp -> String -> Exp -> Shows
>         infixsExp l o r = infixs (showsExp l) o (showsExp r)
> 
>         parInfixsExp :: Exp -> String -> Expr -> Shows
>         parInfixsExp l o r = parens (infixsExp l o r)

```

```haskell

> evalSimple :: Exp -> Integer
> evalSimple (Cst i) = i
> evalSimple (Add l r) = evalSimple l + evalSimple r
> evalSimple (Sub l r) = evalSimple l - evalSimple r
> evalSimple (Mul l r) = evalSimple l * evalSimple r
> evalSimple (Div l r) = evalSimple l `div` evalSimple r
> evalSimple (Pow l r) = evalSimple l ^ evalSimple r
> evalSimple _ = error "unsupported Exp constructor in showExp"

```

```haskell

> type Env = VName -> Maybe Integer
> initEnv :: Env
> initEnv = \_ -> Nothing
>
> extendEnv :: VName -> Integer -> Env -> Env
> extendEnv v i e = \v' -> if v == v' then Just i else e v'

```

```haskell

> evalFull :: Exp -> Env -> Integer
> evalFull (Cst i) env = i
> evalFull (Add l r) env = evalFull l env + evalFull r env
> evalFull (Sub l r) env = evalFull l env - evalFull r env
> evalFull (Mul l r) env = evalFull l env * evalFull r env
> evalFull (Div l r) env = evalFull l env `div` evalFull r env
> evalFull (Pow l r) env = evalFull l env ^ evalFull r env
> evalFull (If c y n) env = if 0 /= evalFull c env then evalFull y env else evalFull n env
> evalFull (Var v) env = case env v of Just i -> i; Nothing -> error ("undefined variable `" ++ v ++ "' in Exp")
> evalFull (Let v a b) env = let x = evalFull a env in evalFull b (extendEnv v x env)
> evalFull (Sum v l h b) env = let x = evalFull l env; y = evalFull h env in sum $ [ evalFull b (extendEnv v i) | i <- [x..y] ]

```

```haskell

> data ArithError
>    = EBadVar VName
>    | EDivZero
>    | ENegPower
>    | EOther String
>
> evalErr :: Exp -> Env -> Either ArithError Integer

```
