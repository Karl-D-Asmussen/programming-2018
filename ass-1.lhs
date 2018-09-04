
data Exp
  = Cst Integer
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Pow Exp Exp
  | If { test, yes, no :: Exp }
  | Var VName
  | Let { var :: VName, aux, body :: Expr }
  | Sum { var :: VName, from, to, body :: Expr }

type VName = String

showExp :: Exp -> String
showExp e = showsExp e ""

  where showsExp :: Exp -> ShowS
        showsExp (Cst i) = shows i
        showsExp (Add l r) = parInfixsExp l "+" r
        showsExp (Sub l r) = parInfixsExp l "-" r
        showsExp (Mul l r) = parInfixsExp l "*" r
        showsExp (Div l r) = parInfixsExp l "/" r
        showsExp (Pow l r) = parInfixsExp l "^" r
        showsExp _ = error "unsupported Exp constructor in showExp"

        parens :: ShowS -> ShowS
        parens s = ('(':) . s . (')':)

        infixs :: ShowS -> String -> ShowS -> Shows
        infixs l o r = l . (' ':) . (o ++) . (' ':) . r

        infixsExp :: Exp -> String -> Exp -> Shows
        infixsExp l o r = infixs (showsExp l) o (showsExp r)

        parInfixsExp :: Exp -> String -> Expr -> Shows
        parInfixsExp l o r = parens (infixsExp l o r)


