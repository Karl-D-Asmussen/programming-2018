import Test.Tasty
import Test.Tasty.HUnit

import System.Random
import Control.Monad

import SubsInterpreter
import SubsAst

main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "All Tests" [handoutTests, basicTests, negativeTests]

handoutTests :: TestTree
handoutTests = testGroup "Handout Tests"
  [ testCase "intro" $
      runExpr introExpr @?= Right introResult
  , testCase "scope" $
      runExpr scopeExpr @?= Right scopeResult
  ]

basicTests = testGroup "Basic Tests"
  [ testCase "Numbers" $
      forM_ [1..100] $
         const $ do i <- getStdRandom (randomR (0, 1000))
                    runExpr (Number i) @?= Right (IntVal i)
  , testCase "Basic Variables" $
      forM_ [1..100] $
         const $ do nm <- sequence $ replicateM 3 getStdRandom (randomR ('a', 'z'))
                    runExpr (Comma (Assign nm (String nm)) (Var nm)) @?= Right (StringVal nm)
  ] 

negativeTests = testGroup "Negative Tests"
  [ testCase "DivZero" $ 
    leftFst (runExpr (Call "%" [Number 1, Number 0])) @?= Just EValue
  ]
  where leftFst (Left (a, _)) = Just a
        leftFst _ = Nothing

introExpr :: Expr
introExpr =
  Comma (Assign "xs"
          (Array [Number 0, Number 1, Number 2, Number 3, Number 4,
                  Number 5, Number 6, Number 7, Number 8, Number 9]))
   (Comma (Assign "squares"
            (Compr (ACFor "x" (Var "xs")
                     (ACBody (Call "*" [Var "x",Var "x"])))))
     (Comma (Assign "evens"
              (Compr (ACFor "x" (Var "xs")
                       (ACIf (Call "===" [Call "%" [Var "x", Number 2],
                                          Number 0])
                         (ACBody (Var "x"))))))
       (Comma (Assign "many_a"
                (Compr (ACFor "x" (Var "xs")
                         (ACFor "y" (Var "xs")
                           (ACBody (String "a"))))))
         (Comma (Assign "hundred"
                  (Compr (ACFor "i" (Array [Number 0])
                           (ACFor "x" (Call "Array" [Number 5])
                             (ACFor "y" (Call "Array" [Number 20])
                               (ACBody (Assign "i"
                                         (Call "+" [Var "i", Number 1]))))))))
           (Array [Var "xs", Var "squares", Var "evens",
                   Var "many_a", Var "hundred"])))))

introResult :: Value
introResult = ArrayVal
  [ ArrayVal [IntVal n | n <- [0..9]]
  , ArrayVal [IntVal (n * n) | n <- [0..9]]
  , ArrayVal $ map IntVal $ [0,2..8]
  , ArrayVal (replicate 100 $ StringVal "a")
  , ArrayVal $ map IntVal $ [1 .. 100]
  ]

scopeExpr :: Expr
scopeExpr =
  Comma (Assign "x" (Number 42))
   (Comma (Assign "y" (Compr (ACFor "x" (String "abc")
                               (ACBody (Var "x")))))
     (Array [Var "x", Var "y"]))

scopeResult :: Value
scopeResult = ArrayVal [IntVal 42, ArrayVal $ map (StringVal . (:"")) "abc"]
  
