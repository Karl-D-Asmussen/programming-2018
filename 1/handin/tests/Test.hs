import Test.Tasty
import Test.Tasty.HUnit

import SubsInterpreter
import SubsAst

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  ([ testCase "intro" $
      runExpr introExpr @?= Right introResult
  , testCase "scope" $
      runExpr scopeExpr @?= Right scopeResult
  ] ++ myTests)

testValues :: TestTree
testValues = testGroup "Values"
  where mkValue (Number i) = IntVal i
        mkValue (String s) = StringVal s
        mkValue TrueConst = TrueVal
        mkValue FalseConst = FalseVal
        mkValue Undefined = UndefinedVal
        mkValue (Array vs) = ArrayVal $ map mkValue vs
        mkValue _ = undefined
        
        numbers = map Number [0..100]
        strings = map String [
                    "rosiness" , "rhododendron" , "Netflix" , "effaced"
                  , "poliomyelitis" , "tutors" , "flagon's" , "moon's"
                  , "bedecks" , "cattiness" , "intellectualized"
                  , "humiliation" , "graphite's" , "Jonah's" , "preliminary's"
                  , "Boru's" , "litterbug" , "caps" , "asbestos" , "clotures"
                  , "concept" , "bidding" , "virile" , "counterfeiter"
                  , "spoofing" , "workhorse" , "frees" , "Buchwald's"
                  , "breakfast's" , "nicks" , "Jesuits" , "geneses"
                  , "hoaxer's" , "bucketful's" , "hypertension's" , "lifestyle"
                  , "anatomist" , "worldly" , "uvulae" , "Nietzsche" , "meddle"
                  , "coverage" , "linkup's" , "Donizetti's" , "Izaak"
                  , "slowdown's" , "weatherproofs" , "sociologist"
                  , "Francisca's" , "parfait's" , "Victorian" , "roundelay's"
                  , "bedrock's" , "artists" , "understated" , "libretto's"
                  , "cob's" , "apprenticeship" , "undeservedly" , "Wise's"
                  , "kinetic" , "nougat" , "cantaloupe" , "generate"
                  , "unsatisfactory" , "corridors" , "backslid" , "birch"
                  , "haze" , "lambasted" , "benevolences" , "fugitives"
                  , "Ir" , "vinegary" , "sooty" , "mills" , "drowses"
                  , "reparations" , "frontiers" , "Roeg" , "smirches"
                  , "gliders" , "snapper" , "thumbtack" , "coil" , "safeguards"
                  , "neckerchief's" , "salver's" , "anointed" , "readmitted"
                  , "rang" , "loam" , "pasturage" , "revoke" , "characterizing"
                  , "loophole" , "plumper" , "pathway" , "crafting"
                  , "cremating" ]
        constants = [TrueConst, FalseConst, Undefined]

        coinduceStep 0 = constants ++ numbers ++ strings
                      ++ [Array numbers, Array strings, Array constants]
        coinduceStep n = let x = coinduceStep (n-1) in x ++ [Array x]
        
        map mkValue coinduceStep 3
myTest (n, a, b) = testCase n $ runExpr a @?= Right b
myTests = map myTest
  [ ("basic", Number 2, IntVal 2)
  , ("basicVar", (Comma (Assign "x" (Number 1)) (Var "x")), IntVal 1)
  , ("compr", (Compr (ACFor "x" (Array [Number 1]) (ACBody (Var "x")))), ArrayVal [IntVal 1])
  , ("stringFmt", (Call "%" [String "Hello %s", Array [String "world"]]), StringVal "Hello world")
  , ("setDiff", (Call "-" [Array [Number 1, Number 2], Array [Number 1]]), ArrayVal [IntVal 2])
  ]

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
  
