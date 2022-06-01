{-# OPTIONS_GHC -Wno-unused-imports #-}
module EvalSpec where
    import Test.Hspec
    import Parser
    import Expr
    import MiniRacketParser

    import Eval
    import Error

    type ParseResult = Either ErrorT (Expr, String)

    spec :: Spec
    spec = do
        describe "eval literal expressions" $ do
            it "evaluates number: 1235" $ 
                evalStr "1235" `shouldBe` Right (IntVal 1235)
            it "evaluates negative numbers: -12235" $
                evalStr "-12235" `shouldBe` Right (IntVal (-12235))
            it "evaluates true" $
                evalStr "true" `shouldBe` Right (BoolVal True)
            it "evaluates false" $
                evalStr "false" `shouldBe` Right (BoolVal False)
        describe "eval bool expressions" $ do
            it "evaluates (and true true) = true" $
                evalStr "(and true true)" `shouldBe` Right (BoolVal True)
            it "evaluates (or false true) = true" $
                evalStr "(or false true)" `shouldBe` Right (BoolVal True)
        describe "eval not expressions" $ do -- evals currently fail
            it "evaluates (not true) = false" $
                evalStr "(not true)" `shouldBe` Right (BoolVal False)
            it "evaluates (not false) = true" $
                evalStr "(not false)" `shouldBe` Right (BoolVal True)
        describe "eval math expressions" $ do
            it "evaluates (+ 1 1) = 2" $
                evalStr "(+ 1 1)" `shouldBe` Right (IntVal 2)
            it "evaluates (- 1 1) = 0" $
                evalStr "(- 1 1)" `shouldBe` Right (IntVal 0)
            it "evaluates (* 2 2) = 4" $
                evalStr "(* 2 2)" `shouldBe` Right (IntVal 4)
            it "evaluates (div 4 2) = 2" $
                evalStr "(div 4 2)" `shouldBe` Right (IntVal 2)
        describe "eval comp expressions" $ do
            it "evaluates (equal? 1 1) = true" $ 
                evalStr "(equal? 1 1)" `shouldBe` Right (BoolVal True)
            it "evaluates (< 1 2) = true" $
                evalStr "(< 1 2)" `shouldBe` Right (BoolVal True)
            it "evaluates (<= 1 1) = true" $ 
                evalStr "(<= 1 1)" `shouldBe` Right (BoolVal True)
            it "evaluates (<= 1 2) = true" $ 
                evalStr "(<= 1 2)" `shouldBe` Right (BoolVal True)
            it "evaluates (> 2 1) = true" $
                evalStr "(> 2 1)" `shouldBe` Right (BoolVal True)
            it "evaluates (>= 1 1) = true" $ 
                evalStr "(>= 1 1)" `shouldBe` Right (BoolVal True)
            it "evaluates (>= 2 1) = true" $ 
                evalStr "(>= 2 1)" `shouldBe` Right (BoolVal True)
        describe "eval var expressions" $ do
            it "evaluates var = 1" $
                parseAndEvalEnv [("var", IntVal 1)] "var" `shouldBe` Right (IntVal 1,([("var",IntVal 1)], EmptyExpr))
            it "does not evaluate and = 1" $
                parseAndEvalEnv [("and", IntVal 1)] "and" `shouldNotBe` Right (IntVal 1,([("and", IntVal 1)],EmptyExpr))
        describe "eval if expressions" $ do
            it "evaluates (if true 1 0) = 1" $
                evalStr "(if true 1 0)" `shouldBe` Right (IntVal 1)
            it "evaluates (if false 1 0) = 0" $
                evalStr "(if false 1 0)" `shouldBe` Right (IntVal 0)
        describe "callFun" $
            it "evaluates (lambda (x) (+ x 1)), x := 1 = 2" $
                callFun (ClosureVal "" "x" (MathExpr Add [VarExpr "x",LiteralExpr (IntVal 1)]) []) (IntVal 1) `shouldBe` Right (IntVal 2)
