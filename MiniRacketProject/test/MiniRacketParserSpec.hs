{-# OPTIONS_GHC -Wno-unused-imports #-}
module MiniRacketParserSpec where 
    import Test.Hspec
    import Parser
    import Expr 
    import MiniRacketParser
    import Error

    type ParseResult = Either ErrorT (Expr, String)

    expr :: Either ErrorT (a2, b) -> a2
    expr (Right (e, _)) = e 
    expr (Left (SyntaxError msg)) = error msg
    expr (Left (ParseError msg)) = error msg
    expr (Left NoParse) = error "no matching parse"
    expr _ = error "expr in MiniRacketParser.hs is not fully implemented yet..."

    spec :: Spec 
    spec = do 
        describe "parse literals" $ do
            it "parses number: 1235" $ 
                parseStr "1235" `shouldBe` Right (LiteralExpr (IntVal 1235),"")
            it "parses negative numbers: -12235" $
                parseStr "-12235" `shouldBe` Right (LiteralExpr (IntVal (-12235)), "")
            it "parses true" $
                parseStr "true" `shouldBe` Right (LiteralExpr (BoolVal True), "")
            it "parses false" $
                parseStr "false" `shouldBe` Right (LiteralExpr (BoolVal False), "")
        describe "parse bools" $ do
            it "parses bool expr: and true" $
                parseStr "(and true)" `shouldBe` Right (BoolExpr And [LiteralExpr (BoolVal True)], "")
            it "parses bool expr: or true" $
                parseStr "(or true)" `shouldBe` Right (BoolExpr Or [LiteralExpr (BoolVal True)], "")
            it "parses not expr: not true" $
                parseStr "(not true)" `shouldBe` Right (NotExpr (LiteralExpr (BoolVal True)), "")
        describe "parse maths" $ do
            it "parses math expr: (+ 1 1)" $
                parseStr "(+ 1 1)" `shouldBe` Right (MathExpr Add [LiteralExpr (IntVal 1), LiteralExpr (IntVal 1)], "")
            it "parses math expr: (- 1 1)" $
                parseStr "(- 1 1)" `shouldBe` Right (MathExpr Sub [LiteralExpr (IntVal 1), LiteralExpr (IntVal 1)], "")
            it "parses math expr: (* 2 2)" $
                parseStr "(* 2 2)" `shouldBe` Right (MathExpr Mul [LiteralExpr (IntVal 2), LiteralExpr (IntVal 2)], "")
            it "parses math expr: (div 4 2)" $
                parseStr "(div 4 2)" `shouldBe` Right (MathExpr Div [LiteralExpr (IntVal 4), LiteralExpr (IntVal 2)], "")
            it "parses math expr: (mod 1 3)" $
                parseStr "(mod 1 3)" `shouldBe` Right (MathExpr Mod [LiteralExpr (IntVal 1), LiteralExpr (IntVal 3)], "")
        describe "parse comps" $ do
            it "parses comp expr: (equal? 1 1)" $ 
                parseStr "(equal? 1 1)" `shouldBe` Right (CompExpr Eq (LiteralExpr (IntVal 1)) (LiteralExpr (IntVal 1)), "")
            it "parses comp expr: (< 1 2)" $
                parseStr "(< 1 2)" `shouldBe` Right (CompExpr Lt (LiteralExpr (IntVal 1)) (LiteralExpr (IntVal 2)), "")
            it "parses comp expr: (<= 1 1)" $ 
                parseStr "(<= 1 1)" `shouldBe` Right (CompExpr Leq (LiteralExpr (IntVal 1)) (LiteralExpr (IntVal 1)), "")
            it "parses comp expr: (> 2 1)" $
                parseStr "(> 2 1)" `shouldBe` Right (CompExpr Gt (LiteralExpr (IntVal 2)) (LiteralExpr (IntVal 1)), "")
            it "parses comp expr: (>= 1 1)" $ 
                parseStr "(>= 1 1)" `shouldBe` Right (CompExpr Geq (LiteralExpr (IntVal 1)) (LiteralExpr (IntVal 1)), "")
        describe "parse vars" $ do
            it "parses var expr: var " $
                parseStr "var" `shouldBe` Right (VarExpr "var", "")
            it "doesn't parse var expr: and" $
                parseStr "and" `shouldNotBe` Right (VarExpr "and", "")
        describe "parse negated atoms" $ do
            it "parses negatom expr: -var" $
                parseStr "-var" `shouldBe` Right (MathExpr Sub [LiteralExpr (IntVal 0), VarExpr "var"], "")
            it "doesn't parse negatom expr: -true" $
                parseStr "-true" `shouldNotBe` Right (MathExpr Sub [LiteralExpr (IntVal 0), VarExpr "true"], "s")
