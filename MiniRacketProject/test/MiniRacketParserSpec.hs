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
        describe "parse ops" $ do
            it "parses bool expr: and true" $
                parseStr "(and true)" `shouldBe` Right (BoolExpr And [LiteralExpr (BoolVal True)],"")
            it "parses bool expr: or true" $
                parseStr "(or true)" `shouldBe` Right (BoolExpr Or [LiteralExpr (BoolVal True)], "")
