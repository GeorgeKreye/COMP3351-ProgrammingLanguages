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
        describe "eval expressions" $ do
            it "evaluates number: 1235" $ 
                evalStr "1235" `shouldBe` Right (IntVal 1235)
            it "evaluates negative numbers: -12235" $
                evalStr "-12235" `shouldBe` Right (IntVal (-12235))
            it "evaluates true" $
                evalStr "true" `shouldBe` Right (BoolVal True)
            it "evaluates false" $
                evalStr "false" `shouldBe` Right (BoolVal False)
            it "evaluates (and true true) = true" $
                evalStr "(and true true)" `shouldBe` Right (BoolVal True)
            it "evaluates (* 2 2) = 4" $
                evalStr "(* 2 2)" `shouldBe` Right (IntVal 4)
