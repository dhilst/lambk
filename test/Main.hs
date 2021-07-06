module Main where

import CEK
import Lamb
import Parser
import Test.Hspec

consumeAllInputTest :: IO ()
consumeAllInputTest =
  hspec $ do
    describe "Parses all input" $ do
      it "(λx.x)" $ do parse "(λx.x)" `shouldBe` Lamb "x" (Var "x")

parserTest :: IO ()
parserTest =
  hspec $ do
    describe "Parses" $ do
      it "Should parse char" $ do
        let parseC = parseChar 'c'
         in runParser parseC "cx" `shouldBe` Just ("x", 'c')
      it "Should parse lambda term" $ do
        let result = show . parse $ "(λx.x)"
         in result `shouldBe` "(λx.x)"
      it "Should parse var term" $ do
        let result = show . parse $ "x"
         in result `shouldBe` "x"
      it "Should parse app term" $ do
        let result = show . parse $ "((λx.x) y)"
         in result `shouldBe` "((λx.x) y)"
      it "Should parse bool op" $ do
        let result = show (runParser parseBoolBinOp "true || true")
         in result `shouldBe` "((λx.x) y)"

cekEvalTest :: IO ()
cekEvalTest =
  hspec $ do
    describe "CEK Beta reduction" $ do
      it "((λx.x) (λy.y)) ->* (λy.y)" $ do
        let result = show . cekEval . parse $ "((λx.x) (λy.y))"
         in result `shouldBe` "(λy.y)"
      it "((λx.x) true) ->* true" $ do
        let result = show . cekEval . parse $ "((λx.x) true)"
         in result `shouldBe` "true"
      it "((λx.x) false) ->* false" $ do
        let result = show . cekEval . parse $ "((λx.x) false)"
         in result `shouldBe` "false"
      it "((λx.x) ()) ->* ()" $ do
        let result = show . cekEval . parse $ "((λx.x) ())"
         in result `shouldBe` "()"

main :: IO ()
main = do
  consumeAllInputTest
  parserTest
  cekEvalTest
