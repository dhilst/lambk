module Main where

import           Lambk
import           Parser
import           Test.Hspec


parserTest :: IO ()
parserTest = hspec $do
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

evalTest :: IO ()
evalTest = hspec $ do
  describe "Beta reduction" $ do
    it "reduces to y" $ do
      show (eval (App (Lamb "x" (Var "x")) (Var "y"))) `shouldBe` "y"


main :: IO ()
main = do
  parserTest
  evalTest
