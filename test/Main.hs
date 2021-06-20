module Main where

import Lambk
import Parser
import Test.Hspec

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

evalTest :: IO ()
evalTest =
  hspec $ do
    describe "Beta reduction" $ do
      it "((λx.(x x)) y) => (y y)" $ do
        let result = show . eval . parse $ "((λx.(x x)) y)"
         in result `shouldBe` "(y y)"
      it "((λx.(x z)) y) => (y z)" $ do
        let result = show . eval . parse $ "((λx.(x z)) y)"
         in result `shouldBe` "(y z)"

main :: IO ()
main = do
  parserTest
  evalTest
