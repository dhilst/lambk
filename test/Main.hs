module Main where

import           Lambk
import Parser
import           Test.Hspec


parserTest :: IO ()
parserTest = hspec $do
  describe "Parses" $ do
    it "Char" $ do
      let parseC = parseChar 'c'
        in runParser parseC "cx" `shouldBe` Just ("x", 'c')

evalTest :: IO ()
evalTest = hspec $ do
  describe "Beta reduction" $ do
    it "reduces to y" $ do
      show (eval (App (Lamb "x" (Var "x")) (Var "y"))) `shouldBe` "y"


main :: IO ()
main = do
  parserTest
  evalTest
