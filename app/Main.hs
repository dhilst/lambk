module Main where

import CEK
import Parser

main :: IO ()
main = do
  print . cekEval . parse $ "((λx.x) (λy.y))"
