module Main where

import Kont
import Parser

main :: IO ()
main = do
  print . cekEval . parse $ "((λx.x) (λy.y))"
