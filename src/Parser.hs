{-# LANGUAGE LambdaCase #-}
module Parser where

import Control.Monad
import Control.Applicative
import Lambk
{-# ANN module ("hlint: ignore Use <$>") #-}

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }


instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (input', x) <- p input
    Just (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (<*>) (Parser p1) (Parser p2) = Parser $ \input -> do
    (input', f) <- p1 input
    (input'', a) <- p2 input'
    return (input'', f a)

instance Alternative Parser where
  empty = Parser $ const Nothing
  Parser p1 <|> Parser p2 = Parser $ \input -> p1 input <|> p2 input


instance Monad Parser where
  return = pure
  (Parser p) >>= f = Parser $ \input -> do
    (input', x) <- p input
    runParser (f x) input'

parseChar :: Char -> Parser Char
parseChar x = Parser $ \case
  (y:ys) | x == y -> Just (ys, x)
  _ -> Nothing


parseString :: String -> Parser String
parseString s = let x = map parseChar s
  in sequenceA x

firstOf :: [Parser a] -> Parser a
firstOf parsers = Parser $ \input -> case parsers of
  (p:ps) -> case runParser p input of
    Just x -> Just x
    Nothing -> runParser (firstOf ps) input
  _ -> Nothing

oneOfChar :: [Char] -> Parser Char
oneOfChar options = firstOf (map parseChar options)

pLpar = parseChar '('
pRpar = parseChar ')'
pLamb = parseChar 'λ'
pDot = parseChar '.'
pVar = oneOfChar ['x'..'z']

parseLambda :: Parser Term
parseLambda = Parser $ \input -> do
  (input1, _) <- runParser pLpar input
  (input2, _) <- runParser pLamb input1
  (input3, v) <- runParser pVar input2
  (input4, _) <- runParser pDot input3
  (input5, body) <- runParser pVar input4
  (input6, _) <- runParser pRpar input5
  return (input6, Lamb [v] (Var [body]))

