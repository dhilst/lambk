{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Applicative
import Control.Monad
import Debug.Trace
import Lamb
import Prelude hiding (and, or)

{-# ANN module ("hlint: ignore Use <$>") #-}

newtype Parser a =
  Parser
    { runParser :: String -> Maybe (String, a)
    }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      Just (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (<*>) (Parser p1) (Parser p2) =
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      return (input'', f a)

instance Alternative Parser where
  empty = Parser $ const Nothing
  Parser p1 <|> Parser p2 = Parser $ \input -> p1 input <|> p2 input

instance Monad Parser where
  return = pure
  (Parser p) >>= f =
    Parser $ \input -> do
      (input', x) <- p input
      runParser (f x) input'

many1 :: Parser a -> Parser [a]
many1 p =
  Parser $ \input -> do
    (input1, x1) <- runParser p input
    (input2, x2) <- helper p input1 [x1]
    return (input2, reverse x2)
  where
    helper :: Parser a -> String -> [a] -> Maybe (String, [a])
    helper p input x = do
      case runParser p input of
        Just (inputNext, xNext) -> helper p inputNext (xNext : x)
        Nothing -> Just (input, x)

chain1l :: Parser a -> Parser op -> Parser a
chain1l p op = Parser $ \input -> do undefined

parseChar :: Char -> Parser Char
parseChar x =
  Parser $ \case
    (y:ys)
      | x == y -> Just (ys, x)
    _ -> Nothing

parseString :: String -> Parser String
parseString s =
  let x = map parseChar s
   in sequenceA x

first :: [Parser a] -> Parser a
first parsers =
  Parser $ \input ->
    case parsers of
      (p:ps) ->
        case runParser p input of
          Just x -> Just x
          Nothing -> runParser (first ps) input
      _ -> Nothing

oneOfChar :: [Char] -> Parser Char
oneOfChar options = first (map parseChar options)

lpar = parseChar '('

rpar = parseChar ')'

lambChar = parseChar 'λ'

dot = parseChar '.'

letter = oneOfChar ['x' .. 'z']

spaces :: Parser String
spaces = many1 $ parseChar ' '

boolConst :: String -> Bool -> Parser Bool
boolConst s v =
  Parser $ \input -> do
    (input', x) <- runParser (parseString s) input
    return (input', v)

true :: Parser Bool
true = boolConst "true" True

false :: Parser Bool
false = boolConst "false" False

and :: Parser BoolOp
and =
  Parser $ \input -> do
    (input1, _) <- runParser (parseString "&&") input
    return (input1, And)

or :: Parser BoolOp
or =
  Parser $ \input -> do
    (input1, _) <- runParser (parseString "||") input
    return (input1, Or)

boolOp :: Parser BoolOp
boolOp = trace "bool op" $ and <|> or

-- start  : let
-- let    : "let" ID "=" let "in" let | lamb
-- lamb   : "λ" lvar [":" type] "." let | appl
-- appl   : appl var | var
-- var    : VAR
-- var    : "(" lamb ")" | ID -> id_ | VAR -> var
-- type   : (VAR | TCONST) "->" type | "'" VAR -> tvar | TCONST -> tconst
-- VAR     : /[a-z]/
-- ID      : /[a-z_][a-zA-Z_']*/
-- TCONST  : /(int|bool|str)/
--
-- start :: Parser Term
-- start = lamb
-- lamb = undefined <|> appl
-- appl = ???? left recursion ??? <|> var
-- var = word
term :: Parser Term
term = lambda <|> app <|> atom

atom = var <|> unit <|> bool

app :: Parser Term
app =
  Parser $ \input -> do
    (input1, _) <- runParser lpar input
    (input2, t1) <- runParser term input1
    (input3, _) <- runParser spaces input2
    (input4, t2) <- runParser term input3
    (input5, _) <- runParser rpar input4
    return (input5, App t1 t2)

unit :: Parser Term
unit =
  Parser $ \input -> do
    (input', _) <- runParser (parseString "()") input
    return (input', TUnit)

bool :: Parser Term
bool =
  trace "tbool" $
  Parser $ \input -> do
    (input', x) <- runParser (true <|> false) input
    return (input', TBool x)

var :: Parser Term
var =
  Parser $ \input -> do
    (input', x) <- runParser letter input
    return (input', Var [x])

lambda :: Parser Term
lambda =
  Parser $ \input -> do
    (input1, _) <- runParser lpar input
    (input2, _) <- runParser lambChar input1
    (input3, v) <- runParser letter input2
    (input4, _) <- runParser dot input3
    (input5, body) <- runParser term input4
    (input6, _) <- runParser rpar input5
    return (input6, Lamb [v] body)

parse :: String -> Term
parse input =
  case runParser term input of
    Just (input', t) ->
      if "" == input'
        then t
        else error $ "Doesnt consume all input >" ++ input' ++ "<"
    _ -> error "Parse error"
