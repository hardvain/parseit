module Combinators

import Core

%access public export

or : Parser a -> Parser a -> Parser a
or p1 p2 = MkParser $ \input => case runParser p1 input of
  r@(Just (rest, result)) => r
  Nothing => runParser p2 input

many : Parser a -> Parser (List a)
many p = MkParser $ \input => case runParser p input of
  Nothing => Nothing
  Just (rest, value) => case runParser (many p) rest of
    Nothing => Just (rest, [value])
    Just (rest2, values) => Just (rest2, value :: values)

skipMany : Parser a -> Parser ()
skipMany p = skipManySpaces `or` result () where 
  skipManySpaces : Parser ()
  skipManySpaces = do
    r <- many p
    result ()

choice : List (Parser a) -> Parser a
choice [] = zero
choice (x :: xs) = MkParser $ \input =>  case runParser x input of
  Nothing => runParser (choice xs) input
  otherwise => otherwise

between : Parser a -> Parser b -> Parser c -> Parser b
between open middle close = do
  _ <- open
  result <- middle
  _ <- close
  pure result

option : a -> Parser a -> Parser a
option default p = MkParser $ \input => case runParser p input of
  Nothing =>  Just(input, default)
  otherwise => otherwise

optionMaybe : Parser a -> Parser (Maybe a)
optionMaybe p = MkParser $ \input => case runParser p input of
  Nothing =>  Just(input, Nothing)
  Just(rest, result) => Just (rest, Just result)
 
optional : Parser a -> Parser ()
optional p = MkParser $ \input => case runParser p input of
  Nothing => Just (input, ())
  Just(rest, _) => Just (rest, ())

-- TODO: this is just a specialized version of `>>` found in haskell for monads. Find how to do it in idris
sequence : Parser a -> Parser b -> Parser b 
sequence p1 p2 = do
  _ <- p1
  p2

sepBy : Parser a -> Parser b -> Parser (List a)
sepBy parser seperator = do
  x <- parser
  xs <- many (sequence seperator parser)
  pure (x::xs)

endBy : Parser a -> Parser b -> Parser (List a)
endBy parser seperator = many ( do
  x <- parser
  _ <- seperator
  pure x)

lookAhead : Parser a -> Parser a
lookAhead p = MkParser $ \input => case runParser p input of
  Nothing => Nothing
  Just (_,result) => Just (input, result) 

oneOf : List Char -> Parser Char
oneOf chars = satisfy (\c => elem c chars)

noneOf : List Char -> Parser Char
noneOf chars = satisfy (\c => not (elem c chars))
