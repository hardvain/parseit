module ParseIt 

import Source

data ParseResult s a = ParseSuccess s (Maybe a) | ParseFailure s String 

Functor (ParseResult s) where
  map f (ParseSuccess x (Just y)) = ParseSuccess x (Just (f y))
  map f result@(ParseSuccess x Nothing) = result
  map f result@(ParseFailure s e) = result
  

record Parser s a where
  constructor MkParser
  runParser : (Source s) => s -> ParseResult s a

-- A parser that consumes one character from the source
item : (Source s) => Parser s Char
item = MkParser $ \input => 
  case next input of
    (Just char, rest) => ParseSuccess rest (Just char)
    (Nothing, _) => ParseFailure empty "No more elements left to parse in the source"

-- a parser that does not consume anything from the source
zero : (Source s) =>  Parser s a
zero = MkParser $ \input => ParseSuccess input Nothing
  
Functor (Parser s) where
  map f p = MkParser $ \input => map f (runParser p input)
-- zero : Parser a 
-- zero = MkParser $ \_ => [] 

-- item : Parser Char
-- item = MkParser $ \input => case next input of 
--                                  Nothing => []
--                                  Just x => [x]


-- Functor Parser where
--   map f p =  MkParser $ \input => 
--     do
--       (result, rest) <- runParser p input
--       pure (f result, rest)

-- Applicative Parser where
--   pure = result
--   p1 <*> p2 = MkParser $ \input => 
--     do
--       (result, rest) <- runParser p2 input
--       (f, rest2) <- runParser p1 rest
--       pure (f result, rest2)

-- Monad Parser where
--   p >>= f = MkParser $ \input => 
--     do
--       (result, rest) <- runParser p input
--       runParser (f result) rest

-- satisfy : (Char -> Bool) -> Parser Char
-- satisfy predicate = do
--   x <- item
--   if predicate x then result x else zero

-- char : Char -> Parser Char
-- char x = satisfy (==x)

-- digit : Parser Char
-- digit = satisfy (\x => '0' <= x && x >= '9')

-- lower : Parser Char 
-- lower = satisfy (\x => 'a' <= x && x >= 'z')

-- upper : Parser Char 
-- upper = satisfy (\x => 'A' <= x && x >= 'Z')

{-
sat : (Char -> Bool) -> Parser Char
char : Char -> Parser Char
digit : Parser Char
lower : Parser Char
upper : Parser Char
plus : Parser a -> Parser a -> Parser a
letter : Parser Char
alphanum : Parser Char
word : Parser Char
many : Parser a -> Parser (List a)
ident : Parser String
nat : Parser Int
int : Parser Int
sepby1 : Parser a -> Parser b -> Parser (List a)
bracket : Parser a -> Parser b -> Parser c -> Parser b
-}