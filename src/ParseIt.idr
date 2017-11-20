module ParseIt 

import Source

data ParseResult a = ParseSuccess (Maybe a) | ParseFailure String 

Functor ParseResult where
  map f (ParseSuccess (Just y)) = ParseSuccess (Just (f y))
  map f result@(ParseSuccess Nothing) = result
  map f result@(ParseFailure e) = result

Applicative ParseResult where
  pure a = ParseSuccess (Just a)
  (ParseSuccess Nothing) <*> result@(ParseSuccess x) = result
  (ParseSuccess (Just y)) <*> (ParseSuccess x) = ?holeApplyApplicative_3
  result@(ParseFailure y) <*> (ParseSuccess x) = result
  f <*> result@(ParseFailure x) = result

record Parser s a where
  constructor MkParser
  runParser : (Source s) => s -> (ParseResult a, s)

-- A parser that consumes one character from the source
item : (Source s) => Parser s Char
item = MkParser $ \input => 
  case next input of
    (Just char, rest) => (ParseSuccess (Just char), rest)
    (Nothing, _) => (ParseFailure "No more elements left to parse in the source", empty)

-- a parser that does not consume anything from the source
zero : (Source s) =>  Parser s a
zero = MkParser $ \input => (ParseSuccess Nothing, input)

result : (Source s) => a -> Parser s a
result a = MkParser $ \input => (ParseSuccess (Just a), input)

Functor (Parser s) where
  map f p = MkParser $ \input => let (parseResult, rest) = (runParser p input) in (map f parseResult, rest)

Applicative (Parser s) where
  -- TODO: Use `result` here
  pure a = MkParser $ \input => (ParseSuccess (Just a), input)
  f <*> fa = ?holeApplyApplicative
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
