module ParseIt 

import Source

data ParseResult a = ParseSuccess (Maybe a) | ParseFailure String 

Functor ParseResult where
  map f (ParseSuccess (Just y)) = ParseSuccess (Just (f y))
  map f result@(ParseSuccess Nothing) = result
  map f result@(ParseFailure e) = result

Applicative ParseResult where
  pure a = ParseSuccess (Just a)
  result@(ParseSuccess Nothing) <*> (ParseSuccess x) = result
  (ParseSuccess (Just y)) <*> result@(ParseSuccess Nothing) = result
  (ParseSuccess (Just y)) <*> result@(ParseSuccess (Just x)) = map y result
  result@(ParseFailure y) <*> (ParseSuccess x) = result
  f <*> result@(ParseFailure x) = result

data Parser s a = MkParser (s -> (ParseResult a, s))

runParser : (Source s) => Parser s a -> s -> (ParseResult a,s)
runParser (MkParser f) y = f y

-- A parser that consumes one character from the source
item : (Source s) => Parser s Char
item = MkParser $ \input => 
  case next input of
    (Just char, rest) => (ParseSuccess (Just char), rest)
    (Nothing, _) => (ParseFailure "No more elements left to parse in the source", empty)

-- a parser that does not consume anything from the source
zero :  Parser s a
zero = MkParser $ \input => (ParseSuccess Nothing, input)

result : a -> Parser s a
result a = MkParser $ \input => (ParseSuccess (Just a), input)

Source s => Functor (Parser s) where
  map f p = MkParser $ \input => let (parseResult, rest) = (runParser p input) in (map f parseResult, rest)

Source s => Applicative (Parser s) where
  pure a = result a
  (MkParser f ) <*> (MkParser p) = MkParser $ \input => 
    case p input of
        (result1, rest) => 
          case f rest of 
            (result2, rest') => (result2 <*> result1, rest')


satisfy : (Source s) => (Char -> Bool) -> Parser s Char
satisfy predicate = MkParser $ \input => case runParser item input of
  (ParseSuccess (Just x), rest) => if (predicate x) then (ParseSuccess (Just x), rest) else (ParseSuccess Nothing, input)
  (ParseSuccess Nothing, rest) => (ParseSuccess Nothing, input)
  (ParseFailure message, rest) => (ParseSuccess Nothing, input)

char : (Source s) => Char -> Parser s Char
char x = satisfy(==x)

digit : (Source s) => Parser s Char
digit = satisfy (\x => '0' <= x && x >= '9')

lower : (Source s) => Parser s Char
lower = satisfy (\x => 'a' <= x && x >= 'z')

upper : (Source s) => Parser s Char
upper = satisfy (\x => 'A' <= x && x >= 'Z')

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
