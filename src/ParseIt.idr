module ParseIt 

record Parser a where
  constructor MkParser
  runParser : String -> List(a, String) 
  
result : a -> Parser a
result a = MkParser $ \input => [(a, input)]

zero : Parser a 
zero = MkParser $ \_ => [] 

item : Parser Char
item = MkParser $ \input => case (unpack input) of
                              [] => []
                              (x :: xs) => [(x, pack xs)] 
Functor Parser where
  map f p =  MkParser $ \input => 
    do
      (result, rest) <- runParser p input
      pure (f result, rest)

Applicative Parser where
  pure = result
  p1 <*> p2 = MkParser $ \input => 
    do
      (result, rest) <- runParser p2 input
      (f, rest2) <- runParser p1 rest
      pure (f result, rest2)

Monad Parser where
  p >>= f = MkParser $ \input => 
    do
      (result, rest) <- runParser p input
      runParser (f result) rest

satisfy : (Char -> Bool) -> Parser Char
satisfy predicate = do
  x <- item
  if predicate x then result x else zero

char : Char -> Parser Char
char x = satisfy (==x)

digit : Parser Char
digit = satisfy (\x => '0' <= x && x >= '9')

lower : Parser Char 
lower = satisfy (\x => 'a' <= x && x >= 'z')

upper : Parser Char 
upper = satisfy (\x => 'A' <= x && x >= 'Z')

string : String -> Parser String
string "" =  result ""
string xs = case unpack xs of
  (x :: xs) => 
    do
      _ <- char x
      _ <- string (pack xs)
      result $ pack (x :: xs)

aravindh : Parser String
aravindh = string "aravindh"

age : Parser Char
age = digit
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
