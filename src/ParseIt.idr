module ParseIt
%access public export

data ParseResult a = ParseSuccess a | ParseFailure String

data Parser a = MkParser (String -> Maybe (String, a))

implicit stringToChars : String -> List Char
stringToChars = unpack

implicit charsToString : List Char -> String
charsToString = pack

zero : Parser a
zero = MkParser $ \input => Nothing

result : a -> Parser a
result a = MkParser $ \input => Just (input, a)

item : Parser Char
item = MkParser $ \input => case (unpack input) of 
  [] => Nothing
  (x::xs) => Just (pack xs, x)

Functor Parser where
  map f (MkParser g) = MkParser $ \input => do
    (rest, x) <- g input
    pure (rest, f x)

Applicative Parser where
  pure = result
  (MkParser f) <*> (MkParser g) = MkParser $ \input => do
    (rest, fun) <- f input
    (rest2, x) <- g rest
    pure (rest2, fun x)

Monad Parser where
  (MkParser g) >>= f = MkParser $ \input => do
    (rest, result) <- g input
    let (MkParser p) = f result
    p rest

runParser : Parser a -> String -> Maybe (String, a)
runParser (MkParser f) source = f source

satisfy : (Char -> Bool) -> Parser Char
satisfy predicate = do
  x <- item
  if predicate x 
    then result x
    else zero

char : Char -> Parser Char
char character = satisfy (== character)

digit : Parser Char
digit = satisfy (\x => '0' <= x && x <= '9')

lower : Parser Char
lower = satisfy (\x => 'a' <= x && x <= 'z')

upper : Parser Char
upper = satisfy (\x => 'A' <= x && x <= 'Z')

string : String -> Parser String
string input = case unpack input of
  [] => MkParser $ \input => Just (input, "")
  (x :: xs) => do 
    _ <- char x
    _ <- string (pack xs)
    result input
    
followedBy : Parser a -> Parser b -> (a -> b -> c) -> Parser c
followedBy p1 p2 x = do
  r1 <- p1
  r2 <- p2
  result (x r1 r2)

or : Parser a -> Parser a -> Parser a
or (MkParser f) (MkParser g) = ?or_rhs_2
{-
parser is a function from string to some output
Parser a : String -> Maybe a
runParser takes a parser and string and gives the result
runParser : Parser a -> String -> Maybe a
parseJson : String -> Json
parseJson ""
parse : a -> b

parseJson : String -> Json
parseJson string = parse string with satisfyParser

-}
