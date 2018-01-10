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

newline : Parser Char
newline = satisfy (== '\n')

tab : Parser Char
tab = satisfy (== '\t')

whitespace : Parser Char
whitespace = satisfy (== ' ')

carriageReturn : Parser Char
carriageReturn = satisfy (== '\r')

-- crlf : Parser Char
-- crlf = satisfy (== '\r\n')

or : Parser a -> Parser a -> Parser a
or p1 p2 = MkParser $ \input => case runParser p1 input of
  r@(Just (rest, result)) => r
  Nothing => runParser p2 input

-- eol : Parser Char
-- eol = crlf `or` newline

letter : Parser Char
letter = upper `or` lower

alphanum : Parser Char
alphanum = letter `or` digit

string : String -> Parser String
string input = case unpack input of
  [] => MkParser $ \input => Just (input, "")
  (x :: xs) => do 
    _ <- char x
    _ <- string (pack xs)
    result input

many : Parser a -> Parser (List a)
many p = MkParser $ \input => case runParser p input of
  Nothing => Nothing
  Just (rest, value) => case runParser (many p) rest of
    Nothing => Just (rest, [value])
    Just (rest2, values) => Just (rest2, value :: values)

skipMany : Parser a -> Parser ()
skipMany p = do
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