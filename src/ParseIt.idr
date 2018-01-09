module ParseIt
%access public export
data Parser a = MkParser (String -> (Maybe a,String))

Functor Parser where
  map f (MkParser g) = MkParser $ \input => case g input of
    (Just x, rest) => (Just $ f x, rest)
    (Nothing,_) => (Nothing, input)

zero : Parser a
zero = MkParser $ \input => (Nothing, input)

result : a -> Parser a
result a = MkParser $ \input => (Just a, input)

Applicative Parser where
  pure a = result a
  (MkParser f) <*> (MkParser g) = MkParser $ \input => case f input of
    (Nothing, rest) => (Nothing, input)
    (Just fun, rest) => case g rest of
      (Nothing, rest2) => (Nothing, input)
      (Just value, rest2) => (Just (fun value), rest2)


Monad Parser where
  (MkParser g) >>= f = MkParser $ \input => case g input of
    (Nothing, rest) => (Nothing, input)
    (Just value, rest) => case (f value) of
      MkParser p => case p rest of
        (Nothing, _) => (Nothing, input)
        (Just y, rest2) => (Just y, rest2)

runParser : Parser a -> String -> Maybe a
runParser (MkParser f) source = fst $ f source

satisfy : (Char -> Bool) -> Parser Char
satisfy predicate = MkParser $ \input => case (unpack input) of
  [] => (Nothing, input)
  (x::xs) => if predicate x then (Just x, pack xs) else (Nothing, input)


item : Parser Char
item = satisfy (\x => True)

char : Char -> Parser Char
char character = satisfy (== character)

digit : Parser Char
digit = satisfy (\x => '0' <= x && x <= '9')

lower : Parser Char
lower = satisfy (\x => 'a' <= x && x <= 'z')

upper : Parser Char
upper = satisfy (\x => 'A' <= x && x <= 'Z')

bind : Parser a -> (a -> Parser b) -> Parser b
bind (MkParser g) f = MkParser $ \input => case g input of 
  (Nothing, _) => (Nothing, input)
  (Just value, rest) => case (f value) of
    MkParser p => p rest


string : String -> Parser String
string input = case unpack input of
  [] => zero
  (x :: xs) => char x >>= \_ => string (pack xs) >>= \_ => result input
    

classKeywordParser : Parser String
classKeywordParser = string "class"

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
