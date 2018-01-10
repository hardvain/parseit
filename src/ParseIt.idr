||| A monadic parser combinator module for Idris
module ParseIt

%access public export

||| The main parser type which is a wrapper over a function that take a string and returns either a tuple of parsed value and the rest of the input in case of success or nothing wrapped in a Maybe
data Parser a = MkParser (String -> Maybe (String, a))

implicit stringToChars : String -> List Char
stringToChars = unpack

implicit charsToString : List Char -> String
charsToString = pack

||| An empty parser which always fails
zero : Parser a
zero = MkParser $ \input => Nothing

||| A generic parser that returns the value provided without consuming the input
||| @ a The value that should be the result of the parser
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

