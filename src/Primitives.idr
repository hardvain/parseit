module Primitives

import Core
import Combinators

%access public export

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

letter : Parser Char
letter = upper `or` lower

alphanum : Parser Char
alphanum = letter `or` digit

vowel : Parser Char
vowel = oneOf (unpack "aeiou")

space : Parser Char
space = satisfy isSpace 

spaces : Parser ()
spaces = skipMany space

hexDigit : Parser Char
hexDigit = satisfy isHexDigit

octDigit : Parser Char
octDigit = satisfy isOctDigit

crlf : Parser Char
crlf = sequence carriageReturn newline

eol : Parser Char
eol = crlf `or` newline

anyChar : Parser Char
anyChar = satisfy (\_ => True)

string : String -> Parser String
string input = case unpack input of
  [] => MkParser $ \input => Just (input, "")
  (x :: xs) => do 
    _ <- char x
    _ <- string (pack xs)
    result input

    
word : Parser String
word = nonEmptyWord `or` result "" where 
  nonEmptyWord : Parser String
  nonEmptyWord = do
    x <- letter
    xs <- word
    pure (x::xs)