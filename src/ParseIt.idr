module ParseIt


data Parser a = MkParser (String -> (Maybe a,String))

runParser : Parser a -> String -> Maybe a
runParser (MkParser f) source = fst $ f source


satisfy : (Char -> Bool) -> Parser Char
satisfy predicate = MkParser $ \input => case (unpack input) of
  [] => (Nothing, input)
  (x::xs) => if predicate x then (Just x, pack xs) else (Nothing, input)

zero : Parser Char
zero = MkParser $ \input => (Nothing, input)

item : Parser Char
item = satisfy (\x => True)

digit : Parser Char
digit = satisfy (\x => '0' <= x && x <= '9')

lower : Parser Char
lower = satisfy (\x => 'a' <= x && x <= 'z')

upper : Parser Char
upper = satisfy (\x => 'A' <= x && x <= 'Z')

combine : Parser a -> Parser a -> Parser (List a)
combine (MkParser fn1) (MkParser fn2) = MkParser $ \input => case fn1 input of
  (Nothing, _) => (Nothing, input)
  (Just result1, rest) => case fn2 rest of
    (Nothing, _) => (Nothing, input)
    (Just result2, rest2) => (Just [result1, result2], rest2)

repeat : Parser a -> Int -> Parser (List a)
repeat parser times = foldl ?fun zero [1..times]


twoLower : Parser (List Char)
twoLower = combine lower lower

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
