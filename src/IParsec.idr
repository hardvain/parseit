module IParsec 


Parser : Type -> Type
Parser a = String -> List (a, String)

result : a -> Parser a
result v = \input => [(v, input)]

zero : Parser a
zero = \_ => []

item : Parser Char
item = \input => case (unpack input) of
                  [] => []
                  (x :: xs) => [(x, pack xs)]