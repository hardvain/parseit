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

applySecondParser : (a, String) -> Parser b -> List ((a,b), String)
applySecondParser (v, s) parserb =  case parserb s of
  [] => []
  (x::xs) => [((v, fst x), snd x)]

seq : Parser a -> Parser b -> Parser (a,b)
seq parser1 parser2 = \input => case parser1 input of
  [] => []
  (xs) => concat (map (\v => applySecondParser v parser2) xs)
