module IParsec 


data Parser a = MkParser (String -> List(a,String))

Functor Parser where
  map func (MkParser f) = MkParser $ \input => map (\(k,v) => (func k, v)) (f input)

result : a -> Parser a
result x = MkParser $ (\input => [(x, input)])

zero : Parser a 
zero = MkParser $ \_ => []

item : Parser Char
item = MkParser $ \input => case (unpack input) of
                             [] => []
                             (x :: xs) => [(x, pack xs)] 

satisfy : (Char -> Bool) -> Parser Char
satisfy predicate = filter (\(k,v) => predicate k) xs
-- result : a -> Parser a
-- result v = MkParser $ \input => [(v, input)]

-- zero : Parser a
-- zero = MkParser ( \_ => MkParser $ [])

-- item : Parser Char
-- item = MkParser ( \input => case (unpack input) of
--                   [] => []
--                   (x :: xs) => [(x, pack xs)] )

-- applySecondParser : (a, String) -> Parser b -> List ((a,b), String)
-- applySecondParser (v, s) parserb =  case parserb s of
--   [] => []
--   (xs) => concat (map (\(b,rest) => [((v,b), rest)] ) xs)

-- seq : Parser a -> Parser b -> Parser (a,b)
-- seq parser1 parser2 = \input => case parser1 input of
--   [] => []
--   (xs) => concat (map (\v => applySecondParser v parser2) xs)


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
