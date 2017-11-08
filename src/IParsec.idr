module IParsec 

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
  map f (MkParser runParser) =  MkParser $ \input => map (\result => (f $ fst result, snd result)) (runParser input)

helper : (a,String) -> (String -> List (a->b,String)) -> List (b,String) 
helper (k,v) parser = let parseResult = parser v in 
                          map (\(func,rest) => (func k, rest)) parseResult

Applicative Parser where
  pure = result
  (MkParser p2) <*> (MkParser p1) = MkParser ( \input => concat $ map (\x => helper x p2) (p1 input))

Monad Parser where
  (MkParser p) >>= f = MkParser $ \input => concat $ map (\(k,v) => runParser (f k) v) (p input)

satisfy : (Char -> Bool) -> Parser Char
satisfy predicate = MkParser $ \input => case runParser item input of 
                                          [] => []
                                          xs => filter (\(k,v) => predicate k) xs




-- seq : Parser a -> Parser b -> Parser (a,b)
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
