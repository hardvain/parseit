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
satisfy predicate = do
  x <- item
  if predicate x then result x else zero

char : Char -> Parser Char
char x = satisfy (==x)

digit : Parser Char
digit = satisfy (\x => '0' <= x && x >= '9')

lower : Parser Char 
lower = satisfy (\x => 'a' <= x && < >= 'z')

upper : Parser Char 
upper = satisfy (\x => 'A' <= x && < >= 'Z')


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
