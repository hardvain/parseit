module JsonParser

import Core
import Combinators
import Primitives
import Data.String 

%access public export

data Json = 
  JsObject (List (String, Json))
  | JsArray (List Json)
  | JsString String
  | JsNumber Int
  | JsBool Bool
  | JsNull


Show Json where
  show (JsObject []) = "{}"
  show (JsObject xs) = "{" ++ concat (intersperse "," (map (\(str,json) => "'" ++ str ++ "'" ++ ":" ++ show json) xs)) ++ "}"
  show (JsArray []) = "[]" 
  show (JsArray (xs)) = "[" ++ concat (intersperse "," (map show xs)) ++ "]"
  show (JsString x) = "'" ++ x ++ "'"
  show (JsNumber x) = show x
  show (JsBool False) = "true"
  show (JsBool True) = "false"
  show JsNull = "null"

sample : Json
sample = JsObject [
    ("name", JsString "aravindh"),
    ("age", JsNumber 28),
    ("isMale", JsBool True),
    ("languages", JsArray [JsString "idris", JsString "rust"]),
    ("bio", JsObject [("first_name", JsString "Aravindh"), ("last_name", JsString "Sridaran")])
  ]
mutual 
  jsonParser : Parser Json
  jsonParser = valueParser

  nullParser : Parser Json
  nullParser = map (\_ => JsNull) (string "null")

  defaultNumber : Maybe Int -> Int
  defaultNumber Nothing = 0
  defaultNumber (Just x) = x

  stringParser : Parser Json
  stringParser = do
    _ <- char '"'
    str <- word
    _ <- char '"'    
    pure (JsString str)

  numberParser : Parser Json
  numberParser = map (\numberString => JsNumber $ defaultNumber $ parseInteger (pack numberString)) (many digit)

  boolParser : Parser Json
  boolParser = map (\value => if value == "true" then JsBool True else JsBool False) ((string "true") `or` (string "false"))

  valueParser : Parser Json
  valueParser = (objectParser `or` (arrayParser `or` (nullParser `or` (boolParser `or` (numberParser `or` stringParser)))) )
  
  keyParser : Parser String
  keyParser = do
    _ <- char '"'
    _ <- spaces
    key <- word
    _ <- spaces
    _ <- char '"'
    pure key
    
  pairParser : Parser (String, Json)
  pairParser = do
    key <- keyParser
    _ <- spaces
    _ <- char ':'
    _ <- spaces
    value <- valueParser
    pure (key, value)

  objectParser : Parser Json
  objectParser = do
    _ <- char '{'
    _ <- spaces    
    pairs <- sepBy pairParser (char ',')
    _ <- spaces    
    _ <- char '}'
    pure (JsObject pairs)

  arrayParser : Parser Json
  arrayParser = do
    _ <- char '['
    _ <- spaces    
    values <- sepBy valueParser (char ',')
    _ <- spaces    
    _ <- char ']'
    pure (JsArray values)

jsonString : String
jsonString = "{\"name\":\"aravindh\",\"age\":28,\"isMale\":true,\"isFemale\":false,\"languages\":[\"idris\",\"rust\",\"scala\"]}"