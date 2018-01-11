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
  jsonParser = ?hole

  nullParser : Parser Json
  nullParser = map (\_ => JsNull) (string "null")

  defaultNumber : Maybe Int -> Int
  defaultNumber Nothing = 0
  defaultNumber (Just x) = x

  stringParser : Parser Json
  stringParser = result (JsString "a")

  numberParser : Parser Json
  numberParser = map (\numberString => JsNumber $ defaultNumber $ parseInteger (pack numberString)) (many digit)

  boolParser : Parser Json
  boolParser = map (\value => if value == "true" then JsBool True else JsBool False) ((string "true") `or` (string "false"))

  valueParser : Parser Json
  valueParser = ((((stringParser `or`numberParser) `or` objectParser) `or` arrayParser) `or` boolParser) `or` nullParser

  pairParser : Parser (String, Json)
  pairParser = do
    key <- word
    _ <- char ':'
    value <- valueParser
    pure (key, value)

  objectParser : Parser Json

  arrayParser : Parser Json