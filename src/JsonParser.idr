module JsonParser

import ParseIt
import Combinators
import Primitives

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