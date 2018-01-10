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


showPair : (String, Json) -> String
showPair (str, json) = show str ++ ":" ++ show json

Show Json where
  show (JsObject []) = "{}"
  show (JsObject xs) = "{" ++ concat (intersperse "," (map (\(str,json) => show str ++ ":" ++ show json) xs)) ++ "}"
  show (JsArray []) = "[]" 
  show (JsArray (xs)) = "[" ++ concat (intersperse "," (map show xs)) ++ "]"
  show (JsString x) = show x
  show (JsNumber x) = show x
  show (JsBool x) = show x
  show JsNull = "null"
