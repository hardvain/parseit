jsonParser : Parser Json
jsonParser = array `or` object

array : Parser JsArray
array = char '[' `followedBy` (zeroOrMore member ',') `followedBy` char ']'

object : Parser JsObject
object = char '{' `followedBy` (zeroOrMore pair ',') `followedBy` char '}'

pair : Parser (String, JsValue)
pair = string `followedBy` char ':' `followedBy` value

value : Parser JsValue
value = string | number | object | array | bool | null

bool : Parser JsBool
bool = string 'true' | string 'false'

null : Parser JsNull
null = string 'null'

number : Parser JsNumber
number = oneOrMore digit


