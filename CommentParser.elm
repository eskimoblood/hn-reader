module CommentParser where


import Parser (..)
import Parser.Char (..)
import Graphics.Element (..)
import Text (..)
import Html (Html, Attribute, i, a ,div, b, pre, code, span, text, toElement)
import Html.Attributes as Attributes
import String (startsWith, fromList, toList, isEmpty)
import Regex as Regex
import List as List
import Dict as Dict
import String as String
import Array (get)
import Maybe (Maybe(Just,Nothing), withDefault)

import Debug(log)

infixl 4 `andThen`

keepJust : (Maybe m) -> Bool
keepJust m = case m of
        Just m -> True
        Nothing -> False

toString : Parser Char Char -> Parser Char String
toString p = fromList <$> (some p)

noneOf : a -> Parser a a
noneOf x = satisfy (\s -> s /= x)

noneOf2 : a -> a -> Parser a a
noneOf2 x y = satisfy (\s -> s /= x && s /= y)

content : Parser Char Html
content =  map (\s->text (Regex.replace Regex.All (regex) replace s)) (toString <|noneOf '<')

whitespace : Parser Char Char
whitespace = map (\e-> ' ') (some <| symbol ' ')

quote : Parser Char Char
quote = symbol '"' `or`  symbol '\''

tagname : Parser Char String
tagname = toString <| noneOf2 '>' ' '


toAttribute : String -> Parser Char (Maybe Attribute)
toAttribute s = map (attr s) attributeValue

attr : String -> String -> (Maybe Attribute)
attr t v = if | t == "href" -> Just <| Attributes.href v
              | t == "class" -> Just <| Attributes.class v
              | otherwise -> Nothing

attribute : Parser Char (Maybe Attribute)
attribute = attributeName `andThen` toAttribute <* quote

attributeName : Parser Char String
attributeName = whitespace *>
                (toString <| noneOf '=')
                <* symbol '='
                <* quote

attributeValue : Parser Char String
attributeValue = toString <| noneOf2 '"' '\''

openingTag : Parser Char (List Html -> Html)
openingTag = symbol '<' *>
             (toNode <$> tagname) `andThen` withAttribute
             <* symbol '>'

closingTag : Parser Char String
closingTag =  (token <| toList "</") *> tagname <* symbol '>'

tag : Parser Char Html
tag =  openingTag `andThen` nodeContent <* closingTag

textNode : Parser Char String -> Parser Char Html
textNode p = (\s->text (Regex.replace Regex.All (regex) replace s)) <$> p

toNode : String -> (List Attribute -> List Html -> Html)
toNode s = if | s == "i" -> i
              | s == "b" -> b
              | s == "a" -> a
              | s == "pre" -> pre
              | s == "code" -> code
              | otherwise -> let sfx = (log "missing tag:" s) in span

withAttribute : (List Attribute -> List Html -> Html) -> Parser Char (List Html -> Html)
withAttribute e = map e (mapAndFilterAttributes <$> (many attribute))

mapAndFilterAttributes : (List (Maybe Attribute)) -> (List Attribute)
mapAndFilterAttributes l = (List.map (\e->case e of Just e->e) (List.filter keepJust l))

nodeContent : (List Html -> Html) -> Parser Char Html
nodeContent l = map (\ c -> l [c]) content

toHtml :  List Html ->  Html
toHtml s = div[]s

parseP : String ->  Html
parseP p =
    let
        result = parseString ( toHtml <$> ( some (tag <|> content))) p
    in
        case result of
            Err e -> div[][text p]
            Ok e -> div[] e

parse : String -> List Html
parse s =
    let
        s1 = Regex.replace Regex.All (Regex.regex "<pre><code>") (\ _-> "<pre>") s
        s2= Regex.replace Regex.All (Regex.regex "</pre></code>") (\ _->  "</pre>") s1
        splited = String.split "<p>" s2
    in
        [div[] (List.map parseP splited)]

matches = Dict.fromList [
    ("&gt;", ">"),
    ("&lt;", "<"),
    ("&quot;", "\""),
    ("&#x27;", "'"),
    ("&#x2F;", "/")
    ]

createRegexString : String -> String -> String
createRegexString s r =
  if
    |isEmpty r -> s
    |otherwise -> s ++ "|" ++ r

replace : Regex.Match -> String
replace {match} = withDefault match (Dict.get match matches)

regex = Regex.regex (List.foldr createRegexString "" (Dict.keys matches))
