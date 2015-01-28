module HN where

import Signal(..)
import Graphics.Element (..)
import Html (..)
import Html.Attributes (href, key, class, id, target)
import String (isEmpty)
import List  as L
import Time (Time, timestamp)
import RelativeTime (relativeTime)
import Json.Decode (..)
import Result (toMaybe)
import Debug (..)
import CommentParser as CP

port topNews : Signal (List News)
port user : Signal User
port comments : Signal Value
-- port loading : Signal Bool

type alias User = {id: String, about: String}
type alias News = {id: Int, title:String, by: String, url: String, text: String, time: Int, score: Int, comments: Int}
type  Comment = Comment {text: String, childs: List Comment, deleted: Bool}

type Update = User' User | News' (List News) | Comment'  (Maybe Comment) | LoadingContent' Bool
type Content = User'' User | Comment''  (Maybe Comment)

type alias State = {
    news: (List News),
    content: Maybe Content,
    time: Time
}

toComment : String -> Bool -> List Comment -> Comment
toComment s b l = Comment {text = s, deleted= b, childs = l}

commentsDecoder : Decoder Comment
commentsDecoder =
                    object3 toComment
                    ( oneOf ["text" := string, succeed ""])
                    ( oneOf ["deleted" := bool, succeed False])
                    ( "childs" := list (lazy (\_ -> commentsDecoder)))

lazy : (() -> Decoder a) -> Decoder a
lazy thunk =
  customDecoder value
      (\js -> decodeValue (thunk ()) js)

decodeComments :  Value -> Maybe Comment
decodeComments s = toMaybe (log "decode" (decodeValue commentsDecoder s))


updates = mergeMany
    [(News' <~ topNews),
     (User' <~ user),
     (Comment' <~ (decodeComments <~ comments))
    ]
    |> timestamp

state = foldp setState {news= [], time=0, content= Nothing} updates

setState : (Time, Update) -> State -> State
setState (time, update) state =
    case update of
        User' user -> {state | content <- (Just (User'' user)), time <- time}
        Comment' user -> {state | content <- (Just (Comment'' user)), time <- time}
        News' news -> {state | news <- news, time <- time}


renderNews :Time ->  News -> Html
renderNews  time news =
    let
        linkToComments = "#!item/" ++ (toString news.id)
        url = if isEmpty news.url then linkToComments else news.url
        t = if isEmpty news.url then "" else "_blank"
    in
        li[key <| toString news.id] [
            div[][
                h3[class "main"][a[href url, target t][text news.title]],
                p[][
                    span[][text ((relativeTime news.time time) ++ " by ")],
                    a[href ("#!user/" ++ news.by)][text news.by]
                ]
            ],
            a[href linkToComments][
                span[class "score main"][text (toString news.score)],
                span[class "comment-count main"][text (toString news.comments)]
            ]
        ]

renderUser : User -> Html
renderUser user = dl[][
        dt[][text "Name"],
        dd[][text user.id],
        dt[][text "About"],
        dd[][text user.about]
    ]

renderComments : Comment -> Html
renderComments comment =
    case  comment of
        Comment comment -> li[][
            div[](CP.parse (log "text" comment.text)),
            ul[](L.map renderComments comment.childs)
        ]

render : State  ->  Html
render state =
    let content =
        case state.content of
            Nothing -> div[][]
            Just  c -> case c of
                User'' u -> renderUser u
                Comment'' s ->
                    case  (log "comment" s) of
                        Nothing -> div[][]
                        Just  e -> ul[][renderComments e]
    in
        div[class "row"][
            div[class "column sidebar"][ul[](L.map (renderNews state.time) state.news)],
            div[class "column main content"][content]

        ]

scene : State -> Html
scene s =render s

main : Signal Html
main = scene <~ state
