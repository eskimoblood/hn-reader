module RelativeTime where

import Time (Time, inSeconds)
minute = 60
hour = minute * 60
day = hour * 24

timespanAsString : Float -> Float -> String
timespanAsString t f = t / f |> floor |> toString

time : String -> String -> String -> String
time  s p t =
    if | t == "1" -> t ++ " " ++ s ++ " ago"
       | otherwise -> t ++ " " ++ p ++ " ago"

relativeTime : Int -> Time -> String
relativeTime start end =
    let
        timespan = (inSeconds end) - (toFloat start)
    in
        if | timespan < minute -> (toString timespan) |> (time "second" "seconds")
           | timespan < hour -> (timespanAsString timespan  minute) |> (time "minute" "minutes")
           | timespan < day -> (timespanAsString timespan  hour) |> (time "hour" "hours")
           | timespan >= day -> (timespanAsString timespan  day) |> (time "day" "days")
