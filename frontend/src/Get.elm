module Get exposing (..)


getScreenWidthInt : Maybe Float -> Int
getScreenWidthInt mScreenWidthF =
    Maybe.withDefault 800 <| String.toInt <| String.fromFloat <| Maybe.withDefault 800.0 mScreenWidthF


getScreenWidthFloat : Maybe Float -> Float
getScreenWidthFloat mScreenWidthF =
    Maybe.withDefault 800.0 mScreenWidthF


getScreenWidthString : Maybe Float -> String
getScreenWidthString mScreenWidthF =
    (String.fromFloat <| Maybe.withDefault 800.0 mScreenWidthF) ++ "px"
