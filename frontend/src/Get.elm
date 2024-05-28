module Get exposing (..)


getScreenWidthInt : Maybe Float -> Int
getScreenWidthInt mScreenWidthF =
    Maybe.withDefault 800 <| String.toInt <| String.fromFloat <| Maybe.withDefault 800.0 mScreenWidthF


getScreenWidthIntNgl : Maybe Float -> Int
getScreenWidthIntNgl mScreenWidthF =
    case mScreenWidthF of
        Just width ->
            (0.85 * width) |> round

        _ ->
            800


getScreenWidthFloat : Maybe Float -> Float
getScreenWidthFloat mScreenWidthF =
    Maybe.withDefault 800.0 mScreenWidthF


getScreenWidthString : Maybe Float -> String
getScreenWidthString mScreenWidthF =
    (String.fromFloat <| Maybe.withDefault 800.0 mScreenWidthF) ++ "px"


getScreenWidthStringNgl : Maybe Float -> String
getScreenWidthStringNgl mScreenWidthF =
    case mScreenWidthF of
        Just width ->
            String.fromFloat (0.85 * width) ++ "px"

        _ ->
            "800"
