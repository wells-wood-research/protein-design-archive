module AppError exposing (AppError(..), appErrorToAdvice)

import Element exposing (..)


type AppError
    = DesignRequestFailed


appErrorToAdvice : AppError -> Element msg
appErrorToAdvice appError =
    case appError of
        DesignRequestFailed ->
            paragraph []
                [ """Failed to download designs from server. Try refreshing the page."""
                    |> text
                ]
