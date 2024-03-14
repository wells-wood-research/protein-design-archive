module Pages.Designs.DesignId_ exposing (Model, Msg, page)

import Html exposing (Html)
import Page exposing (Page)
import View exposing (View)


page : { designId : String } -> Page Model Msg
page { designId } =
    Page.sandbox
        { init = init designId
        , update = update
        , view = view
        }



-- INIT


type alias Model =
    { designId : String }


init : String -> Model
init designId =
    { designId = designId }



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model



-- VIEW


view : Model -> View Msg
view model =
    View.fromString <| "Pages.Designs.DesignId_" ++ model.designId
