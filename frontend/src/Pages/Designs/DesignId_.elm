module Pages.Designs.DesignId_ exposing (Model, Msg, page)

import AppError exposing (AppError(..))
import Components.Title
import Date
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Keyed as Keyed
import FeatherIcons
import Html
import Html.Attributes as HAtt
import Http
import Json.Decode
import Page exposing (Page)
import ProteinDesign exposing (ProteinDesign, authorsToString, classificationToString, designToCitation)
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Shared
import Style
import Task
import View exposing (View)


page : Shared.Model -> Route { designId : String } -> Page Model Msg
page _ route =
    Page.new
        { init = \_ -> init route.params.designId
        , update = update
        , subscriptions = subscriptions
        , view = view >> Components.Title.view
        }



-- INIT


type alias Model =
    { designId : String
    , design : RemoteData Http.Error ProteinDesign
    , prevCurrNext : RemoteData Http.Error (List ProteinDesign)
    , errors : List AppError
    }


init : String -> ( Model, Effect Msg )
init designId =
    ( { designId = designId
      , design = NotAsked
      , prevCurrNext = NotAsked
      , errors = []
      }
    , Effect.sendCmd <|
        (Task.succeed SendDesignsHttpRequest
            |> Task.perform identity
        )
    )


getData : String -> String -> Cmd Msg
getData url designId =
    Http.get
        { url = url ++ designId
        , expect =
            Http.expectJson DesignsDataReceived (Json.Decode.list ProteinDesign.rawDesignDecoder)
        }



-- UPDATE


type Msg
    = SendDesignsHttpRequest
    | DesignsDataReceived (Result Http.Error (List ProteinDesign))


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case model.prevCurrNext of
        RemoteData.NotAsked ->
            case msg of
                SendDesignsHttpRequest ->
                    ( { model | design = Loading, prevCurrNext = Loading }, Effect.sendCmd (getData "http://localhost:5000/three-designs/" model.designId) )

                _ ->
                    ( model, Effect.none )

        RemoteData.Loading ->
            case msg of
                SendDesignsHttpRequest ->
                    ( model, Effect.none )

                DesignsDataReceived (Ok d) ->
                    case d of
                        [] ->
                            ( { model | design = Failure (Http.BadBody "This design doesn't exist."), prevCurrNext = Failure (Http.BadBody "This design doesn't exist.") }
                            , Effect.none
                            )

                        designs ->
                            case List.head designs of
                                Just currentDesign ->
                                    ( { model | design = Success currentDesign, prevCurrNext = Success designs }
                                    , Effect.none
                                    )

                                _ ->
                                    ( model, Effect.none )

                DesignsDataReceived (Err e) ->
                    ( { model
                        | design = Failure e
                        , errors = DesignRequestFailed :: model.errors
                      }
                    , Effect.none
                    )

        RemoteData.Failure _ ->
            case msg of
                _ ->
                    ( model, Effect.none )

        RemoteData.Success _ ->
            case msg of
                _ ->
                    ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Design Details of " ++ model.designId
    , attributes = [ width fill ]
    , element = details model
    }


details : Model -> Element msg
details model =
    let
        mDesign =
            model.design
    in
    row []
        [ column
            [ width fill ]
            [ row [ width fill, spaceEvenly ]
                [ browseButton model "back"
                , el
                    (Style.h1Font
                        ++ [ centerX
                           , padding 20
                           ]
                    )
                  <|
                    text "Design Details"
                , browseButton model "next"
                ]
            , case mDesign of
                NotAsked ->
                    paragraph
                        (Style.bodyFont
                            ++ [ Font.center, Font.justify ]
                        )
                        [ text "Error querying the database. Try reloading the page."
                        ]

                Loading ->
                    paragraph
                        (Style.bodyFont
                            ++ [ Font.center, Font.justify ]
                        )
                        [ text "Loading the design..."
                        ]

                Failure e ->
                    paragraph
                        (Style.bodyFont
                            ++ [ Font.center, Font.justify ]
                        )
                        [ case e of
                            Http.BadUrl s ->
                                text "Error loading design: invalid URL."

                            Http.Timeout ->
                                text "Error loading design: it took too long to get a response."

                            Http.NetworkError ->
                                text "Error loading design: please connect to the Internet."

                            Http.BadStatus i ->
                                text ("Error loading design: status code " ++ String.fromInt i)

                            Http.BadBody s ->
                                text ("Error decoding JSON: " ++ s)
                        ]

                Success d ->
                    designDetailsView d
            ]
        ]


browseButton : Model -> String -> Element msg
browseButton model direction =
    link
        []
        { url = "/"
        , label =
            el [ centerX ]
                (html <|
                    FeatherIcons.toHtml [ HAtt.align "center" ] <|
                        FeatherIcons.withSize 50 <|
                            case direction of
                                "back" ->
                                    FeatherIcons.arrowLeftCircle

                                "next" ->
                                    FeatherIcons.arrowRightCircle

                                _ ->
                                    FeatherIcons.home
                )
        }


designDetailsView : ProteinDesign -> Element msg
designDetailsView proteinDesign =
    column
        ([ centerX
         , width fill
         , padding 20
         , spacing 30
         , height fill
         ]
            ++ Style.bodyFont
        )
        [ wrappedRow
            [ width fill
            , spacing 10
            ]
            [ el
                [ padding 2
                , Border.width 2
                , Border.color <| rgb255 220 220 220
                , Border.rounded 3
                , alignTop
                , width <| fillPortion 3
                ]
                (image
                    [ width fill ]
                    { src = proteinDesign.picture_path
                    , description = "Structure of " ++ proteinDesign.pdb
                    }
                )
            , column
                [ height fill
                , width <| fillPortion 7
                , spacing 10
                , Font.justify
                ]
                [ paragraph
                    []
                    [ text "PDB Code: "
                    , link
                        [ Font.color <| rgb255 104 176 171
                        , Font.underline
                        ]
                        { url =
                            "https://www.rcsb.org/structure/"
                                ++ proteinDesign.pdb
                        , label =
                            proteinDesign.pdb
                                |> text
                        }
                    ]
                , paragraph
                    []
                    [ "Release Date: "
                        ++ Date.toIsoString proteinDesign.release_date
                        |> text
                    ]
                , paragraph
                    []
                    [ "Design Classification: "
                        ++ classificationToString proteinDesign.classification
                        |> text
                    ]
                , paragraph
                    []
                    [ text "Structural Keywords: "
                    , el [] (text <| String.join ", " proteinDesign.tags)
                    ]
                , paragraph
                    []
                    [ text "Publication citation: "
                    , el [ Font.italic ] (text <| designToCitation proteinDesign)
                    ]
                , paragraph
                    []
                    [ text "Publication ISSN link: "
                    , link
                        [ Font.color <| rgb255 104 176 171
                        , Font.underline
                        ]
                        { url =
                            "https://portal.issn.org/resource/ISSN/" ++ proteinDesign.publication_id_issn
                        , label =
                            proteinDesign.publication_id_issn
                                |> text
                        }
                    ]
                , paragraph
                    []
                    [ "Authors: "
                        ++ authorsToString proteinDesign.authors
                        |> text
                    ]
                ]
            ]
        , column
            [ width fill
            , spacing 20
            ]
            [ column
                Style.h2Font
                [ text "Structure"
                ]
            , Keyed.el
                [ height fill
                , width fill
                , padding 5
                , Border.width 2
                , Border.rounded 3
                , Border.color <| rgb255 220 220 220
                ]
                ( proteinDesign.pdb
                , Html.node "ngl-viewer"
                    [ HAtt.id "viewer"
                    , HAtt.style "width" "910px"
                    , HAtt.style "height" "400px"
                    , HAtt.style "align" "center"
                    , HAtt.alt "3D structure"
                    , HAtt.attribute "pdb-string" proteinDesign.pdb
                    ]
                    []
                    |> html
                )
            ]
        , paragraph
            Style.h2Font
            [ text "Sequence"
            ]
        , table
            [ padding 2 ]
            { data = proteinDesign.chains
            , columns =
                [ { header =
                        paragraph
                            [ Font.bold
                            , paddingXY 5 10
                            , Border.widthEach { bottom = 2, top = 2, left = 0, right = 0 }
                            , Border.color <| rgb255 220 220 220
                            ]
                            [ text "Chain ID" ]
                  , width = fillPortion 2
                  , view =
                        \chain ->
                            paragraph
                                Style.monospacedFont
                                [ column
                                    [ width (fill |> maximum 150)
                                    , height fill
                                    , scrollbarX
                                    , paddingXY 5 10
                                    ]
                                    [ text chain.chain_id ]
                                ]
                  }
                , { header =
                        paragraph
                            [ Font.bold
                            , paddingXY 10 10
                            , Border.widthEach { bottom = 2, top = 2, left = 0, right = 0 }
                            , Border.color <| rgb255 220 220 220
                            ]
                            [ text "Sequence" ]
                  , width = fillPortion 8
                  , view =
                        \chain ->
                            paragraph
                                Style.monospacedFont
                                [ column
                                    [ width (fill |> maximum 700)
                                    , height fill
                                    , scrollbarX
                                    , paddingXY 10 10
                                    ]
                                    [ text chain.chain_seq ]
                                ]
                  }
                ]
            }
        , column
            [ width fill
            , spacing 20
            ]
            [ paragraph
                Style.h2Font
                [ text "Description"
                ]
            , paragraph
                [ Font.justify ]
                [ proteinDesign.abstract
                    |> text
                ]
            ]
        ]
