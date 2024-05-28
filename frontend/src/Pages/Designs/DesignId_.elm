module Pages.Designs.DesignId_ exposing (Model, Msg, page)

import AppError exposing (AppError(..))
import Components.Title
import Effect exposing (Effect)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Keyed as Keyed
import FeatherIcons
import Html
import Html.Attributes as HAtt
import Http
import Page exposing (Page)
import ProteinDesign exposing (ProteinDesign, designDetailsFromProteinDesign)
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Shared
import Style
import Urls
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
    , errors : List AppError
    }


init : String -> ( Model, Effect Msg )
init designId =
    ( { designId = designId
      , design = Loading
      , errors = []
      }
    , Effect.sendCmd (getData <| Urls.designDetailsFromId designId)
    )


getData : String -> Cmd Msg
getData url =
    Http.get
        { url = url
        , expect =
            Http.expectJson DesignsDataReceived ProteinDesign.rawDesignDecoder
        }



-- UPDATE


type Msg
    = SendDesignsHttpRequest
    | DesignsDataReceived (Result Http.Error ProteinDesign)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case model.design of
        RemoteData.NotAsked ->
            case msg of
                SendDesignsHttpRequest ->
                    ( { model | design = Loading }
                    , Effect.sendCmd (getData <| Urls.designDetailsFromId model.designId)
                    )

                _ ->
                    ( model, Effect.none )

        RemoteData.Loading ->
            case msg of
                SendDesignsHttpRequest ->
                    ( model, Effect.none )

                DesignsDataReceived (Ok design) ->
                    ( { model | design = Success design }
                    , Effect.none
                    )

                DesignsDataReceived (Err e) ->
                    ( { model
                        | design = Failure e
                        , errors = DesignRequestFailed :: model.errors
                      }
                    , Effect.none
                    )

        RemoteData.Failure e ->
            case msg of
                _ ->
                    ( { model
                        | design = Failure e
                        , errors = DesignRequestFailed :: model.errors
                      }
                    , Effect.none
                    )

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
    { title = "Design Details"
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
            [ case mDesign of
                NotAsked ->
                    paragraph
                        (Style.bodyFont
                            ++ [ width fill, Font.center, Font.justify ]
                        )
                        [ text "Error querying the database. Try reloading the page."
                        ]

                Loading ->
                    paragraph
                        (Style.bodyFont
                            ++ [ width fill, Font.center, Font.justify ]
                        )
                        [ text "Loading the design..."
                        ]

                Failure e ->
                    paragraph
                        (Style.bodyFont
                            ++ [ width fill, Font.center, Font.justify ]
                        )
                        [ case e of
                            Http.BadUrl _ ->
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
        [ designDetailsHeader proteinDesign
        , wrappedRow
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
                [ table
                    [ padding 2 ]
                    { data = designDetailsFromProteinDesign proteinDesign
                    , columns =
                        [ { header =
                                paragraph
                                    [ Font.bold
                                    , paddingXY 5 10
                                    , Border.widthEach { bottom = 2, top = 2, left = 0, right = 0 }
                                    , Border.color <| rgb255 220 220 220
                                    ]
                                    [ text "Attribute" ]
                          , width = fillPortion 2
                          , view =
                                \category ->
                                    paragraph
                                        Style.monospacedFont
                                        [ column
                                            [ width (fill |> maximum 150)
                                            , height fill
                                            , scrollbarX
                                            , paddingXY 5 10
                                            ]
                                            [ text category.header ]
                                        ]
                          }
                        , { header =
                                paragraph
                                    [ Font.bold
                                    , paddingXY 10 10
                                    , Border.widthEach { bottom = 2, top = 2, left = 0, right = 0 }
                                    , Border.color <| rgb255 220 220 220
                                    ]
                                    [ text "Value" ]
                          , width = fillPortion 8
                          , view =
                                \detail ->
                                    paragraph
                                        Style.monospacedFont
                                        [ column
                                            [ width (fill |> maximum 700)
                                            , height fill
                                            , scrollbarX
                                            , paddingXY 10 10
                                            ]
                                            [ detail.property ]
                                        ]
                          }
                        ]
                    }
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
                [ width <| px 900
                , height <| px 400
                , padding 5
                , centerX
                , Border.width 2
                , Border.rounded 3
                , Border.color <| rgb255 220 220 220
                ]
                ( proteinDesign.pdb
                , Html.node "ngl-viewer"
                    [ HAtt.id "viewer"
                    , HAtt.style "width" "890px"
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
                                    [ text chain.chain_seq_unnat ]
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


designDetailsHeader : ProteinDesign -> Element msg
designDetailsHeader { pdb, previousDesign, nextDesign } =
    row
        [ width fill
        , spaceEvenly
        ]
        [ link
            []
            { url = "/designs/" ++ previousDesign
            , label =
                el [ centerX ]
                    (html <|
                        FeatherIcons.toHtml [ HAtt.align "center" ] <|
                            FeatherIcons.withSize 36 <|
                                FeatherIcons.arrowLeftCircle
                    )
            }
        , paragraph
            (Style.h2Font ++ [ Font.center ])
            [ text "Design Details" ]
        , link
            []
            { url = "/designs/" ++ nextDesign
            , label =
                el [ centerX ]
                    (html <|
                        FeatherIcons.toHtml [ HAtt.align "center" ] <|
                            FeatherIcons.withSize 36 <|
                                FeatherIcons.arrowRightCircle
                    )
            }
        ]
