module Pages.Designs.DesignId_ exposing (Model, Msg, designDetailsView, details, page)

import AppError exposing (AppError(..))
import Browser.Dom
import Browser.Events
import Components.Title
import Effect exposing (Effect)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Keyed as Keyed
import FeatherIcons
import Get exposing (getScreenWidthFloat, getScreenWidthInt, getScreenWidthIntNgl, getScreenWidthString, getScreenWidthStringNgl)
import Html
import Html.Attributes as HAtt
import Http
import Page exposing (Page)
import ProteinDesign exposing (ProteinDesign, designDetailsFromProteinDesign)
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Shared
import Style
import Task
import Urls
import View exposing (View)


page : Shared.Model -> Route { designId : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \_ -> init route.params.designId shared.mScreenWidth
        , update = update
        , subscriptions = subscriptions
        , view = view >> Components.Title.view
        }



-- INIT


type alias Model =
    { designId : String
    , design : RemoteData Http.Error ProteinDesign
    , errors : List AppError
    , mWidthF : Maybe Float
    , widthS : String
    , widthSscaled : String
    , widthI : Int
    , originalWidthIscaled : Int
    , widthIscaled : Int
    , widthF : Float
    }


init : String -> Maybe Float -> ( Model, Effect Msg )
init designId mScreenWidthF =
    ( { designId = designId
      , design = Loading
      , errors = []
      , mWidthF = mScreenWidthF
      , widthS = "800"
      , widthSscaled = "800"
      , widthI = 800
      , originalWidthIscaled = 800
      , widthIscaled = 800
      , widthF = 800.0
      }
    , Effect.batch
        [ Effect.sendCmd (Task.attempt ViewportResult Browser.Dom.getViewport)
        , Effect.resetViewport ViewportReset
        , Effect.sendCmd (getData <| Urls.designDetailsFromId designId)
        ]
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
    | WindowResizes Int Int
    | ViewportResult (Result Browser.Dom.Error Browser.Dom.Viewport)
    | ViewportReset


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

                WindowResizes width _ ->
                    let
                        widthF =
                            toFloat width
                    in
                    ( { model | mWidthF = Just widthF }, Effect.resetViewport ViewportReset )

                ViewportResult result ->
                    case result of
                        Ok viewport ->
                            ( { model | mWidthF = Just viewport.viewport.width }, Effect.resetViewport ViewportReset )

                        Err _ ->
                            ( model, Effect.none )

                ViewportReset ->
                    ( { model
                        | widthS = getScreenWidthString model.mWidthF
                        , widthSscaled = getScreenWidthStringNgl model.mWidthF
                        , widthI = getScreenWidthInt model.mWidthF
                        , originalWidthIscaled = getScreenWidthIntNgl model.mWidthF
                        , widthIscaled = getScreenWidthIntNgl model.mWidthF
                        , widthF = getScreenWidthFloat model.mWidthF
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
                WindowResizes width _ ->
                    let
                        widthF =
                            toFloat width
                    in
                    ( { model | mWidthF = Just widthF }, Effect.resetViewport ViewportReset )

                ViewportResult result ->
                    case result of
                        Ok viewport ->
                            ( { model | mWidthF = Just viewport.viewport.width }, Effect.resetViewport ViewportReset )

                        Err _ ->
                            ( model, Effect.none )

                ViewportReset ->
                    let
                        newOriginalWidthIscaled =
                            case model.mWidthF of
                                Just widthF ->
                                    if toFloat model.originalWidthIscaled > widthF then
                                        getScreenWidthInt model.mWidthF

                                    else
                                        getScreenWidthInt (Just <| toFloat model.originalWidthIscaled)

                                _ ->
                                    model.originalWidthIscaled
                    in
                    ( { model
                        | widthS = getScreenWidthString model.mWidthF
                        , widthSscaled = getScreenWidthStringNgl model.mWidthF
                        , widthI = getScreenWidthInt model.mWidthF
                        , originalWidthIscaled = newOriginalWidthIscaled
                        , widthIscaled = getScreenWidthIntNgl model.mWidthF
                        , widthF = getScreenWidthFloat model.mWidthF
                      }
                    , Effect.none
                    )

                _ ->
                    ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\width height -> WindowResizes width height)



-- VIEW


view : Model -> View Msg
view model =
    { title = "Design Details"
    , attributes =
        [ centerX
        , width
            (fill
                |> minimum model.widthI
            )
        ]
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
                    designDetailsView model d
            ]
        ]


designDetailsView : Model -> ProteinDesign -> Element msg
designDetailsView model proteinDesign =
    column
        ([ centerX
         , width (fill |> maximum model.widthI)
         , padding 30
         , spacing 30
         , height fill
         ]
            ++ Style.bodyFont
        )
        [ designDetailsHeader proteinDesign
        , wrappedRow
            [ width fill
            , spacing 10
            , spaceEvenly
            ]
            [ column
                [ height fill
                , width (fill |> maximum model.widthI)
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
                                            [ width (fill |> maximum (model.widthI - 200))
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
            [ width (fill |> maximum model.widthIscaled)
            , spacing 20
            ]
            [ column
                Style.h2Font
                [ text "Structure"
                ]
            ]
        , column
            [ spacing 20
            , centerX
            ]
            [ Keyed.el
                [ width <| px model.originalWidthIscaled
                , height <| px 400

                --, Border.width 2
                --, Border.rounded 3
                --, Border.color <| rgb255 220 220 220
                ]
                ( proteinDesign.pdb
                , Html.node "ngl-viewer"
                    [ HAtt.id "viewer"
                    , HAtt.style "width" model.widthSscaled
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
                                    [ width
                                        (fill
                                            |> maximum 150
                                            |> minimum 80
                                        )
                                    , height fill
                                    , scrollbarX
                                    , paddingXY 5 10
                                    ]
                                    [ text chain.chain_id ]
                                ]
                  }
                , { header =
                        paragraph
                            [ width fill
                            , Font.bold
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
                                    [ width (fill |> maximum (model.widthI - 200))
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
designDetailsHeader { previousDesign, nextDesign } =
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
