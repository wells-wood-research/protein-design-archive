module Pages.Review.DesignId_ exposing (Model, Msg, page)

import AppError exposing (AppError(..))
import Browser.Dom
import Browser.Events
import Components.Title
import DesignFilter exposing (defaultKeys, keyToLabel)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Get exposing (..)
import Http
import Page exposing (Page)
import Pages.Designs.DesignId_ as Details
import Plots exposing (RenderPlotState(..))
import ProteinDesign exposing (ProteinDesign)
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Shared
import Style
import Task
import Time
import Urls
import View exposing (View)


page : Shared.Model -> Route { designId : String } -> Page Model Msg
page { mScreenWidthF } route =
    Page.new
        { init = \_ -> init mScreenWidthF route.params.designId
        , update = update
        , subscriptions = subscriptions
        , view = view >> Components.Title.view mScreenWidthF
        }



-- INIT


type alias Model =
    { designId : String
    , design : RemoteData Http.Error ProteinDesign
    , errors : List AppError
    , reviewer : String
    , classificationCheckboxDict : Dict String Bool
    , classification : Dict String Bool
    , voteCheckboxDict : Dict String Bool
    , voteRemove : Bool
    , comment : Maybe String
    , mScreenWidthF : Maybe Float
    , replotTime : Int
    , renderPlotState : RenderPlotState
    }


init : Maybe Float -> String -> ( Model, Effect Msg )
init mSharedScreenWidthF designId =
    ( { designId = designId
      , design = Loading
      , errors = []
      , reviewer = "Marta Chronowska (default)"
      , classificationCheckboxDict = classificationCheckboxDict
      , classification = Dict.empty
      , voteCheckboxDict = voteCheckboxDict
      , voteRemove = False
      , comment = Nothing
      , replotTime = 3
      , renderPlotState = WillRender
      , mScreenWidthF = mSharedScreenWidthF
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


classificationCheckboxDict : Dict String Bool
classificationCheckboxDict =
    Dict.fromList
        [ ( defaultKeys.classificationMinimalKey, False )
        , ( defaultKeys.classificationRationalKey, False )
        , ( defaultKeys.classificationEngineeredKey, False )
        , ( defaultKeys.classificationPhysKey, False )
        , ( defaultKeys.classificationDeepLearningKey, False )
        , ( defaultKeys.classificationConsensusKey, False )
        , ( defaultKeys.classificationOtherKey, False )
        ]


voteCheckboxDict : Dict String Bool
voteCheckboxDict =
    Dict.fromList
        [ ( defaultKeys.voteKeep, False )
        , ( defaultKeys.voteRemove, False )
        ]



-- UPDATE


type Msg
    = SendDesignsHttpRequest
    | DesignsDataReceived (Result Http.Error ProteinDesign)
    | UpdateDesignClassification String Bool
    | UpdateClassificationCheckbox String Bool
    | ClearClassificationCheckbox String
    | UpdateDesignVote
    | UpdateVoteCheckbox String Bool
    | ClearVoteCheckbox String
    | UpdateComment String
    | RenderWhenReady Time.Posix
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

                ViewportResult result ->
                    case result of
                        Ok viewport ->
                            ( { model | mScreenWidthF = Just viewport.viewport.width }, Effect.none )

                        Err _ ->
                            ( model, Effect.none )

                WindowResizes width _ ->
                    let
                        widthF =
                            toFloat width
                    in
                    ( { model | mScreenWidthF = Just widthF }, Effect.resetViewport ViewportReset )

                _ ->
                    ( model, Effect.none )

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
                UpdateDesignClassification key newFilter ->
                    ( { model | classification = Dict.insert key newFilter model.classification }
                    , Effect.none
                    )

                UpdateClassificationCheckbox key checkboxStatus ->
                    { model | classificationCheckboxDict = Dict.insert key checkboxStatus model.classificationCheckboxDict }
                        |> (if checkboxStatus then
                                update (UpdateDesignClassification key checkboxStatus)

                            else
                                update (ClearClassificationCheckbox key)
                           )

                ClearClassificationCheckbox key ->
                    ( { model | classificationCheckboxDict = Dict.remove key model.classificationCheckboxDict, classification = Dict.remove key model.classification }
                    , Effect.none
                    )

                UpdateDesignVote ->
                    ( { model | voteRemove = True }
                    , Effect.none
                    )

                UpdateVoteCheckbox key checkboxStatus ->
                    { model | voteCheckboxDict = Dict.insert key checkboxStatus model.voteCheckboxDict }
                        |> (if checkboxStatus then
                                update UpdateDesignVote

                            else
                                update (ClearVoteCheckbox key)
                           )

                ClearVoteCheckbox key ->
                    ( { model | voteCheckboxDict = Dict.remove key model.voteCheckboxDict, voteRemove = False }
                    , Effect.none
                    )

                UpdateComment comment ->
                    ( { model | comment = Just comment }
                    , Effect.none
                    )

                RenderWhenReady _ ->
                    case model.renderPlotState of
                        AwaitingRender 0 ->
                            ( { model | renderPlotState = Rendered }
                            , Effect.resetViewport ViewportReset
                            )

                        AwaitingRender remaining ->
                            ( { model | renderPlotState = AwaitingRender (remaining - 1) }
                            , Effect.none
                            )

                        _ ->
                            ( model, Effect.none )

                WindowResizes width _ ->
                    let
                        widthF =
                            toFloat width
                    in
                    ( { model | mScreenWidthF = Just widthF, renderPlotState = AwaitingRender model.replotTime }, Effect.none )

                ViewportResult result ->
                    case result of
                        Ok viewport ->
                            ( { model | mScreenWidthF = Just viewport.viewport.width }, Effect.resetViewport ViewportReset )

                        Err _ ->
                            ( model, Effect.none )

                _ ->
                    ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\width height -> WindowResizes width height)



-- VIEW


view : Model -> View Msg
view model =
    { title = "Design Review"
    , attributes =
        [ centerX
        , width
            (fill
                |> minimum (getScreenWidthInt model.mScreenWidthF)
            )
        ]
    , element = details model
    }


details : Model -> Element Msg
details model =
    let
        mDesign =
            model.design

        screenWidth =
            getScreenWidthInt <| model.mScreenWidthF

        halfWidth =
            getScreenWidthInt <| Just (toFloat screenWidth / 2)
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

                Success proteinDesign ->
                    if screenWidth > 800 then
                        column
                            ([ centerX
                             , width (fill |> maximum screenWidth)
                             , padding 30
                             , spacing 30
                             , height fill
                             ]
                                ++ Style.bodyFont
                            )
                            [ Details.designDetailsHeader "Design Review" "/review/" proteinDesign
                            , row
                                []
                                [ Details.designDetailsBody halfWidth proteinDesign
                                , reviewArea halfWidth model
                                ]
                            ]

                    else
                        column
                            ([ centerX
                             , width (fill |> maximum screenWidth)
                             , padding 30
                             , spacing 30
                             , height fill
                             ]
                                ++ Style.bodyFont
                            )
                            [ Details.designDetailsHeader "Design Review" "/review/" proteinDesign
                            , Details.designDetailsBody screenWidth proteinDesign
                            , reviewArea screenWidth model
                            ]
            ]
        ]


reviewArea : Int -> Model -> Element Msg
reviewArea elementWidthI model =
    column
        [ centerX
        , width fill
        , height fill
        , padding 30
        , spacing 30
        , Background.color <| rgb255 255 176 156
        ]
        [ classificationArea model
        , votingArea model
        , commentArea elementWidthI model
        ]


classificationArea : Model -> Element Msg
classificationArea model =
    column [ spacing 10, width fill ]
        [ paragraph
            (Style.h3Font
                ++ [ padding 10
                   , Border.widthEach { bottom = 2, top = 0, left = 0, right = 0 }
                   , Border.color <| rgb255 220 220 220
                   ]
            )
            [ text "Classification" ]
        , column [] <|
            List.map (\label -> classificationCheckbox ( model, label ))
                [ defaultKeys.classificationMinimalKey
                , defaultKeys.classificationRationalKey
                , defaultKeys.classificationEngineeredKey
                , defaultKeys.classificationPhysKey
                , defaultKeys.classificationDeepLearningKey
                , defaultKeys.classificationConsensusKey
                , defaultKeys.classificationOtherKey
                ]
        ]


votingArea : Model -> Element Msg
votingArea model =
    column [ spacing 10 ]
        [ paragraph
            (Style.h3Font
                ++ [ padding 10
                   , Border.widthEach { bottom = 2, top = 0, left = 0, right = 0 }
                   , Border.color <| rgb255 220 220 220
                   ]
            )
            [ text "Vote to remove" ]
        , row [] <|
            List.map (\label -> voteCheckbox ( model, label )) [ defaultKeys.voteRemove ]
        ]


classificationCheckbox : ( Model, String ) -> Element Msg
classificationCheckbox ( model, dictKey ) =
    Input.checkbox [ paddingXY 3 10, alignTop ]
        { onChange = \checkboxStatus -> UpdateClassificationCheckbox dictKey checkboxStatus
        , icon = Input.defaultCheckbox
        , checked =
            case Dict.get dictKey model.classificationCheckboxDict of
                Just value ->
                    value

                Nothing ->
                    False
        , label =
            case Dict.get dictKey model.classificationCheckboxDict of
                Just True ->
                    Input.labelRight [ centerY, width fill ] (wrappedRow (Font.bold :: Style.monospacedFont) <| [ text <| keyToLabel dictKey ])

                _ ->
                    Input.labelRight [ centerY, width fill ] (wrappedRow Style.monospacedFont <| [ text <| keyToLabel dictKey ])
        }


voteCheckbox : ( Model, String ) -> Element Msg
voteCheckbox ( model, dictKey ) =
    Input.checkbox [ paddingXY 3 10, alignTop ]
        { onChange = \checkboxStatus -> UpdateVoteCheckbox dictKey checkboxStatus
        , icon = Input.defaultCheckbox
        , checked =
            case Dict.get dictKey model.voteCheckboxDict of
                Just value ->
                    value

                Nothing ->
                    False
        , label =
            case Dict.get dictKey model.voteCheckboxDict of
                Just True ->
                    Input.labelRight [ centerY, width fill ] (paragraph (Font.bold :: Style.monospacedFont) <| [ text <| "Yes, I vote to REMOVE this design." ])

                _ ->
                    Input.labelRight [ centerY, width fill ] (paragraph Style.monospacedFont <| [ text <| "Yes, I vote to REMOVE this design." ])
        }


commentArea : Int -> Model -> Element Msg
commentArea elementWidthI model =
    column
        [ spacing 10
        , width
            (fill |> maximum elementWidthI)
        ]
        [ paragraph
            ([ padding
                10
             , Border.widthEach { bottom = 2, top = 0, left = 0, right = 0 }
             , Border.color <|
                rgb255 220
                    220
                    220
             ]
                ++ Style.h3Font
            )
            [ text "Comments" ]
        , Input.multiline
            Style.bodyFont
            { onChange = \string -> UpdateComment string
            , text = Maybe.withDefault "" model.comment
            , placeholder = Just <| Input.placeholder Style.monospacedFont (text "Enter your comments here")
            , label = Input.labelHidden "Design Review Comment Box"
            , spellcheck = True
            }
        ]
