module Pages.Review.DesignId_ exposing (Model, Msg, page)

import AppError exposing (AppError(..))
import Components.Title
import DesignFilter exposing (defaultKeys, keyToLabel)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Http
import Page exposing (Page)
import Pages.Designs.DesignId_ as Details
import ProteinDesign exposing (ProteinDesign)
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
    , mWidthF : Maybe Float
    , reviewer : String
    , classificationCheckboxDict : Dict String Bool
    , classification : Dict String Bool
    , voteCheckboxDict : Dict String Bool
    , voteRemove : Bool
    , comment : Maybe String
    }


init : String -> ( Model, Effect Msg )
init designId =
    ( { designId = designId
      , design = Loading
      , errors = []
      , mWidthF = Just 800.0
      , reviewer = "Marta Chronowska (default)"
      , classificationCheckboxDict = classificationCheckboxDict
      , classification = Dict.empty
      , voteCheckboxDict = voteCheckboxDict
      , voteRemove = False
      , comment = Nothing
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

                _ ->
                    ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Design Review"
    , attributes = [ width fill ]
    , element = details model
    }


details : Model -> Element Msg
details model =
    column
        [ width fill ]
        [ Details.details model.mWidthF model.design
        , reviewArea model
        ]


reviewArea : Model -> Element Msg
reviewArea model =
    column
        [ centerX
        , width fill
        , padding 20
        , spacing 30
        , Background.color <| rgb255 255 176 156
        ]
        [ classificationArea model
        , votingArea model
        , commentArea model
        ]


classificationArea : Model -> Element Msg
classificationArea model =
    column []
        [ paragraph
            Style.h2Font
            [ text "Classification" ]
        , row [] <|
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
    column []
        [ paragraph
            Style.h2Font
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
                    Input.labelRight [ centerY, width fill ] (paragraph (Font.bold :: Style.bodyFont) <| [ text <| keyToLabel dictKey ])

                _ ->
                    Input.labelRight [ centerY, width fill ] (paragraph Style.bodyFont <| [ text <| keyToLabel dictKey ])
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
                    Input.labelRight [ centerY, width fill ] (paragraph (Font.bold :: Style.bodyFont) <| [ text <| "Yes, I vote to REMOVE this design." ])

                _ ->
                    Input.labelRight [ centerY, width fill ] (paragraph Style.bodyFont <| [ text <| "Yes, I vote to REMOVE this design." ])
        }


commentArea : Model -> Element Msg
commentArea model =
    column
        ([ spacing 10, width fill ]
            ++ Style.h2Font
        )
        [ text "Comments"
        , Input.multiline
            Style.bodyFont
            { onChange = \string -> UpdateComment string
            , text = Maybe.withDefault "" model.comment
            , placeholder = Just <| Input.placeholder [] (text "Enter your comments here")
            , label = Input.labelHidden "Design Review Comment Box"
            , spellcheck = True
            }
        ]
