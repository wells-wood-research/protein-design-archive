module Pages.Review.DesignId_ exposing (Model, Msg, page)

import AppError exposing (AppError(..))
import Components.Title
import Date
import DesignFilter exposing (defaultKeys, keyToLabel)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import FeatherIcons
import Html
import Html.Attributes as HAtt exposing (align)
import Http
import Page exposing (Page)
import ProteinDesign
    exposing
        ( Classification
        , ProteinDesign
        , authorsToString
        , classificationToString
        , designToCitation
        , stringToClassification
        , tagsToString
        )
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Shared
import Style
import Vega exposing (background)
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
      , reviewer = "Marta Chronowska (default)"
      , classificationCheckboxDict = classificationCheckboxDict
      , classification = Dict.empty
      , voteCheckboxDict = voteCheckboxDict
      , voteRemove = False
      , comment = Nothing
      }
    , Effect.sendCmd (getData ("http://localhost:5000/design-details/" ++ designId))
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
        , ( defaultKeys.classificationCompPhysKey, False )
        , ( defaultKeys.classificationCompDLKey, False )
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
                    , Effect.sendCmd (getData ("http://localhost:5000/design-details/" ++ model.designId))
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
    let
        mDesign =
            model.design
    in
    column []
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
        , reviewArea model
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


designDetailsHeader : ProteinDesign -> Element msg
designDetailsHeader { previousDesign, nextDesign } =
    row
        [ width fill
        , spaceEvenly
        ]
        [ link
            []
            { url = "/review/" ++ previousDesign
            , label =
                el [ centerX ]
                    (html <|
                        FeatherIcons.toHtml [ HAtt.align "center" ] <|
                            FeatherIcons.withSize 36 <|
                                FeatherIcons.arrowLeftCircle
                    )
            }
        , el Style.h2Font (text "Design Details")
        , link
            []
            { url = "/review/" ++ nextDesign
            , label =
                el [ centerX ]
                    (html <|
                        FeatherIcons.toHtml [ HAtt.align "center" ] <|
                            FeatherIcons.withSize 36 <|
                                FeatherIcons.arrowRightCircle
                    )
            }
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
                , defaultKeys.classificationCompPhysKey
                , defaultKeys.classificationCompDLKey
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
