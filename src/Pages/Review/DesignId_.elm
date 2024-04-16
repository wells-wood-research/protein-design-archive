module Pages.Review.DesignId_ exposing (Model, Msg, page)

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
import Html.Attributes exposing (align)
import Page exposing (Page)
import ProteinDesign exposing (Classification, ProteinDesign, authorsToString, classificationToString, stringToClassification, tagsToString)
import Route exposing (Route)
import Shared
import Style
import Vega exposing (background)
import View exposing (View)



-- Add "Auth.User ->" at the front later


page : Shared.Model -> Route { designId : String } -> Page Model Msg
page shared route =
    Page.new
        { init = init route.params.designId
        , update = update
        , subscriptions = subscriptions
        , view = view shared >> Components.Title.view
        }



-- INIT


type alias Model =
    { designId : String
    , reviewer : String
    , classificationCheckboxDict : Dict String Bool
    , classification : Dict String Bool
    , voteCheckboxDict : Dict String Bool
    , voteRemove : Bool
    , comment : Maybe String
    }


init : String -> () -> ( Model, Effect Msg )
init designId _ =
    ( { designId = designId
      , reviewer = "Marta Chronowska (default)"
      , classificationCheckboxDict = classificationCheckboxDict
      , classification = Dict.empty
      , voteCheckboxDict = voteCheckboxDict
      , voteRemove = False
      , comment = Nothing
      }
    , Effect.batch
        [ Effect.resetViewport NoOp
        ]
    )


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
    = NoOp
    | UpdateDesignClassification String Bool
    | UpdateClassificationCheckbox String Bool
    | ClearClassificationCheckbox String
    | UpdateDesignVote
    | UpdateVoteCheckbox String Bool
    | ClearVoteCheckbox String
    | UpdateComment String


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )

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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Design Review"
    , attributes = [ width fill ]
    , element =
        details model (Dict.get model.designId shared.designs)
    }


details : Model -> Maybe ProteinDesign -> Element Msg
details model mDesign =
    column
        [ width fill ]
        [ el
            (Style.h1Font
                ++ [ centerX
                   , padding 20
                   ]
            )
          <|
            text "Design Review"
        , case mDesign of
            Nothing ->
                paragraph
                    (Style.bodyFont
                        ++ [ Font.center ]
                    )
                    [ text "This design doesn't need review."
                    ]

            Just design ->
                designDetailsView design
        , reviewArea model
        ]


designDetailsView : ProteinDesign -> Element msg
designDetailsView proteinDesign =
    column
        ([ centerX
         , width fill
         , padding 20
         , spacing 30

         --, Background.color <| rgb255 174 209 246
         ]
            ++ Style.bodyFont
        )
        [ wrappedRow
            [ height fill
            , width fill
            , spacing 10
            ]
            [ image
                [ width <| px 250 ]
                { src = proteinDesign.picture_path
                , description = "Structure of " ++ proteinDesign.pdb
                }
            , column
                [ height fill
                , width fill
                , spacing 10
                , Font.alignLeft
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
                    , el [ Font.italic ] (text <| proteinDesign.citation)
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
            [ paragraph
                Style.h2Font
                [ text "Sequence"
                ]
            , table []
                { data = proteinDesign.chains
                , columns =
                    [ { header = paragraph [ Font.bold, paddingXY 0 10 ] [ text "Chain ID" ]
                      , width = fill
                      , view =
                            \chain ->
                                text chain.chain_id
                      }
                    , { header = paragraph [ Font.bold, paddingXY 0 10 ] [ text "Sequence" ]
                      , width = fill
                      , view =
                            \chain ->
                                text chain.chain_seq
                      }
                    ]
                }
            ]
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
        , icon = checkboxIcon
        , checked =
            case Dict.get dictKey model.classificationCheckboxDict of
                Just value ->
                    value

                Nothing ->
                    False
        , label =
            case Dict.get dictKey model.classificationCheckboxDict of
                Just True ->
                    Input.labelRight [ centerY, width fill ] (paragraph ([ Font.bold ] ++ Style.bodyFont) <| [ text <| keyToLabel dictKey ])

                _ ->
                    Input.labelRight [ centerY, width fill ] (paragraph Style.bodyFont <| [ text <| keyToLabel dictKey ])
        }


voteCheckbox : ( Model, String ) -> Element Msg
voteCheckbox ( model, dictKey ) =
    Input.checkbox [ paddingXY 3 10, alignTop ]
        { onChange = \checkboxStatus -> UpdateVoteCheckbox dictKey checkboxStatus
        , icon = checkboxIcon
        , checked =
            case Dict.get dictKey model.voteCheckboxDict of
                Just value ->
                    value

                Nothing ->
                    False
        , label =
            case Dict.get dictKey model.voteCheckboxDict of
                Just True ->
                    Input.labelRight [ centerY, width fill ] (paragraph ([ Font.bold ] ++ Style.bodyFont) <| [ text <| "Yes, I vote to REMOVE this design." ])

                _ ->
                    Input.labelRight [ centerY, width fill ] (paragraph Style.bodyFont <| [ text <| "Yes, I vote to REMOVE this design." ])
        }


checkboxIcon : Bool -> Element msg
checkboxIcon isChecked =
    el
        [ width <| px 25
        , height <| px 25
        , padding 4
        ]
    <|
        el
            [ width fill
            , height fill
            , Border.rounded 3
            , Background.color <|
                if isChecked then
                    rgb255 104 176 171

                else
                    rgb255 255 255 255
            ]
            none


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
