module Pages.Review.DesignId_ exposing (Model, Msg, page)

import Components.Title
import Date
import DesignFilter exposing (defaultKeys)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
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
    , classificationCheckbox : Dict String Bool
    , classification : Dict String Bool
    , voteKeep : Bool
    , comment : String
    }


init : String -> () -> ( Model, Effect Msg )
init designId _ =
    ( { designId = designId
      , reviewer = "Marta Chronowska (default)"
      , classificationCheckbox = classificationCheckboxDict
      , classification = Dict.empty
      , voteKeep = True
      , comment = ""
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



-- UPDATE


type Msg
    = NoOp
    | UpdateDesignClassification String Bool
    | UpdateCheckbox String Bool
    | ClearCheckbox String


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

        UpdateCheckbox key checkboxStatus ->
            { model | classificationCheckbox = Dict.insert key checkboxStatus model.classificationCheckbox }
                |> (if checkboxStatus then
                        update (UpdateDesignClassification key checkboxStatus)

                    else
                        update (ClearCheckbox key)
                   )

        ClearCheckbox key ->
            ( { model | classificationCheckbox = Dict.remove key model.classificationCheckbox }
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
    , attributes = [ width fill, Background.color <| rgb255 255 176 156 ]
    , element = details <| Dict.get model.designId shared.designs
    }


details : Maybe ProteinDesign -> Element msg
details mDesign =
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
        , reviewArea
        ]


designDetailsView : ProteinDesign -> Element msg
designDetailsView proteinDesign =
    column
        ([ centerX
         , width fill
         , padding 20
         , spacing 30
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


reviewArea : Element msg
reviewArea =
    column []
        [ classificationArea
        , votingArea
        , commentArea
        ]


classificationArea : Element msg
classificationArea =
    paragraph
        Style.h2Font
        [ text "Classification" ]


votingArea : Element msg
votingArea =
    paragraph
        Style.h2Font
        [ text "Vote to dispose" ]


commentArea : Element msg
commentArea =
    paragraph
        Style.h2Font
        [ text "Comments" ]



{--
            , row [] <| List.map (\label -> checkbox ( model, label )) [ defaultKeys.classificationOriginalDeNovoKey, defaultKeys.classificationRelativeDeNovoKey, defaultKeys.classificationSmallKey, defaultKeys.classificationEngineeredKey, defaultKeys.classificationUnknownKey ]
        ]

checkbox : ( Model, String ) -> Element Msg
checkbox ( model, dictKey ) =
    Input.checkbox [ padding 3 ]
        { onChange = \checkboxStatus -> UpdateCheckbox dictKey checkboxStatus
        , icon = checkboxIcon
        , checked =
            case Dict.get dictKey model.checkbox of
                Just value ->
                    value

                Nothing ->
                    False
        , label = Input.labelRight [ centerY, width fill ] (paragraph [] <| [ text <| keyToLabel dictKey ])
        }
--}


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
