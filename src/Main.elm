module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyPress)
import Date exposing (Date, Unit(..))
import Decoders exposing (..)
import DesignFilter exposing (DesignFilter(..))
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font exposing (center)
import Element.Input as Input exposing (checkbox)
import FeatherIcons exposing (alignCenter)
import Html exposing (Html, div, input)
import Html.Attributes exposing (placeholder, type_)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, list)
import List exposing (minimum)
import List.Extra as ListEx
import ProteinDesign exposing (..)
import Random
import Svg as S
import Svg.Attributes as SAtt
import Svg.Events as SEvents
import Task
import Time exposing (Month(..))


searchTextKey : String
searchTextKey =
    "search-text-string"


classificationOriginalDeNovoKey : String
classificationOriginalDeNovoKey =
    "design-classification-original"


classificationRelativeDeNovoKey : String
classificationRelativeDeNovoKey =
    "design-classification-relative"


classificationSmallKey : String
classificationSmallKey =
    "design-classification-small"


classificationEngineeredKey : String
classificationEngineeredKey =
    "design-classification-engineered"


classificationUnknownKey : String
classificationUnknownKey =
    "design-classification-unknown"


keywordSyntheticKey : String
keywordSyntheticKey =
    "design-keyword-synthetic"


keywordDeNovoKey : String
keywordDeNovoKey =
    "design-keyword-de-novo"


keywordNovelKey : String
keywordNovelKey =
    "design-keyword-novel"


keywordDesignedKey : String
keywordDesignedKey =
    "design-keyword-designed"


keywordProteinBindingKey : String
keywordProteinBindingKey =
    "design-keyword-protein-binding"


keywordMetalBindingKey : String
keywordMetalBindingKey =
    "design-keyword-metal-binding"


keywordTranscriptionKey : String
keywordTranscriptionKey =
    "design-keyword-transcription"


keywordGrowthKey : String
keywordGrowthKey =
    "design-keyword-growth"


keywordStructuralKey : String
keywordStructuralKey =
    "design-keyword-structural"


keywordAlphaHelicalBundleKey : String
keywordAlphaHelicalBundleKey =
    "design-keyword-alpha-helical-bundle"


keywordBetaBetaAlphaKey : String
keywordBetaBetaAlphaKey =
    "design-keyword-beta-beta-alpha"


keywordCoiledCoilKey : String
keywordCoiledCoilKey =
    "design-keyword-coiled-coil"


keywordUnknownFunctionKey : String
keywordUnknownFunctionKey =
    "design-keyword-unknown"


dateStartKey : String
dateStartKey =
    "deposition-date-start"


dateEndKey : String
dateEndKey =
    "deposition-date-end"


defaultStartDate : Date
defaultStartDate =
    Date.fromCalendarDate 1900 Jan 1


defaultEndDate : Date
defaultEndDate =
    Date.fromCalendarDate 2100 Dec 31


checkboxDict : Dict String Bool
checkboxDict =
    Dict.fromList
        [ ( classificationOriginalDeNovoKey, False )
        , ( classificationRelativeDeNovoKey, False )
        , ( classificationSmallKey, False )
        , ( classificationEngineeredKey, False )
        , ( classificationUnknownKey, False )
        , ( keywordSyntheticKey, False )
        , ( keywordDeNovoKey, False )
        , ( keywordNovelKey, False )
        , ( keywordDesignedKey, False )
        , ( keywordProteinBindingKey, False )
        , ( keywordMetalBindingKey, False )
        , ( keywordTranscriptionKey, False )
        , ( keywordGrowthKey, False )
        , ( keywordStructuralKey, False )
        , ( keywordAlphaHelicalBundleKey, False )
        , ( keywordBetaBetaAlphaKey, False )
        , ( keywordCoiledCoilKey, False )
        , ( keywordUnknownFunctionKey, False )
        ]


keyToLabel : String -> String
keyToLabel label =
    DesignFilter.toString <| DesignFilter.toDesignFilter label



---- MODEL ----


type alias Model =
    { proteinStructures : List ProteinStructure
    , proteinDesigns : List ProteinDesign
    , focusedProteinDesign : Maybe ProteinDesign
    , randomNumbers : List Int
    , filters : Dict String DesignFilter
    , checkbox : Dict String Bool
    , mStartDate : Maybe String
    , mEndDate : Maybe String
    }


type alias ProteinStructure =
    Decoders.ProteinStructure


init : ( Model, Cmd Msg )
init =
    ( { proteinStructures = []
      , proteinDesigns = []
      , focusedProteinDesign = Nothing
      , randomNumbers = []
      , filters = Dict.empty
      , checkbox = checkboxDict
      , mStartDate = Just ""
      , mEndDate = Just ""
      }
    , Cmd.batch
        [ Random.generate RandomNumbers gen1000Numbers
        , Task.succeed SendDesignsHttpRequest
            |> Task.perform identity
        ]
    )


gen1000Numbers : Random.Generator (List Int)
gen1000Numbers =
    Random.list 100 (Random.int -2 2)



---- UPDATE ----


type Msg
    = ClickedDesign Int
    | RandomNumbers (List Int)
    | UpdateFilters String DesignFilter
    | UpdateCheckbox String Bool
    | UpdateStartDateTextField String
    | UpdateEndDateTextField String
    | ClearFilter String
    | ClearAllFilters
    | SendDesignsHttpRequest
    | DesignsDataReceived (Result Http.Error (List ProteinStructure))
    | ProcessedProteinDesigns (List ProteinDesign)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedDesign index ->
            ( { model | focusedProteinDesign = ListEx.getAt index model.proteinDesigns }, Cmd.none )

        RandomNumbers numbers ->
            ( { model | randomNumbers = numbers }, Cmd.none )

        UpdateFilters key newFilter ->
            ( { model | filters = Dict.insert key newFilter model.filters }
            , Cmd.none
            )

        UpdateCheckbox key checkboxStatus ->
            ( { model | checkbox = Dict.insert key checkboxStatus model.checkbox }
            , if checkboxStatus then
                Task.succeed (UpdateFilters key (DesignFilter.toDesignFilter key))
                    |> Task.perform identity

              else
                Task.succeed (ClearFilter key)
                    |> Task.perform identity
            )

        UpdateStartDateTextField phrase ->
            ( { model | mStartDate = phrase |> Just }
            , if String.right 1 phrase /= "-" then
                case Date.fromIsoString phrase of
                    Err _ ->
                        Task.succeed (UpdateFilters dateStartKey (DateStart defaultStartDate))
                            |> Task.perform identity

                    Ok date ->
                        Task.succeed (UpdateFilters dateStartKey (DateStart date))
                            |> Task.perform identity

              else
                case Date.fromIsoString (String.dropRight 1 phrase) of
                    Err _ ->
                        Task.succeed (UpdateFilters dateStartKey (DateStart defaultStartDate))
                            |> Task.perform identity

                    Ok date ->
                        Task.succeed (UpdateFilters dateStartKey (DateStart date))
                            |> Task.perform identity
            )

        UpdateEndDateTextField phrase ->
            ( { model | mEndDate = phrase |> Just }
            , if String.right 1 phrase /= "-" then
                case Date.fromIsoString phrase of
                    Err _ ->
                        Task.succeed (UpdateFilters dateEndKey (DateEnd defaultEndDate))
                            |> Task.perform identity

                    Ok date ->
                        Task.succeed (UpdateFilters dateEndKey (DateEnd date))
                            |> Task.perform identity

              else
                case Date.fromIsoString (String.dropRight 1 phrase) of
                    Err _ ->
                        Task.succeed (UpdateFilters dateEndKey (DateEnd defaultEndDate))
                            |> Task.perform identity

                    Ok date ->
                        Task.succeed (UpdateFilters dateEndKey (DateEnd date))
                            |> Task.perform identity
            )

        ClearFilter key ->
            ( { model | filters = Dict.remove key model.filters }
            , Cmd.none
            )

        ClearAllFilters ->
            ( { model
                | filters = Dict.empty
                , checkbox = checkboxDict
              }
            , Cmd.none
            )

        SendDesignsHttpRequest ->
            ( { model | proteinStructures = [] }, getData )

        DesignsDataReceived result ->
            case result of
                Ok proteinDataJson ->
                    let
                        ( proteinDesigns, _ ) =
                            processProteinStructures proteinDataJson
                    in
                    ( { model | proteinStructures = proteinDataJson, proteinDesigns = proteinDesigns }, Cmd.none )

                Err str ->
                    let
                        _ =
                            Debug.log "decode error" str
                    in
                    ( { model | proteinStructures = [] }, Cmd.none )

        ProcessedProteinDesigns processedDesigns ->
            ( { model | proteinDesigns = processedDesigns }, Cmd.none )



---- INITIATE STRUCTURES ----


getData : Cmd Msg
getData =
    Http.get
        { url = "/designs.json"
        , expect = Http.expectJson DesignsDataReceived proteinStructuresDecoder
        }


proteinStructuresDecoder : Decoder (List ProteinStructure)
proteinStructuresDecoder =
    list Decoders.proteinStructureDecoder


processProteinStructures : List ProteinStructure -> ( List ProteinDesign, Cmd Msg )
processProteinStructures proteinStructures =
    let
        processedDesigns =
            proteinStructures |> List.filterMap proteinStructureToDesign
    in
    ( processedDesigns
    , Task.succeed (ProcessedProteinDesigns processedDesigns)
        |> Task.perform identity
    )


proteinStructureToDesign : ProteinStructure -> Maybe ProteinDesign
proteinStructureToDesign proteinStructure =
    let
        pdbCode =
            String.toLower proteinStructure.identifier

        structuralKeywords =
            stringToKeyword <| String.trim proteinStructure.keywords

        depositionDate =
            Date.fromIsoString proteinStructure.date
                |> Result.withDefault (Date.fromCalendarDate 1900 Jan 1)

        picturePath =
            "https://cdn.rcsb.org/images/structures/"
                ++ String.slice 1 3 pdbCode
                ++ "/"
                ++ pdbCode
                ++ "/"
                ++ pdbCode
                ++ "_assembly-1.jpeg"

        doiLink =
            proteinStructure.doi

        sequences =
            List.concatMap (\polyEntity -> extractSequencesFromPolyEntity polyEntity) proteinStructure.data.polymerEntities

        classification =
            stringToClassfication proteinStructure.classification

        authors =
            proteinStructure.authors
                |> List.map (\author -> String.join " " author.forename ++ " " ++ String.join " " author.surname)
                |> String.join ", "

        pubmedID =
            proteinStructure.pubmed_id

        abstract =
            proteinStructure.abstract
    in
    ProteinDesign pdbCode structuralKeywords depositionDate picturePath doiLink sequences classification authors pubmedID abstract |> Just



---- VIEW ----


view : Model -> Html Msg
view model =
    layout [] <|
        portraitView model


portraitView : Model -> Element Msg
portraitView model =
    column
        [ width (fill |> maximum 1080)
        , centerX
        , height fill
        ]
        [ title
        , row [ width fill ]
            [ timeline model
            , details model.focusedProteinDesign
            , sidebar model
            ]
        ]


title : Element msg
title =
    el
        (titleFont
            ++ [ width fill
               , padding 30
               , Background.color <| rgb255 143 192 169
               , Font.center
               ]
        )
    <|
        paragraph [] [ text "Protein Design Archive" ]


sidebar : Model -> Element Msg
sidebar model =
    column
        (bodyFont
            ++ [ width <| fillPortion 3
               , height fill
               , Background.color <| rgb255 105 109 125
               , paddingXY 10 10
               , spacing 10
               ]
        )
        [ searchArea
            (Dict.get searchTextKey model.filters
                |> Maybe.map DesignFilter.toString
            )
        , filterArea model
        ]


searchArea : Maybe String -> Element Msg
searchArea mSearchPhrase =
    row
        bodyFont
        [ searchField mSearchPhrase
        , searchButton
        ]


searchButton : Element Msg
searchButton =
    Input.button
        [ padding 5
        ]
        { label = sidebarButton FeatherIcons.search
        , onPress = ClearAllFilters |> Just
        }


searchField : Maybe String -> Element Msg
searchField mCurrentText =
    Input.text
        []
        { onChange = \string -> UpdateFilters searchTextKey (ContainsText string)
        , text = Maybe.withDefault "" mCurrentText
        , placeholder = Just <| Input.placeholder [] (text "Enter search phrase here")
        , label = Input.labelHidden "Filter Designs Search Box"
        }


filterArea : Model -> Element Msg
filterArea model =
    column
        bodyFont
        [ row []
            [ paragraph []
                [ text "Press button to clear filters:" ]
            , filterButton
            ]
        , filterField model
        ]


filterButton : Element Msg
filterButton =
    Input.button
        [ padding 5 ]
        { label = sidebarButton FeatherIcons.filter
        , onPress = ClearAllFilters |> Just
        }


filterField : Model -> Element Msg
filterField model =
    column
        [ paddingXY 0 10 ]
        [ paragraph [ Font.bold, paddingXY 0 10 ]
            [ text "Deposit date:" ]
        , dateStartField model
        , dateEndField model
        , paragraph [ Font.bold, paddingXY 0 10 ]
            [ text "Classification:" ]
        , classificationFilter model
        , paragraph [ Font.bold, paddingXY 0 10 ]
            [ text "Keywords:" ]
        , keywordsFilter model
        ]


dateStartField : Model -> Element Msg
dateStartField model =
    column [ spacing 3 ]
        [ row [ width fill, spacing 5 ]
            [ Input.text
                [ width <| fillPortion 5 ]
                { onChange =
                    \string ->
                        UpdateStartDateTextField string
                , text = Maybe.withDefault "" model.mStartDate
                , placeholder = Just <| Input.placeholder [] (text "YYYY-MM-DD")
                , label = Input.labelHidden "Filter Designs by Date - start"
                }
            , paragraph [ width <| fillPortion 3 ] [ text <| "Start date" ]
            ]
        ]


dateEndField : Model -> Element Msg
dateEndField model =
    column [ spacing 3 ]
        [ row [ width fill, spacing 5 ]
            [ Input.text
                [ width <| fillPortion 5 ]
                { onChange =
                    \string ->
                        UpdateEndDateTextField string
                , text = Maybe.withDefault "" model.mEndDate
                , placeholder = Just <| Input.placeholder [] (text "YYYY-MM-DD")
                , label = Input.labelHidden "Filter Designs by Date - end"
                }
            , paragraph [ width <| fillPortion 3 ] [ text <| "End date" ]
            ]
        ]


classificationFilter : Model -> Element Msg
classificationFilter model =
    column []
        [ filterCheckbox ( model, classificationOriginalDeNovoKey )
        , filterCheckbox ( model, classificationRelativeDeNovoKey )
        , filterCheckbox ( model, classificationSmallKey )
        , filterCheckbox ( model, classificationEngineeredKey )
        , filterCheckbox ( model, classificationUnknownKey )
        ]


keywordsFilter : Model -> Element Msg
keywordsFilter model =
    column []
        [ filterCheckbox ( model, keywordDeNovoKey )
        , filterCheckbox ( model, keywordSyntheticKey )
        , filterCheckbox ( model, keywordNovelKey )
        , filterCheckbox ( model, keywordDesignedKey )
        , filterCheckbox ( model, keywordProteinBindingKey )
        , filterCheckbox ( model, keywordMetalBindingKey )
        , filterCheckbox ( model, keywordTranscriptionKey )
        , filterCheckbox ( model, keywordGrowthKey )
        , filterCheckbox ( model, keywordStructuralKey )
        , filterCheckbox ( model, keywordAlphaHelicalBundleKey )
        , filterCheckbox ( model, keywordBetaBetaAlphaKey )
        , filterCheckbox ( model, keywordCoiledCoilKey )
        , filterCheckbox ( model, keywordUnknownFunctionKey )
        ]


filterCheckbox : ( Model, String ) -> Element Msg
filterCheckbox ( model, dictKey ) =
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


sidebarButton : FeatherIcons.Icon -> Element Msg
sidebarButton icon =
    el
        [ Border.solid
        , Border.width 2
        ]
        (icon
            |> FeatherIcons.toHtml []
            |> html
        )


timeline : Model -> Element Msg
timeline { proteinDesigns, filters, randomNumbers } =
    let
        filterValues =
            Dict.values filters

        filteredDesigns =
            proteinDesigns
                |> List.filterMap (\d -> DesignFilter.meetsAllFilters filterValues d)

        timelineDates =
            getFirstAndLastDate proteinDesigns
    in
    column
        (bodyFont
            ++ [ width <| fillPortion 1
               , height fill
               , Font.center
               , Background.color <| rgb255 105 109 125
               ]
        )
        [ paragraph [ paddingXY 20 10 ] [ text "Timeline" ]
        , column [ width <| px 80, centerX ]
            [ paragraph
                (bodyFont
                    ++ [ Font.center
                       , paddingXY 5 10
                       ]
                )
                [ timelineDates.firstDate
                    |> Date.year
                    |> String.fromInt
                    |> text
                ]
            , html (timelineGraphic timelineDates randomNumbers filteredDesigns)
            , paragraph
                (bodyFont
                    ++ [ Font.center
                       , paddingXY 5 10
                       ]
                )
                [ timelineDates.lastDate
                    |> Date.year
                    |> String.fromInt
                    |> text
                ]
            ]
        ]


timelineGraphic : { firstDate : Date, lastDate : Date } -> List Int -> List ProteinDesign -> Html Msg
timelineGraphic { firstDate, lastDate } randomNumbers proteinDesigns =
    let
        -- plot dimensions
        width =
            20

        height =
            360

        radius =
            2
    in
    S.svg
        [ SAtt.width "100%"
        , SAtt.height "100%"
        , "0 0 "
            ++ String.fromInt width
            ++ " "
            ++ String.fromInt height
            |> SAtt.viewBox
        ]
        ([ S.line
            [ SAtt.x1 <| String.fromInt <| width // 2
            , SAtt.y1 <| String.fromInt <| 0
            , SAtt.x2 <| String.fromInt <| width // 2
            , SAtt.y2 <| String.fromInt <| height
            , SAtt.stroke "black"
            , SAtt.strokeWidth "2"
            ]
            []
         , S.line
            [ SAtt.x1 <| String.fromInt <| (width // 2) - 4
            , SAtt.y1 "1"
            , SAtt.x2 <| String.fromInt <| (width // 2) + 4
            , SAtt.y2 "1"
            , SAtt.stroke "black"
            , SAtt.strokeWidth "2"
            ]
            []
         , S.line
            [ SAtt.x1 <| String.fromInt <| (width // 2) - 4
            , SAtt.y1 <| String.fromInt <| height - 1
            , SAtt.x2 <| String.fromInt <| (width // 2) + 4
            , SAtt.y2 <| String.fromInt <| height - 1
            , SAtt.stroke "black"
            , SAtt.strokeWidth "2"
            ]
            []
         ]
            ++ (List.indexedMap
                    Tuple.pair
                    proteinDesigns
                    |> List.map2 Tuple.pair randomNumbers
                    |> List.map
                        (designToMarker
                            { width = width
                            , height = height
                            , radius = radius
                            , firstDate = firstDate
                            , lastDate = lastDate
                            }
                        )
               )
        )


getFirstAndLastDate : List ProteinDesign -> { firstDate : Date, lastDate : Date }
getFirstAndLastDate proteinDesigns =
    let
        sortedDesigns =
            List.sortWith
                (\a b -> Date.compare a.depositionDate b.depositionDate)
                proteinDesigns

        firstDesignDate =
            List.head sortedDesigns
                |> Maybe.map .depositionDate
                |> Maybe.withDefault defaultStartDate

        lastDesignDate =
            List.reverse sortedDesigns
                |> List.head
                |> Maybe.map .depositionDate
                |> Maybe.withDefault defaultEndDate
    in
    { firstDate = firstDesignDate, lastDate = lastDesignDate }


designToMarker :
    { width : Int
    , height : Int
    , radius : Int
    , firstDate : Date
    , lastDate : Date
    }
    -> ( Int, ( Int, ProteinDesign ) )
    -> S.Svg Msg
designToMarker { width, height, radius, firstDate, lastDate } ( randomShift, ( index, proteinDesign ) ) =
    S.circle
        [ SAtt.cx <| String.fromInt <| width // 2 + randomShift
        , SAtt.cy <|
            String.fromInt <|
                (dateToPosition
                    { firstDate = firstDate
                    , lastDate = lastDate
                    , date = proteinDesign.depositionDate
                    , height = height
                    , radius = radius
                    }
                    + randomShift
                )
        , SAtt.r <| String.fromInt radius
        , SAtt.fill <| classificationToColour proteinDesign.classification
        , SAtt.strokeWidth "0.5"
        , SAtt.stroke "black"
        , SAtt.cursor "pointer"
        , SEvents.onClick <| ClickedDesign index
        ]
        []


dateToPosition :
    { firstDate : Date
    , lastDate : Date
    , date : Date
    , height : Int
    , radius : Int
    }
    -> Int
dateToPosition { firstDate, lastDate, date, height, radius } =
    let
        dateRange =
            Date.diff Days lastDate firstDate
                |> toFloat

        dateDelta =
            Date.diff Days date firstDate
                |> toFloat

        fraction =
            dateDelta / dateRange
    in
    fraction
        * toFloat (height - (2 * radius))
        |> round
        |> (+) 3


details : Maybe ProteinDesign -> Element msg
details mDesign =
    column
        [ centerX
        , width <|
            fillPortion 11
        , height fill
        ]
        [ el
            (h1Font
                ++ [ width fill
                   , padding 20
                   , Background.color <| rgb255 255 255 255
                   ]
            )
          <|
            text "Design Details"
        , case mDesign of
            Nothing ->
                paragraph
                    (bodyFont
                        ++ [ Font.center ]
                    )
                    [ text "Click on the timeline for detailed information about a design."
                    ]

            Just design ->
                designDetailsView design
        ]


designDetailsView : ProteinDesign -> Element msg
designDetailsView proteinDesign =
    column
        [ centerX
        , width fill
        , padding 20
        , spacing 30
        ]
        [ wrappedRow
            [ height fill
            , width fill
            , spacing 10
            ]
            [ image
                []
                { src = proteinDesign.picturePath
                , description = "Structure of " ++ proteinDesign.pdbCode
                }
            , column
                [ height fill
                , width fill
                , spacing 10
                , Font.alignLeft
                ]
                [ paragraph
                    bodyFont
                    [ text "PDB Code: "
                    , link
                        [ Font.color <| rgb255 104 176 171
                        , Font.underline
                        ]
                        { url =
                            "https://www.rcsb.org/structure/"
                                ++ proteinDesign.pdbCode
                        , label =
                            proteinDesign.pdbCode
                                |> text
                        }
                    ]
                , paragraph
                    bodyFont
                    [ "Deposition Date: "
                        ++ Date.toIsoString proteinDesign.depositionDate
                        |> text
                    ]
                , paragraph
                    bodyFont
                    [ "Design Classification: "
                        ++ classificationToString proteinDesign.classification
                        |> text
                    ]
                , paragraph
                    bodyFont
                    [ text "Structural Keywords: "
                    , el [ Font.italic ] (text <| keywordToString proteinDesign.structuralKeywords)
                    ]
                , paragraph
                    bodyFont
                    [ text "Publication: "
                    , link
                        [ Font.color <| rgb255 104 176 171
                        , Font.underline
                        ]
                        { url =
                            proteinDesign.doiLink
                        , label =
                            proteinDesign.doiLink
                                |> text
                        }
                    ]
                , paragraph
                    bodyFont
                    [ "Authors: "
                        ++ proteinDesign.authors
                        |> text
                    ]
                ]
            ]
        , column
            [ width fill
            , spacing 20
            ]
            [ paragraph
                h2Font
                [ text "Sequence"
                ]
            , column
                (width (fill |> maximum 800) :: monospacedFont)
              <|
                List.indexedMap
                    (\index str ->
                        paragraph []
                            [ text <|
                                "chain "
                                    ++ String.fromInt (index + 1)
                                    ++ ": "
                                    ++ str
                            ]
                    )
                    proteinDesign.sequences
            ]
        , column
            [ width fill
            , spacing 20
            ]
            [ paragraph
                h2Font
                [ text "Description"
                ]
            , paragraph
                (bodyFont
                    ++ [ Font.justify ]
                )
                [ proteinDesign.abstract
                    |> text
                ]
            ]

        {-
           , paragraph
               h2Font
               [ text "Structural Similarity"
               ]
           , paragraph
               h2Font
               [ text "Homologues"
               ]
           , paragraph
               h2Font
               [ text "Sequence Metrics"
               ]
           , paragraph
               h2Font
               [ text "DE-STRESS Metrics"
               ]
        -}
        ]


extractSequencesFromPolyEntity : PolyEntities -> List String
extractSequencesFromPolyEntity polyEntity =
    let
        entityPoly =
            polyEntity.entity_poly
    in
    [ entityPoly.pdbx_seq_one_letter_code_can ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }



---- STYLE ----
-- https://coolors.co/faf3dd-c8d5b9-8fc0a9-68b0ab-696d7d
-- eggshell 250 243 221
-- tea green 200 213 185
-- cambridge blue 143 192 169
-- verdigris 104 176 171
-- payne's gray 105 109 125


titleFont : List (Attribute msg)
titleFont =
    [ Font.family
        [ Font.typeface "Barlow"
        , Font.sansSerif
        ]
    , Font.size 40
    ]


h1Font : List (Attribute msg)
h1Font =
    [ Font.family
        [ Font.typeface "Lato"
        , Font.sansSerif
        ]
    , Font.size 32
    ]


h2Font : List (Attribute msg)
h2Font =
    [ Font.family
        [ Font.typeface "Lato"
        , Font.sansSerif
        ]
    , Font.size 24
    ]


bodyFont : List (Attribute msg)
bodyFont =
    [ Font.family
        [ Font.typeface "Lato"
        , Font.sansSerif
        ]
    , Font.size 16
    , Font.alignLeft
    ]


monospacedFont : List (Attribute msg)
monospacedFont =
    [ Font.family
        [ Font.typeface "Fira Code"
        , Font.sansSerif
        ]
    , Font.size 16
    ]
