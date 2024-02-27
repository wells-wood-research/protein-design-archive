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
import Element.Input as Input
import FeatherIcons
import Html exposing (Html, div, input)
import Html.Attributes exposing (placeholder, type_)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, list)
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



---- MODEL ----


type alias Model =
    { proteinStructures : List ProteinStructure
    , proteinDesigns : List ProteinDesign
    , focusedProteinDesign : Maybe ProteinDesign
    , randomNumbers : List Int
    , filters : Dict String DesignFilter

    -- To be removed
    , searchPhrase : String
    , checkbox : Bool
    , checkboxFalse : Bool
    , filter : Filter
    }


type alias ProteinStructure =
    Decoders.ProteinStructure


type alias Filter =
    { classificationOriginal : Bool
    , classificationRelative : Bool
    , classificationSmall : Bool
    , classificationEngineered : Bool
    , classificationUnknown : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { proteinStructures = []
      , proteinDesigns = []
      , focusedProteinDesign = Nothing
      , randomNumbers = []
      , filters = Dict.empty
      , searchPhrase = ""
      , checkbox = False
      , checkboxFalse = False
      , filter =
            { classificationOriginal = False
            , classificationRelative = False
            , classificationSmall = False
            , classificationEngineered = False
            , classificationUnknown = False
            }
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
    | SearchInput String
    | SearchSubmit
    | ClearSearchField
    | SendDesignsHttpRequest
    | DesignsDataReceived (Result Http.Error (List ProteinStructure))
    | ProcessedProteinDesigns (List ProteinDesign)
    | UserToggledCheckbox Bool


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

        SearchInput newSearchPhrase ->
            ( { model | searchPhrase = newSearchPhrase }, Cmd.none )

        SearchSubmit ->
            if String.trim model.searchPhrase /= "" then
                ( { model | focusedProteinDesign = ListEx.getAt 4 model.proteinDesigns }, Cmd.none )

            else
                ( model, Cmd.none )

        ClearSearchField ->
            ( model, Cmd.none )

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

        UserToggledCheckbox checkbox ->
            if checkbox then
                ( { model | focusedProteinDesign = ListEx.getAt 3 model.proteinDesigns }, Cmd.none )

            else
                ( { model | focusedProteinDesign = Nothing }, Cmd.none )



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
            proteinStructure.keywords

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
            rawStringToClassfication proteinStructure.classification

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
            ++ [ width (fill |> maximum 150)
               , height fill
               , Font.center
               , Background.color <| rgb255 105 109 125
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
        (bodyFont
            ++ [ Font.alignLeft
               , Background.color <| rgb255 105 109 125
               , paddingXY 0 10
               ]
        )
        [ elmUiSearchField mSearchPhrase
        , searchButton
        ]


searchButton : Element Msg
searchButton =
    Input.button
        [ padding 3
        ]
        { label = sidebarButton FeatherIcons.search
        , onPress = SearchSubmit |> Just
        }


elmUiSearchField : Maybe String -> Element Msg
elmUiSearchField mCurrentText =
    Input.text
        []
        { onChange = \string -> UpdateFilters searchTextKey (ContainsText string)
        , text = Maybe.withDefault "" mCurrentText
        , placeholder = Just <| Input.placeholder [] (text "Enter search phrase here")
        , label = Input.labelHidden "Filter Designs Search Box"
        }


searchField : Element Msg
searchField =
    html <|
        div [ Html.Attributes.style "padding" "5px" ]
            [ input
                [ type_ "text"
                , placeholder "Enter search phrase here"
                , onInput SearchInput
                , onClick ClearSearchField
                , Html.Attributes.style "width" "90%"
                ]
                []
            ]


filterArea : Model -> Element Msg
filterArea model =
    column
        (bodyFont
            ++ [ Font.alignLeft
               , Background.color <| rgb255 105 109 125
               , width fill
               ]
        )
        [ row []
            [ paragraph [ paddingXY 5 0 ]
                [ text "Press button to filter:" ]
            , filterButton
            ]
        , filterField model
        ]


filterButton : Element Msg
filterButton =
    Input.button
        [ padding 3
        , alignTop
        ]
        { label = sidebarButton FeatherIcons.filter
        , onPress = ClickedDesign 2 |> Just
        }


filterField : Model -> Element Msg
filterField model =
    column [ paddingXY 20 10 ]
        [ paragraph [ Font.bold, paddingXY 0 10 ]
            [ text "Classification:" ]
        , classificationFilter model
        ]


classificationFilter : Model -> Element Msg
classificationFilter model =
    column []
        [ filterCheckbox ( model, "classificationOriginalDeNovo" )
        , filterCheckbox ( model, "classificationRelativeDeNovo" )
        , filterCheckbox ( model, "classificationSmall" )
        , filterCheckbox ( model, "classificationEngineered" )
        , filterCheckbox ( model, "classificationUnknown" )
        ]


filterCheckbox : ( Model, String ) -> Element Msg
filterCheckbox ( model, checkboxType ) =
    let
        checkboxLabel =
            case checkboxType of
                "classificationOriginalDeNovo" ->
                    text "original"

                "classificationRelativeDeNovo" ->
                    text "relative"

                "classificationSmall" ->
                    text "small"

                "classificationEngineered" ->
                    text "engineered"

                "classificationUnknown" ->
                    text "unknown"

                _ ->
                    Debug.todo "error"

        filterType =
            case checkboxType of
                "classificationOriginalDeNovo" ->
                    True

                _ ->
                    False
    in
    Input.checkbox [ padding 3 ]
        { onChange = UserToggledCheckbox
        , icon = checkboxIcon
        , checked = model.checkbox
        , label = Input.labelRight [ centerY ] checkboxLabel
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
timeline { proteinDesigns, randomNumbers } =
    let
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
            , html (timelineGraphic timelineDates randomNumbers proteinDesigns)
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
                |> Maybe.withDefault (Date.fromCalendarDate 1900 Jan 1)

        lastDesignDate =
            List.reverse sortedDesigns
                |> List.head
                |> Maybe.map .depositionDate
                |> Maybe.withDefault (Date.fromCalendarDate 2100 Dec 31)
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
                            "https://www.ebi.ac.uk/pdbe/entry/pdb/"
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
                    , el [ Font.italic ] (text proteinDesign.structuralKeywords)
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
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyPress keyDecoder


keyDecoder : Json.Decode.Decoder Msg
keyDecoder =
    Json.Decode.map toKey (Json.Decode.field "key" Json.Decode.string)


toKey : String -> Msg
toKey keyValue =
    case keyValue of
        "Enter" ->
            SearchSubmit

        _ ->
            ClearSearchField



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
