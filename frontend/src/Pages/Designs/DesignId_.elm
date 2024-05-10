module Pages.Designs.DesignId_ exposing (Model, Msg, page)

import Components.Title
import Date
import Dict
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Element.Keyed as Keyed
import FeatherIcons
import Html
import Html.Attributes as HAtt
import List.Extra as LE
import Page exposing (Page)
import ProteinDesign exposing (ProteinDesign, authorsToString, classificationToString, tagsToString)
import Route exposing (Route)
import Shared
import Style
import View exposing (View)


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
    { designId : String }


init : String -> () -> ( Model, Effect Msg )
init designId _ =
    ( { designId = designId }
    , Effect.resetViewport NoOp
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Design Details"
    , attributes = [ width fill ]
    , element =
        details shared model
    }


details : Shared.Model -> Model -> Element msg
details shared model =
    let
        mDesign =
            Dict.get model.designId shared.designs
    in
    row []
        [ column
            [ width fill ]
            [ row [ width fill, spaceEvenly ]
                [ browseButton shared model "back"
                , el
                    (Style.h1Font
                        ++ [ centerX
                           , padding 20
                           ]
                    )
                  <|
                    text "Design Details"
                , browseButton shared model "next"
                ]
            , case mDesign of
                Nothing ->
                    paragraph
                        (Style.bodyFont
                            ++ [ Font.center ]
                        )
                        [ text "This design does not exist."
                        ]

                Just design ->
                    designDetailsView design
            ]
        ]


getNextDesign : Shared.Model -> Model -> String -> Maybe String
getNextDesign shared model direction =
    let
        list =
            Dict.keys shared.designs

        currentIndex =
            LE.elemIndex model.designId list

        lastIndex =
            (\i -> i - 1) <| List.length list
    in
    case currentIndex of
        Just index ->
            case direction of
                "next" ->
                    if index == lastIndex then
                        LE.getAt 0 list

                    else
                        LE.getAt (index + 1) list

                "back" ->
                    if index == 0 then
                        LE.last list

                    else
                        LE.getAt (index - 1) list

                _ ->
                    LE.getAt index list

        _ ->
            LE.getAt 5 list


browseButton : Shared.Model -> Model -> String -> Element msg
browseButton shared model direction =
    link
        []
        { url =
            case getNextDesign shared model direction of
                Nothing ->
                    "/"

                Just designId ->
                    "/designs/" ++ designId
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
         , height <| px 1200
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
                ]
                (image
                    [ width <| px 300
                    ]
                    { src = proteinDesign.picture_path
                    , description = "Structure of " ++ proteinDesign.pdb
                    }
                )
            , column
                [ height fill
                , width fill
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
            [ column
                Style.h2Font
                [ text "Structure"
                ]
            , Keyed.el
                [ height <| px 400
                , width <| px 850
                , padding 5
                , Border.width 2
                , Border.rounded 3
                , Border.color <| rgb255 220 220 220
                ]
                ( proteinDesign.pdb
                , Html.node "ngl-viewer"
                    [ HAtt.id "viewer"
                    , HAtt.style "width" "100%"
                    , HAtt.style "height" "100%"
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
                  , width = px 150
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
                  , width = fill
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
