module Pages.Designs.DesignId_ exposing (Model, Msg, page)

import Components.Title
import Date
import Dict
import Effect exposing (Effect)
import Element exposing (..)
import Element.Font as Font
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
            text "Design Details"
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
