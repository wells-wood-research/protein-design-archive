module Pages.Designs.DesignId_ exposing (Model, Msg, page)

import Components.Title
import Date
import Effect exposing (Effect)
import Element exposing (..)
import Element.Font as Font
import Page exposing (Page)
import ProteinDesign exposing (ProteinDesign, classificationToString, keywordToString)
import Route exposing (Route)
import Shared
import Style
import View exposing (View)


page : Shared.Model -> Route { designId : String } -> Page Model Msg
page _ route =
    Page.new
        { init = init route.params.designId
        , update = update
        , subscriptions = subscriptions
        , view = view >> Components.Title.view
        }



-- INIT


type Model
    = LoadedDesign ProteinDesign
    | DesignDoesNotExist


init : String -> () -> ( Model, Effect Msg )
init _ _ =
    ( DesignDoesNotExist
    , Effect.none
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


view : Model -> View Msg
view model =
    { title = "Design Details"
    , attributes = [ width fill ]
    , element = details model
    }


details : Model -> Element msg
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
            DesignDoesNotExist ->
                paragraph
                    (Style.bodyFont
                        ++ [ Font.center ]
                    )
                    [ text "This design does not exist."
                    ]

            LoadedDesign design ->
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
                    []
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
                    []
                    [ "Deposition Date: "
                        ++ Date.toIsoString proteinDesign.depositionDate
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
                    , el [ Font.italic ] (text <| keywordToString proteinDesign.structuralKeywords)
                    ]
                , paragraph
                    []
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
                    []
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
                Style.h2Font
                [ text "Sequence"
                ]
            , column
                (width (fill |> maximum 800) :: Style.monospacedFont)
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
