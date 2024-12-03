module Pages.Help exposing (Model, Msg, page)

import Browser.Dom
import Browser.Events
import Components.Title
import Effect exposing (Effect)
import Element exposing (..)
import Element.Font as Font
import Get exposing (..)
import Page exposing (Page)
import Plots exposing (RenderPlotState(..))
import Route exposing (Route)
import Shared
import Style
import Task
import Time
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = \_ -> init shared.mScreenWidthF shared.mScreenHeightF
        , update = update
        , subscriptions = subscriptions
        , view = view >> Components.Title.view shared.mScreenWidthF
        }



-- INIT


type alias Model =
    { mScreenWidthF : Maybe Float
    , mScreenHeightF : Maybe Float
    , replotTime : Int
    , renderPlotState : RenderPlotState
    }


init : Maybe Float -> Maybe Float -> ( Model, Effect Msg )
init mSharedScreenWidthF mSharedScreenHeightF =
    ( { mScreenWidthF = mSharedScreenWidthF
      , mScreenHeightF = mSharedScreenHeightF
      , replotTime = 3
      , renderPlotState = WillRender
      }
    , Effect.batch
        [ Effect.sendCmd (Task.attempt ViewportResult Browser.Dom.getViewport)
        , Effect.resetViewport ViewportReset
        ]
    )



-- UPDATE


type Msg
    = RenderWhenReady Time.Posix
    | WindowResizes Int Int
    | ViewportResult (Result Browser.Dom.Error Browser.Dom.Viewport)
    | ViewportReset


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
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

        WindowResizes width height ->
            let
                widthF =
                    toFloat width

                heightF =
                    toFloat height
            in
            ( { model | mScreenWidthF = Just widthF, mScreenHeightF = Just heightF, renderPlotState = AwaitingRender model.replotTime }, Effect.none )

        ViewportResult result ->
            case result of
                Ok viewport ->
                    ( { model | mScreenWidthF = Just viewport.viewport.width, mScreenHeightF = Just viewport.viewport.height }, Effect.resetViewport ViewportReset )

                Err _ ->
                    ( model, Effect.none )

        _ ->
            ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize (\width height -> WindowResizes width height) ]



-- VIEW


view : Model -> View Msg
view model =
    let
        title =
            "PDA | Help"
    in
    { title = title
    , attributes =
        [ centerX
        , width
            (fill |> maximum (getScreenWidthInt model.mScreenWidthF))
        ]
    , element = helpBody model
    }


helpBody : Model -> Element msg
helpBody model =
    let
        screenWidth =
            getScreenWidthInt model.mScreenWidthF

        columnPadding =
            30

        columnWidth =
            screenWidth - 2 * columnPadding
    in
    column
        ([ alignLeft
         , width (fill |> maximum columnWidth)
         , paddingXY columnPadding 20
         , spacing 30
         , height fill
         ]
            ++ Style.bodyFont
        )
        [ paragraph
            (Style.monospacedFont
                ++ [ Font.justify
                   , width <| px columnWidth
                   ]
            )
            [ text <|
                "Thank you for visiting The Protein Design Archive, an up-to-date resource striving to systematically capture all de novo protein designs. It offers relevant information at-a-glance, and is readily searchable and analysable to gain deeper insights."
            ]
        , citationArea columnWidth
        , codeAvailabilityArea columnWidth
        , uiArea columnWidth
        , dataCollectionArea columnWidth
        , dataProcessingArea columnWidth
        ]


citationArea : Int -> Element msg
citationArea columnWidth =
    column
        [ width fill
        , spacing 10
        ]
        [ paragraph
            Style.h2Font
            [ text "Citation"
            ]
        , column
            (Style.monospacedFont
                ++ [ Font.justify
                   , width
                        (fill
                            |> maximum columnWidth
                        )
                   ]
            )
            [ paragraph
                [ width
                    (fill
                        |> maximum columnWidth
                    )
                ]
                [ text <| "If you find The PDA useful in your work, please cite as:" ]
            , column
                [ spacing 5
                , paddingXY 30 10
                , width
                    (fill
                        |> maximum
                            columnWidth
                    )
                , scrollbarX
                ]
                [ paragraph
                    [ Font.alignLeft
                    , width
                        (fill
                            |> maximum
                                (columnWidth - 30)
                        )
                    ]
                    [ text "The Protein Design Archive (PDA): insights from 40 years of protein design" ]
                , paragraph
                    [ Font.alignLeft
                    , width
                        (fill
                            |> maximum
                                (columnWidth - 30)
                        )
                    ]
                    [ text "Marta Chronowska, Michael J. Stam, Derek N. Woolfson, Luigi F. Di Constanzo, Christopher W. Wood" ]
                , paragraph
                    [ Font.alignLeft
                    , width
                        (fill
                            |> maximum
                                (columnWidth - 30)
                        )
                    ]
                    [ text "bioRxiv 2024.09.05.611465" ]
                , row
                    [ spacing 5 ]
                    [ text "doi: "
                    , newTabLink
                        [ Font.color <| rgb255 104 176 171
                        , Font.underline
                        ]
                        { url = "https://doi.org/10.1101/2024.09.05.611465"
                        , label = text "https://doi.org/10.1101/2024.09.05.611465"
                        }
                    ]
                ]
            ]
        ]


codeAvailabilityArea : Int -> Element msg
codeAvailabilityArea columnWidth =
    column
        [ width fill
        , spacing 10
        ]
        [ paragraph
            Style.h2Font
            [ text "Code Availability"
            ]
        , column
            (Style.monospacedFont
                ++ [ Font.justify
                   , width fill
                   ]
            )
            [ paragraph [ width <| px columnWidth ]
                [ paragraph
                    [ width fill ]
                    [ text <| "Code supporting The PDA is available on " ]
                , newTabLink
                    [ Font.color <| rgb255 104 176 171
                    , Font.underline
                    ]
                    { url = "https://github.com/wells-wood-research/chronowska-stam-wood-2024-protein-design-archive/tree/main"
                    , label = text <| "GitHub"
                    }
                , paragraph
                    [ width fill ]
                    [ text <| ". This includes scripts for data collection and processing, and files detailing manual curation of The PDA dataset, such as csv files listing proteins that were manually " ]
                , newTabLink
                    [ Font.color <| rgb255 104 176 171
                    , Font.underline
                    ]
                    { url = "https://github.com/wells-wood-research/chronowska-stam-wood-2024-protein-design-archive/blob/main/entries_to_manually_include.csv"
                    , label = text <| "included"
                    }
                , paragraph
                    [ width fill ]
                    [ text <| " and " ]
                , newTabLink
                    [ Font.color <| rgb255 104 176 171
                    , Font.underline
                    ]
                    { url = "https://github.com/wells-wood-research/chronowska-stam-wood-2024-protein-design-archive/blob/main/entries_to_manually_exclude.csv"
                    , label = text <| "excluded"
                    }
                , paragraph
                    [ width fill ]
                    [ text <| ", and how information was manually " ]
                , newTabLink
                    [ Font.color <| rgb255 104 176 171
                    , Font.underline
                    ]
                    { url = "https://github.com/wells-wood-research/chronowska-stam-wood-2024-protein-design-archive/blob/main/scripts/manual_data_curation.py"
                    , label = text <| " corrected"
                    }
                , paragraph
                    [ width fill ]
                    [ text <| "." ]
                ]
            ]
        ]


uiArea : Int -> Element msg
uiArea columnWidth =
    column
        [ width fill
        , spacing 10
        ]
        [ paragraph
            Style.h2Font
            [ text "Explanation of the website interface and certain choices"
            ]
        , paragraph
            Style.h3Font
            [ text "Website structure"
            ]
        , column
            (Style.monospacedFont
                ++ [ Font.justify
                   , width <| px columnWidth
                   , spacing 5
                   ]
            )
            [ paragraph [ width fill ]
                [ paragraph [ width fill ] [ text <| "The PDA is organised around the " ]
                , paragraph [ width fill, Font.bold ] [ text <| " Home page" ]
                , paragraph [ width fill ] [ text <| ", which is structured as a timeline describing the protein design field, with points representing structurally characterised designs, sorted by their RCSB PDB release date. The points display the design PDB code when hovered over, and take the user to this design's details page when right-clicked. All designs are also listed in an alphabetical order below the timeline and filtering tools. Minimalist design cards provide an overview of each entry by means of an image, PDB structure (sub)title, and authors. Again, the cards direct the user to the Design Details page when right-clicked." ]
                ]
            , paragraph [ width fill ]
                [ paragraph [ width fill ] [ text <| "The " ]
                , paragraph [ width fill, Font.bold ] [ text <| "Design Details page" ]
                , paragraph [ width fill ] [ text <| " should be self-explanatory, starting with a table summarising basic publication and experimental information, followed by an interactive 3D structure, sequence information, and ending with a description (primary publication's abstract). One point may potenially cause confusion: " ]
                , paragraph [ width fill, Font.bold ] [ text <| "related proteins" ]
                , paragraph [ width fill ] [ text <| ". This is described in detail below." ]
                ]
            ]
        , column
            (Style.monospacedFont
                ++ [ Font.justify
                   , width <| px columnWidth
                   , spacing 5
                   ]
            )
            [ paragraph
                Style.h3Font
                [ text "Related proteins"
                ]
            , paragraph [ width fill ]
                [ text <| "The Design Details table contains fields such as \"Sequence related designs (bits)\", \"Structure related proteins (LDDT)\" etc., which list hyperlinked PDB codes and values in brackets. These are entries that have come up in "
                , newTabLink
                    [ Font.color <| rgb255 104 176 171
                    , Font.underline
                    ]
                    { url = "https://www.nature.com/articles/nbt.3988"
                    , label = text <| "MMseqs2"
                    }
                , text <| " (for sequence) and "
                , newTabLink
                    [ Font.color <| rgb255 104 176 171
                    , Font.underline
                    ]
                    { url = "https://www.nature.com/articles/s41587-023-01773-0"
                    , label = text <| "Foldseek"
                    }
                , text <| " (for structure) analysis as being similar to the currently viewed design. In this context, "
                , paragraph [ Font.italic ] [ text <| " designs" ]
                , text <| " refers to related structure also found in The PDA, and"
                , paragraph [ Font.italic ] [ text <| " proteins" ]
                , text <| " refers to natural proteins, that is ones not found in The PDA."
                , paragraph [ Font.italic ] [ text <| " bits" ]
                , text <| " refers to the bit score, and "
                , paragraph [ Font.italic ] [ text <| " LDDT" ]
                , text <| " refers to the Local Distance Difference Test, as calculated by respective software."
                ]
            , paragraph [ width fill ]
                [ paragraph [ width fill ] [ text <| " We have selected values of " ]
                , paragraph [ Font.bold ] [ text <| "50 bit score and 95% LDDT as thresholds above which we list related entries." ]
                , paragraph [ width fill ] [ text <| " For designs that do not have any structures that meet these criteria, we list the most similar ones instead." ]
                ]
            ]
        ]


dataCollectionArea : Int -> Element msg
dataCollectionArea columnWidth =
    column
        [ width fill
        , spacing 10
        ]
        [ paragraph
            Style.h2Font
            [ text "Data collection"
            ]
        , column
            (Style.monospacedFont
                ++ [ Font.justify
                   , width
                        (fill
                            |> maximum columnWidth
                        )
                   ]
            )
            [ paragraph
                [ width
                    (fill
                        |> maximum columnWidth
                    )
                ]
                [ text <| "If you find The PDA useful in your work, please cite as:" ]
            ]
        ]


dataProcessingArea : Int -> Element msg
dataProcessingArea columnWidth =
    column
        [ width fill
        , spacing 10
        ]
        [ paragraph
            Style.h2Font
            [ text "Data processing"
            ]
        , column
            (Style.monospacedFont
                ++ [ Font.justify
                   , width
                        (fill
                            |> maximum columnWidth
                        )
                   ]
            )
            [ paragraph
                [ width
                    (fill
                        |> maximum columnWidth
                    )
                ]
                [ text <| "If you find The PDA useful in your work, please cite as:" ]
            ]
        ]
