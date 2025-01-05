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
        , referencesArea columnWidth
        ]


citationArea : Int -> Element msg
citationArea columnWidth =
    column
        [ width <| px columnWidth
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
                    [ text "Marta Chronowska, Michael J. Stam, Derek N. Woolfson, Luigi F. Di Costanzo, Christopher W. Wood" ]
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
        [ width <| px columnWidth
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
        , websiteStructureArea columnWidth
        , relatedProteinsArea columnWidth
        , filteringArea columnWidth
        ]


websiteStructureArea : Int -> Element msg
websiteStructureArea columnWidth =
    column
        (Style.monospacedFont
            ++ [ Font.justify
               , width <| px columnWidth
               , spacing 5
               ]
        )
        [ paragraph
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
                , paragraph [ width fill ] [ text <| " is self-explanatory. It provides information about a design, such as publication and basic experimental facts, interactive 3D structure, chain and sequence details, and a description, that is the abstract of the primary publication associated with this design. One element might merit elaborating on, that is " ]
                , paragraph [ width fill, Font.bold ] [ text <| "related proteins" ]
                , paragraph [ width fill ] [ text <| ". This is described in detail below." ]
                ]
            ]
        ]


relatedProteinsArea : Int -> Element msg
relatedProteinsArea columnWidth =
    column
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
            , text <| " [1] (for sequence) and "
            , newTabLink
                [ Font.color <| rgb255 104 176 171
                , Font.underline
                ]
                { url = "https://www.nature.com/articles/s41587-023-01773-0"
                , label = text <| "Foldseek"
                }
            , text <| " [2] (for structure) analysis as being similar to the currently viewed design. In the context of this analysis, "
            , paragraph [ Font.italic ] [ text <| " designs" ]
            , text <| " refers to other structures also found in The PDA, and"
            , paragraph [ Font.italic ] [ text <| " proteins" ]
            , text <| " refers to natural proteins, that is ones not found in The PDA."
            , paragraph [ Font.italic ] [ text <| " bits" ]
            , text <| " refers to the bit score, and "
            , paragraph [ Font.italic ] [ text <| " LDDT" ]
            , text <| " refers to the Local Distance Difference Test, as calculated by MMseqs2 and Foldseek, respectively."
            ]
        , paragraph [ width fill ]
            [ paragraph [ width fill ] [ text <| " We have selected values of " ]
            , paragraph [ Font.bold ] [ text <| "50 bit score and 95% LDDT as thresholds above which we list related entries." ]
            , paragraph [ width fill ] [ text <| " For designs that do not have any structures that meet these criteria, we list a related partner with the highest similarity score." ]
            ]
        ]


filteringArea : Int -> Element msg
filteringArea columnWidth =
    column
        (Style.monospacedFont
            ++ [ Font.justify
               , width <| px columnWidth
               , spacing 5
               ]
        )
        [ paragraph
            Style.h3Font
            [ text "Filtering functionality"
            ]
        , column
            (Style.monospacedFont
                ++ [ Font.justify
                   , width <| px columnWidth
                   , spacing 5
                   ]
            )
            [ paragraph [ width fill ]
                [ paragraph [ width fill ] [ text <| "The PDA website allows filtering the dataset based on the following, currently implemented criteria: by case-insensitive string-matching a search phrase, by release date, and by maximum similarity to natural proteins. We have implemented boolean logic in the text-based search: phrases can be separated by && (AND), || (OR), and &&!! (AND NOT). To illustrate, intentions behind the example search phrase: " ]
                , paragraph [ width fill, Font.italic ] [ text <| " Woolfson && coiled-coil || coiled coil &&!! 4-helix" ]
                , paragraph [ width fill ] [ text <| " could be translated as: " ]
                , paragraph [ width fill, Font.italic ] [ text <| " Show me designs that were designed by Woolfson, are coiled(-)coil, and are not 4-helical." ]
                , paragraph [ width fill ] [ text <| " Release date can be used to filter as a closed range, or with open upper or lower bound. Filtering by similarity can be performed with or without entries that do not have any related partners calculated (either due to an error, such as being too short, or because of their novelty) - to exclude, one needs to manually tick the checkbox next to the threshold slider. Filters are combined when multiple are defined simultaneously." ]
                ]
            ]
        ]


dataCollectionArea : Int -> Element msg
dataCollectionArea columnWidth =
    column
        [ width <| px columnWidth
        , spacing 10
        ]
        [ paragraph
            Style.h2Font
            [ text "Data collection"
            ]
        , column
            (Style.monospacedFont
                ++ [ Font.justify
                   , width fill
                   ]
            )
            [ paragraph
                [ width fill
                ]
                [ text <| "Data was collected and is updated monthly by running a search query on RCSB PDB with the following criteria: polymer entity type IS protein, polymer entity type IS NOT DNA, polymer entity type IS NOT RNA, polymer entity type IS NOT NA-hybrid, AND source organism taxonomy name (full lineage) IS synthetic construct. These entries are then manually reviewed, and if any appears to not be relevant to the de novo protein design field, it is added to the "
                , newTabLink
                    [ Font.color <| rgb255 104 176 171
                    , Font.underline
                    ]
                    { url = "https://github.com/wells-wood-research/chronowska-stam-wood-2024-protein-design-archive/blob/main/entries_to_manually_exclude.csv"
                    , label = text <| "excluded"
                    }
                , text <|
                    " list for transparency and discarded from The PDA dataset. Although the following is not exhaustive, example reasons for entries to be discarded are: "
                , column
                    [ width fill, spacing 5, paddingXY 30 10 ]
                    [ text <| "* method of development - if protein was created without rational reasoning, such as by screening a large random library, it is discarded,"
                    , text <| "* length - very short peptides, such as less than 14 amino acids (threshold chosen based on MMseqs2 similarity screening limitations), tend to be discarded unless their method of development strongly suggests that they should be included,"
                    , text <| "* identity of synthetic construct - certain entities that are not de novo designed proteins tend to be labelled as synthetic constructs when deposited to the PDB database, such as the scFv16 antibody used to stabilise GPCR/G-protein complexes; as these are not the focus of their study as de novo designs, they are discarded."
                    ]
                ]
            ]
        ]


dataProcessingArea : Int -> Element msg
dataProcessingArea columnWidth =
    column
        [ width <| px columnWidth
        , spacing 10
        ]
        [ paragraph
            Style.h2Font
            [ text "Data processing"
            ]
        , column
            (Style.monospacedFont
                ++ [ Font.justify
                   , width fill
                   ]
            )
            [ paragraph
                [ width fill
                ]
                [ paragraph [ width fill ] [ text <| "Details of data processing are available in the " ]
                , newTabLink
                    [ Font.color <| rgb255 104 176 171
                    , Font.underline
                    ]
                    { url = "https://doi.org/10.1101/2024.09.05.611465"
                    , label = text <| "pre-print"
                    }
                , paragraph [ width fill ] [ text <| " methods section, and the code used to generate the dataset is available fully with instructions on " ]
                , newTabLink
                    [ Font.color <| rgb255 104 176 171
                    , Font.underline
                    ]
                    { url = "https://github.com/wells-wood-research/chronowska-stam-wood-2024-protein-design-archive/tree/main"
                    , label = text <| "GitHub"
                    }
                , paragraph [ width fill ] [ text <| ". Please note that the method of extracting designed chains for analysis has been significantly improved between submission of " ]
                , paragraph [ width fill, Font.italic ] [ text <| "The Protein Design Archive (PDA): insights from 40 years of protein design" ]
                , paragraph [ width fill ] [ text <| " paper and updates made since October 2024. Please see the code on GitHub for the currently implemented, improved method. For any questions and feedback, we ask that you contact us following the link at the bottom of the page." ]
                ]
            ]
        ]


referencesArea : Int -> Element msg
referencesArea columnWidth =
    column
        [ width <| px columnWidth
        , spacing 10
        ]
        [ paragraph
            Style.h2Font
            [ text "References"
            ]
        , paragraph
            (Style.monospacedFont
                ++ [ Font.justify
                   , Font.size 14
                   , width (fill |> maximum columnWidth)
                   ]
            )
            [ paragraph [ width fill ] [ text <| "[1]" ]
            , paragraph [ Font.italic, width fill ] [ text <| " MMseqs2 enables sensitive protein sequence searching for the analysis of massive data sets" ]
            , paragraph [ width fill ] [ text <| ". Steinegger, M., Söding, J." ]
            , paragraph [ width fill ] [ text <| ", Nat Biotechnol 35, 1026–1028 (2017) " ]
            , newTabLink
                [ Font.color <| rgb255 104 176 171
                , Font.underline
                ]
                { url = "https://doi.org/10.1038/nbt.3988"
                , label = text "https://doi.org/10.1038/nbt.3988"
                }
            ]
        , paragraph
            (Style.monospacedFont
                ++ [ Font.justify
                   , Font.size 14
                   , width (fill |> maximum columnWidth)
                   ]
            )
            [ paragraph [ width fill ] [ text <| "[2]" ]
            , paragraph [ Font.italic, width fill ] [ text <| " Fast and accurate protein structure search with Foldseek" ]
            , paragraph [ width fill ] [ text <| ". van Kempen, M., Kim, S.S., Tumescheit, C. et al." ]
            , paragraph [ width fill ] [ text <| ", Nat Biotechnol 42, 243–246 (2024) " ]
            , newTabLink
                [ Font.color <| rgb255 104 176 171
                , Font.underline
                ]
                { url = "https://doi.org/10.1038/s41587-023-01773-0"
                , label = text "https://doi.org/10.1038/s41587-023-01773-0"
                }
            ]
        ]
