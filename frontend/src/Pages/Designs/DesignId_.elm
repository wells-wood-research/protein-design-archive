module Pages.Designs.DesignId_ exposing (Model, Msg, designDetailsBody, designDetailsHeader, designDetailsView, details, page)

import AppError exposing (AppError(..))
import Browser.Dom
import Browser.Events
import Components.Title
import Dict
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import FeatherIcons
import Get exposing (..)
import Html
import Html.Attributes as HAtt
import Http
import Json.Decode
import Page exposing (Page)
import Plots exposing (RenderPlotState(..))
import ProteinDesign exposing (DownloadFileType, ProteinDesign, csvStringFromProteinDesignDownload, designDetailsFromProteinDesign, downloadDesignDecoder, jsonStringFromProteinDesignDownload)
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Set
import Shared
import String
import Style
import Task
import Time
import Urls
import View exposing (View)


page : Shared.Model -> Route { designId : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \_ -> init shared.mScreenWidthF shared.mScreenHeightF route.params.designId
        , update = update
        , subscriptions = subscriptions
        , view = view shared >> Components.Title.view shared.mScreenWidthF
        }



-- INIT


type DetailsTab
    = Publication
    | GeneralTab
    | Similarity
    | Solubility
    | Energy
    | Description


type EnergyTab
    = BUDE
    | DFIRE2
    | EvoEF2
    | Rosetta


type alias Model =
    { designId : String
    , design : RemoteData Http.Error ProteinDesign
    , errors : List AppError
    , mScreenWidthF : Maybe Float
    , mScreenHeightF : Maybe Float
    , replotTime : Int
    , renderPlotState : RenderPlotState
    , dataDownload : RemoteData Http.Error String
    , activeDetailsTab : DetailsTab
    , activeEnergyTab : EnergyTab
    }


init : Maybe Float -> Maybe Float -> String -> ( Model, Effect Msg )
init mSharedScreenWidthF mSharedScreenHeightF designId =
    ( { designId = designId
      , design = Loading
      , errors = []
      , replotTime = 3
      , renderPlotState = WillRender
      , mScreenWidthF = mSharedScreenWidthF
      , mScreenHeightF = mSharedScreenHeightF
      , dataDownload = NotAsked
      , activeDetailsTab = Publication
      , activeEnergyTab = Rosetta
      }
    , Effect.batch
        [ Effect.sendCmd (Task.attempt ViewportResult Browser.Dom.getViewport)
        , Effect.resetViewport ViewportReset
        , Effect.sendCmd (getData <| Urls.designDetailsFromId designId)
        ]
    )


getData : String -> Cmd Msg
getData url =
    Http.get
        { url = url
        , expect =
            Http.expectJson DesignsDataReceived ProteinDesign.rawDesignDecoder
        }



-- UPDATE


type Msg
    = SendDesignsHttpRequest
    | DesignsDataReceived (Result Http.Error ProteinDesign)
    | RequestSelectedDesignData DownloadFileType
    | ForExportResponse DownloadFileType (Result Http.Error String)
    | AddToDownloadList
    | RemoveFromDownloadList
    | RenderWhenReady Time.Posix
    | WindowResizes Int Int
    | ViewportResult (Result Browser.Dom.Error Browser.Dom.Viewport)
    | ViewportReset
    | SelectDetailsTab DetailsTab
    | SelectEnergyTab EnergyTab


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case model.design of
        RemoteData.NotAsked ->
            case msg of
                SendDesignsHttpRequest ->
                    ( { model | design = Loading }
                    , Effect.sendCmd (getData <| Urls.designDetailsFromId model.designId)
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

                ViewportResult result ->
                    case result of
                        Ok viewport ->
                            ( { model | mScreenWidthF = Just viewport.viewport.width, mScreenHeightF = Just viewport.viewport.height }, Effect.none )

                        Err _ ->
                            ( model, Effect.none )

                WindowResizes width height ->
                    let
                        widthF =
                            toFloat width

                        heightF =
                            toFloat height
                    in
                    ( { model | mScreenWidthF = Just widthF, mScreenHeightF = Just heightF }, Effect.resetViewport ViewportReset )

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
                RequestSelectedDesignData fileType ->
                    ( { model | dataDownload = Loading }
                    , Http.get
                        { url = Urls.downloadSelectedDesigns [ model.designId ]
                        , expect =
                            Http.expectString (ForExportResponse fileType)
                        }
                        |> Effect.sendCmd
                    )

                ForExportResponse _ (Err err) ->
                    ( { model | dataDownload = Failure err }
                    , Effect.none
                    )

                ForExportResponse fileType (Ok designData) ->
                    let
                        encodedFileContent =
                            case fileType of
                                ProteinDesign.Json ->
                                    case Json.Decode.decodeString (Json.Decode.list downloadDesignDecoder) designData of
                                        Ok designs ->
                                            jsonStringFromProteinDesignDownload designs

                                        Err _ ->
                                            "There was an error in generating JSON file. Please try downloading CSV instead and contact us with feedback if the issue persists."

                                ProteinDesign.Csv ->
                                    case Json.Decode.decodeString (Json.Decode.list downloadDesignDecoder) designData of
                                        Ok designs ->
                                            csvStringFromProteinDesignDownload designs

                                        Err _ ->
                                            "There was an error in generating CSV file. Please try downloading JSON instead and contact us with feedback if the issue persists."
                    in
                    ( { model | dataDownload = NotAsked }
                    , Effect.downloadFile model.designId encodedFileContent fileType
                    )

                AddToDownloadList ->
                    ( model, Effect.addDesignsToDownload [ model.designId ] )

                RemoveFromDownloadList ->
                    ( model, Effect.removeDesignsFromDownload [ model.designId ] )

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

                SelectDetailsTab tab ->
                    ( { model | activeDetailsTab = tab }, Effect.none )

                SelectEnergyTab tab ->
                    ( { model | activeEnergyTab = tab }, Effect.none )

                _ ->
                    ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\width height -> WindowResizes width height)



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "PDA | " ++ model.designId
    , attributes =
        [ centerX
        , width
            (fill
                |> minimum (getScreenWidthInt model.mScreenWidthF)
            )
        ]
    , element = details shared model
    }


details : Shared.Model -> Model -> Element Msg
details shared model =
    let
        mDesign =
            model.design

        screenWidth =
            getScreenWidthInt model.mScreenWidthF

        screenHeight =
            getScreenWidthInt model.mScreenHeightF - 130
    in
    column
        [ width (fill |> maximum screenWidth) ]
        [ case mDesign of
            NotAsked ->
                column
                    (Style.monospacedFont
                        ++ [ width (fill |> maximum screenWidth)
                           , height <| px screenHeight
                           , centerX
                           , spaceEvenly
                           ]
                    )
                    [ el [ centerX, centerY ]
                        (html <|
                            FeatherIcons.toHtml [] <|
                                FeatherIcons.withSize 106 <|
                                    FeatherIcons.refreshCcw
                        )
                    , paragraph [ Font.center, Font.size 24, padding 50 ] [ text "Error querying the database. Try reloading the page." ]
                    ]

            Loading ->
                column
                    (Style.monospacedFont
                        ++ [ width (fill |> maximum screenWidth)
                           , height <| px screenHeight
                           , centerX
                           , spaceEvenly
                           ]
                    )
                    [ el [ centerX, centerY ]
                        (html <|
                            FeatherIcons.toHtml [] <|
                                FeatherIcons.withSize 106 <|
                                    FeatherIcons.loader
                        )
                    , paragraph [ Font.center, Font.size 16, padding 50, centerY ] [ text "Loading the design..." ]
                    ]

            Failure e ->
                column
                    (Style.monospacedFont
                        ++ [ width (fill |> maximum screenWidth)
                           , height <| px screenHeight
                           , centerX
                           , spaceEvenly
                           ]
                    )
                    [ el [ centerX, centerY ]
                        (html <|
                            FeatherIcons.toHtml [] <|
                                FeatherIcons.withSize 106 <|
                                    FeatherIcons.alertOctagon
                        )
                    , paragraph [ Font.center, Font.size 24, padding 50 ]
                        [ case e of
                            Http.BadUrl _ ->
                                paragraph [ Font.center, Font.size 24, padding 50, centerY ] [ text "Error loading design: invalid URL." ]

                            Http.Timeout ->
                                paragraph [ Font.center, Font.size 24, padding 50, centerY ] [ text "Error loading design: it took too long to get a response." ]

                            Http.NetworkError ->
                                paragraph [ Font.center, Font.size 24, padding 50, centerY ] [ text "Error loading design: please connect to the Internet." ]

                            Http.BadStatus i ->
                                paragraph [ Font.center, Font.size 24, padding 50, centerY ] [ text ("Error loading design: status code " ++ String.fromInt i) ]

                            Http.BadBody s ->
                                paragraph [ Font.center, Font.size 24, padding 50, centerY ] [ text ("Error decoding JSON: " ++ s) ]
                        ]
                    ]

            Success design ->
                designDetailsView shared model design screenWidth screenHeight
        ]


designDetailsView : Shared.Model -> Model -> ProteinDesign -> Int -> Int -> Element Msg
designDetailsView shared model proteinDesign screenWidth screenHeight =
    column
        ([ centerX
         , width (fill |> minimum screenWidth)
         , height fill
         ]
            ++ Style.bodyFont
        )
        [ designDetailsHeader proteinDesign.pdb "/designs/" proteinDesign screenWidth
        , designDetailsBody shared model proteinDesign screenWidth screenHeight
        ]


downloadButton : Length -> List (Attribute msg) -> Maybe msg -> Element msg -> Element msg
downloadButton widthButton buttonAttributes onPressCmd textLabel =
    Input.button
        (buttonAttributes
            ++ [ padding 10
               , width widthButton
               , centerX
               , Font.center
               , Element.mouseOver
                    [ Background.color <| rgb255 220 220 220
                    ]
               ]
        )
        { onPress = onPressCmd
        , label = textLabel
        }


downloadArea : Shared.Model -> String -> String -> Element Msg
downloadArea shared designId layoutMode =
    let
        elementType =
            case layoutMode of
                "wide-row" ->
                    row

                "mid-row" ->
                    column

                _ ->
                    row
    in
    elementType
        [ width fill
        , Font.bold
        , Border.widthEach { bottom = 1, top = 1, left = 0, right = 0 }
        , Border.color <| rgb255 220 220 220
        , centerX
        ]
        [ downloadButton fill [] (Just <| RequestSelectedDesignData ProteinDesign.Csv) (text "Download CSV")
        , downloadButton fill [] (Just <| RequestSelectedDesignData ProteinDesign.Json) (text "Download JSON")
        , if Set.member designId shared.designsToDownload then
            downloadButton fill [] (Just RemoveFromDownloadList) (text "Remove from download")

          else
            downloadButton fill [] (Just AddToDownloadList) (text "Add to download list")
        ]


designDetailsHeader : String -> String -> ProteinDesign -> Int -> Element msg
designDetailsHeader pdbCode path design screenWidth =
    let
        leftNav =
            link
                []
                { url = path ++ design.previous_design
                , label =
                    el []
                        (html <|
                            FeatherIcons.toHtml [ HAtt.align "center" ] <|
                                FeatherIcons.withSize 36 <|
                                    FeatherIcons.withStrokeWidth 1.2 <|
                                        FeatherIcons.arrowLeftCircle
                        )
                }

        rightNavAndBadge =
            link []
                { url = path ++ design.next_design
                , label =
                    el []
                        (html <|
                            FeatherIcons.toHtml [] <|
                                FeatherIcons.withSize 36 <|
                                    FeatherIcons.withStrokeWidth 1.2 <|
                                        FeatherIcons.arrowRightCircle
                        )
                }
    in
    row
        [ width (fill |> maximum screenWidth)
        , paddingXY 20 10
        , centerY
        ]
        [ leftNav
        , paragraph
            (Style.monospacedFont ++ [ Font.size 28, Font.center ])
            [ text <| pdbCode ]
        , rightNavAndBadge
        ]


detailsTabBar : DetailsTab -> Int -> Element Msg
detailsTabBar activeTab tableWidth =
    let
        numTabs =
            6

        widthButton =
            px ((tableWidth // numTabs) - 10)

        buttonAttributesBase =
            [ Border.width 0
            , Element.mouseOver [ Background.color <| rgb255 220 220 220 ]
            , padding 10
            ]

        buttonFor tab label =
            let
                isActive =
                    tab == activeTab

                attrs =
                    (Style.h3Font ++ buttonAttributesBase)
                        ++ [ if isActive then
                                Background.color <| rgb255 220 220 220

                             else
                                Background.color <| rgb255 250 250 250
                           ]
            in
            downloadButton widthButton attrs (Just (SelectDetailsTab tab)) (text label)
    in
    row
        [ width fill
        , Border.widthEach { bottom = 2, top = 2, left = 0, right = 0 }
        , Border.color <| rgb255 220 220 220
        , alignLeft
        ]
        [ buttonFor Publication "Publication"
        , buttonFor GeneralTab "General"
        , buttonFor Solubility "Solubility"
        , buttonFor Similarity "Similarity"
        , buttonFor Energy "Energy"
        , buttonFor Description "Description"
        ]


energyTabBar : EnergyTab -> Int -> Element Msg
energyTabBar activeTab tableWidth =
    let
        widthButton =
            px ((tableWidth // 4) - 10)

        buttonAttributesBase =
            [ Border.widthEach { bottom = 1, top = 1, left = 0, right = 0 }
            , Border.width 0
            , Element.mouseOver [ Background.color <| rgb255 220 220 220 ]
            , Border.color <| rgb255 220 220 220
            , padding 10
            ]

        buttonFor tab label =
            let
                isActive =
                    tab == activeTab

                attrs =
                    (Style.h3Font ++ buttonAttributesBase)
                        ++ [ if isActive then
                                Background.color <| rgb255 220 220 220

                             else
                                Background.color <| rgb255 250 250 250
                           ]
            in
            downloadButton widthButton attrs (Just (SelectEnergyTab tab)) (text label)
    in
    row
        [ width fill
        , Border.widthEach { bottom = 2, top = 2, left = 0, right = 0 }
        , Border.color <| rgb255 220 220 220
        , alignLeft
        ]
        [ buttonFor Rosetta "Rosetta"
        , buttonFor EvoEF2 "EvoEF2"
        , buttonFor DFIRE2 "DFIRE2"
        , buttonFor BUDE "BUDE"
        ]


detailsTable : List (ProteinDesign.DesignDetails Msg) -> Int -> Element Msg
detailsTable detailsList tableWidth =
    table
        [ width (px tableWidth) ]
        { data = detailsList
        , columns =
            [ { header = paragraph [ Font.bold, paddingXY 5 10, Border.widthEach { bottom = 2, top = 2, left = 0, right = 0 }, Border.color <| rgb255 220 220 220 ] [ text "Attribute" ]
              , width = px (tableWidth // 4)
              , view = \category -> paragraph Style.monospacedFont [ column [ width (px (tableWidth // 5 - 20)), height fill, scrollbarX, paddingXY 5 10 ] [ text category.header ] ]
              }
            , { header = paragraph [ Font.bold, paddingXY 10 10, Border.widthEach { bottom = 2, top = 2, left = 0, right = 0 }, Border.color <| rgb255 220 220 220 ] [ text "Value" ]
              , width = px (tableWidth * 3 // 4)
              , view = \detail -> paragraph Style.monospacedFont [ column [ width (px (tableWidth * 4 // 5)), height fill, scrollbarX, paddingXY 10 10 ] [ detail.property ] ]
              }
            ]
        }



-- Amino acid & secondary structure visualization (HTML fragments)


renderAminoAcidHtml : List ( String, Float ) -> Int -> Html.Html Msg
renderAminoAcidHtml kvs tableWidth =
    let
        -- Amino acid metadata: full name, three-letter code, property group
        aaMeta =
            Dict.fromList
                [ ( "A", ( "Alanine", "Ala", "hydrophobic" ) )
                , ( "R", ( "Arginine", "Arg", "basic" ) )
                , ( "N", ( "Asparagine", "Asn", "polar" ) )
                , ( "D", ( "Aspartic acid", "Asp", "acidic" ) )
                , ( "C", ( "Cysteine", "Cys", "special" ) )
                , ( "E", ( "Glutamic acid", "Glu", "acidic" ) )
                , ( "Q", ( "Glutamine", "Gln", "polar" ) )
                , ( "G", ( "Glycine", "Gly", "special" ) )
                , ( "H", ( "Histidine", "His", "basic" ) )
                , ( "I", ( "Isoleucine", "Ile", "hydrophobic" ) )
                , ( "L", ( "Leucine", "Leu", "hydrophobic" ) )
                , ( "K", ( "Lysine", "Lys", "basic" ) )
                , ( "M", ( "Methionine", "Met", "hydrophobic" ) )
                , ( "F", ( "Phenylalanine", "Phe", "hydrophobic" ) )
                , ( "P", ( "Proline", "Pro", "special" ) )
                , ( "S", ( "Serine", "Ser", "polar" ) )
                , ( "T", ( "Threonine", "Thr", "polar" ) )
                , ( "W", ( "Tryptophan", "Trp", "hydrophobic" ) )
                , ( "Y", ( "Tyrosine", "Tyr", "polar" ) )
                , ( "V", ( "Valine", "Val", "hydrophobic" ) )
                ]

        propColor =
            Dict.fromList
                [ ( "hydrophobic", "#1f77b4" )
                , ( "polar", "#2ca02c" )
                , ( "acidic", "#d62728" )
                , ( "basic", "#9467bd" )
                , ( "special", "#8c564b" )
                ]

        -- normalize: if sums to ~1, multiply by 100
        total =
            kvs |> List.map Tuple.second |> List.sum

        multiplier =
            if total <= 1.01 then
                100.0

            else
                1.0

        enriched =
            kvs
                |> List.map
                    (\tuple ->
                        let
                            aa =
                                String.toUpper (Tuple.first tuple)

                            v =
                                Tuple.second tuple * multiplier

                            ( name, three, prop ) =
                                Dict.get aa aaMeta |> Maybe.withDefault ( "Unknown", "Xxx", "special" )

                            color =
                                Dict.get prop propColor |> Maybe.withDefault "#777777"

                            rec =
                                { name = name, three = three, prop = prop, color = color, v = v }
                        in
                        ( aa, rec )
                    )
                |> List.sortBy (\( aa, rec ) -> -rec.v)

        -- build rows as Html
        rowFor ( aa, rec ) =
            let
                name =
                    rec.name

                three =
                    rec.three

                prop =
                    rec.prop

                color =
                    rec.color

                v =
                    rec.v

                barMaxPx =
                    toFloat (tableWidth - 160)

                barPx =
                    String.fromInt (Basics.round (barMaxPx * (v / 100.0))) ++ "px"

                pctText =
                    String.fromFloat (toFloat (Basics.round (v * 100.0)) / 100.0) ++ "%"

                titleAttr =
                    name ++ " (" ++ three ++ ") " ++ aa ++ ": " ++ pctText

                barStyle =
                    "background-color: " ++ color ++ "; height:20px; width:" ++ barPx ++ "; border-radius:3px;"
            in
            Html.node "div"
                [ HAtt.style "display" "flex", HAtt.style "align-items" "center", HAtt.style "margin" "6px 0", HAtt.title titleAttr ]
                [ Html.node "div" [ HAtt.style "width" "120px", HAtt.style "font-family" "monospace" ] [ Html.text (aa ++ " (" ++ three ++ ")") ]
                , Html.node "div"
                    [ HAtt.style "background-color" "#eee", HAtt.style "width" (String.fromInt tableWidth ++ "px"), HAtt.style "padding" "4px", HAtt.style "border-radius" "3px" ]
                    [ Html.node "div"
                        [ HAtt.style "position" "relative" ]
                        [ Html.node "div" [ HAtt.attribute "style" barStyle ] []
                        , Html.node "div" [ HAtt.style "position" "absolute", HAtt.style "left" "8px", HAtt.style "top" "0px", HAtt.style "height" "20px", HAtt.style "line-height" "20px", HAtt.style "color" "white", HAtt.style "font-weight" "bold" ] [ Html.text pctText ]
                        ]
                    ]
                ]
    in
    Html.node "div" [] (List.map rowFor enriched)


renderSecondaryStructureHtml : List ( String, Float ) -> Float -> Int -> Bool -> Html.Html Msg
renderSecondaryStructureHtml kvs barHeight barWidth isVertical =
    let
        -- 1. Centralized Metadata
        infoFor name =
            case name of
                "alpha_helix" ->
                    ( "#1f77b4", "Helix" )

                "beta_strand" ->
                    ( "#ff7f0e", "Strand" )

                "beta_bridge" ->
                    ( "#2ca02c", "Bridge" )

                "3_10_helix" ->
                    ( "#d62728", "3_10" )

                "pi_helix" ->
                    ( "#9467bd", "pi" )

                "turn" ->
                    ( "#8c564b", "Turn" )

                "bend" ->
                    ( "#e377c2", "Bend" )

                "loop" ->
                    ( "#7f7f7f", "Loop" )

                _ ->
                    ( "#7f7f7f", name )

        total =
            kvs |> List.map Tuple.second |> List.sum

        multiplier =
            if total <= 1.01 then
                100.0

            else
                1.0

        renderSegment ( name, rawValue ) =
            let
                v =
                    rawValue * multiplier

                ( color, label ) =
                    infoFor name

                -- Calculate dimensions based on orientation
                dimStyle =
                    if isVertical then
                        [ ( "display", "block" )
                        , ( "width", String.fromInt barWidth ++ "px" )
                        , ( "height", String.fromInt (Basics.round (barHeight * (v / 100.0))) ++ "px" )
                        ]

                    else
                        [ ( "display", "inline-block" )
                        , ( "height", "100px" )
                        , ( "width", String.fromInt (Basics.round (toFloat barWidth * (v / 100.0))) ++ "px" )
                        ]

                styleAttr =
                    dimStyle
                        ++ [ ( "background-color", color )
                           , ( "position", "relative" )
                           , ( "overflow", "hidden" )
                           ]
                        |> List.map (\( k, val ) -> k ++ ":" ++ val)
                        |> String.join ";"

                innerStyle =
                    "display:flex; align-items:center; justify-content:center; height:100%; color:white; font-weight:bold; font-size:18px; text-shadow:0 0 3px rgba(0,0,0,0.6); overflow:hidden;"

                titleAttr =
                    name ++ ": " ++ String.fromFloat (toFloat (Basics.round (v * 100.0)) / 100.0) ++ "%"
            in
            Html.node "div"
                [ HAtt.attribute "style" styleAttr, HAtt.title titleAttr ]
                [ Html.node "div" [ HAtt.style "display" "flex", HAtt.attribute "style" innerStyle ] [ Html.text label ] ]

        segments =
            kvs
                |> List.filter (\( _, v ) -> v > 0)
                |> List.sortBy (\( _, v ) -> -v)
                |> List.map renderSegment
    in
    Html.node "div"
        [ HAtt.style "border-radius" "4px", HAtt.style "overflow" "hidden" ]
        segments


detailsTabContent : Model -> ProteinDesign -> Int -> Int -> Element Msg
detailsTabContent model proteinDesign tableWidth contentHeight =
    let
        designDetailsList =
            ProteinDesign.designDetailsFromProteinDesign proteinDesign

        select headers =
            List.filter (\d -> List.member d.header headers) designDetailsList

        publicationHeaders =
            [ "PDB code", "Release date", "Subtitle", "Authors", "Publication", "Reference link", "Tags", "Experimental charact. method", "Synthesis comment" ]

        structureHeaders =
            [ "CATH", "Symmetry group" ]

        similarityHeaders =
            [ "Sequence related designs (bits)", "Sequence related proteins (bits)", "Structure related designs (LDDT)", "Structure related proteins (LDDT)" ]

        -- helper to convert simple kv pairs into DesignDetails entries
        maybeFloatToString mv =
            case mv of
                Just f ->
                    String.fromFloat f

                Nothing ->
                    "-"

        kvToDetail ( k, mv ) =
            { header = k
            , property = paragraph Style.monospacedFont [ text <| maybeFloatToString mv ]
            }
    in
    case model.activeDetailsTab of
        Publication ->
            el [ width (px tableWidth), height <| px contentHeight, scrollbarY ] (detailsTable (select publicationHeaders) tableWidth)

        GeneralTab ->
            let
                numResiduesF =
                    proteinDesign.physicochem.num_residues

                massF =
                    case proteinDesign.physicochem.mass of
                        Just f ->
                            Just f

                        Nothing ->
                            Just proteinDesign.formula_weight

                chargeF =
                    proteinDesign.physicochem.charge

                pIF =
                    proteinDesign.physicochem.isoelectric_point

                packingF =
                    proteinDesign.physicochem.packing_density

                physRows =
                    [ ( "Number of residues", numResiduesF )
                    , ( "Mass (Da)", massF )
                    , ( "Charge", chargeF )
                    , ( "Isoelectric_point", pIF )
                    , ( "Packing density", packingF )
                    ]

                physDetails =
                    List.map kvToDetail physRows

                combined =
                    physDetails ++ select structureHeaders
            in
            el [ width (px tableWidth), height <| px contentHeight, scrollbarY ] (detailsTable combined tableWidth)

        Solubility ->
            let
                hyd =
                    proteinDesign.physicochem.hydrophobic_fitness

                sol_total =
                    Maybe.map .total proteinDesign.physicochem.solubility

                sol_avg =
                    Maybe.map .avg proteinDesign.physicochem.solubility

                sol_min =
                    Maybe.map .min proteinDesign.physicochem.solubility

                sol_max =
                    Maybe.map .max proteinDesign.physicochem.solubility

                solDetails =
                    [ ( "Hydrophobic fitness", hyd ), ( "Total aggregation propensity", sol_total ), ( "Average aggregation propensity", sol_avg ), ( "Minimum aggregation propensity", sol_min ), ( "Maximum aggregation propensity", sol_max ) ]
                        |> List.map kvToDetail
            in
            el [ width (px tableWidth), height <| px contentHeight, scrollbarY ] (detailsTable solDetails tableWidth)

        Similarity ->
            el [ width (px tableWidth), height <| px contentHeight, scrollbarY ] (detailsTable (select similarityHeaders) tableWidth)

        Energy ->
            el [ width (px tableWidth), height <| px contentHeight, scrollbarY ]
                (column [ width fill ]
                    [ el [ width fill ] (energyTabBar model.activeEnergyTab tableWidth)
                    , el [ width fill, paddingXY 0 4 ] (energyTabContent model proteinDesign tableWidth)
                    ]
                )

        Description ->
            el [ width (px tableWidth), height <| px contentHeight, scrollbarY ]
                (if proteinDesign.abstract == "No description found." then
                    column [ width fill ] [ text "" ]

                 else
                    column [ width fill, spacing 10, paddingXY 0 4 ]
                        [ paragraph (paddingXY 0 20 :: Style.h2Font) [ text "Description" ]
                        , paragraph
                            (Style.monospacedFont
                                ++ [ Font.justify
                                   , width (fill |> maximum tableWidth)
                                   ]
                            )
                            [ proteinDesign.abstract
                                |> text
                            ]
                        ]
                )


energyTabContent : Model -> ProteinDesign -> Int -> Element Msg
energyTabContent model proteinDesign tableWidth =
    let
        -- helper to convert simple kv pairs into DesignDetails entries
        maybeFloatToString mv =
            case mv of
                Just f ->
                    String.fromFloat f

                Nothing ->
                    "-"

        kvToDetail ( k, mv ) =
            { header = k
            , property = paragraph Style.monospacedFont [ text <| maybeFloatToString mv ]
            }

        -- build metric pairs from the decoded Energy record
        budePairs =
            case proteinDesign.physicochem.energy.bude of
                Nothing ->
                    [ ( "Total", Nothing ), ( "Steric", Nothing ), ( "Desolvation", Nothing ), ( "Charge", Nothing ) ]

                Just b ->
                    [ ( "Total", b.budeff_total ), ( "Steric", b.budeff_steric ), ( "Desolvation", b.budeff_desolvation ), ( "Charge", b.budeff_charge ) ]

        dfire2Pairs =
            case proteinDesign.physicochem.energy.dfire2 of
                Nothing ->
                    [ ( "Total", Nothing ) ]

                Just d ->
                    [ ( "Total", d.dfire2_total ) ]

        evoef2Pairs =
            case proteinDesign.physicochem.energy.evoef2 of
                Nothing ->
                    [ ( "Total", Nothing ), ( "Reference", Nothing ), ( "Intra Residue", Nothing ), ( "Inter Residue (Same Chain)", Nothing ), ( "Inter Residue (Different Chains)", Nothing ) ]

                Just e ->
                    [ ( "Total", e.evoef2_total ), ( "Reference", e.evoef2_ref_total ), ( "Intra Residue", e.evoef2_intraR_total ), ( "Inter Residue (Same Chain)", e.evoef2_interS_total ), ( "Inter Residue (Different Chains)", e.evoef2_interD_total ) ]

        rosettaPairs =
            case proteinDesign.physicochem.energy.rosetta of
                Nothing ->
                    [ ( "Total", Nothing ), ( "vdW (Attractive)", Nothing ), ( "vdW (Repulsive)", Nothing ), ( "vdW (Repulsive, Intra Residue)", Nothing ), ( "Electrostatics", Nothing ), ( "Solvation (Isotropic)", Nothing ), ( "Solvation (Anisotropic, Polar Atoms)", Nothing ), ( "Solvation (Isotropic, Intra Residue)", Nothing ), ( "Hydrogen Bonding (Long Range, Backbone)", Nothing ), ( "Hydrogen Bonding (Short Range, Backbone)", Nothing ), ( "Hydrogen Bonding (Backbone-Sidechain)", Nothing ), ( "Hydrogen Bonding (Sidechain-Sidechain)", Nothing ), ( "Disulfide Bridges", Nothing ), ( "Backbone Torsion Preference", Nothing ), ( "Amino Acid Propensity", Nothing ), ( "Dunbrack Rotamer", Nothing ), ( "Omega Penalty", Nothing ), ( "Open Proline Penalty", Nothing ), ( "Tyrosine χ3 Dihedral Angle Penalty", Nothing ) ]

                Just r ->
                    [ ( "Total", r.rosetta_total ), ( "vdW (Attractive)", r.rosetta_vdw_atr ), ( "vdW (Repulsive)", r.rosetta_vdw_rep ), ( "vdW (Repulsive, Intra Residue)", r.rosetta_vdw_intra_rep ), ( "Electrostatics", r.rosetta_electrostatics ), ( "Solvation (Isotropic)", r.rosetta_solvation_isotropic ), ( "Solvation (Anisotropic, Polar Atoms)", r.rosetta_solvation_anisotropic_polar_atoms ), ( "Solvation (Isotropic, Intra Residue)", r.rosetta_solvation_isotropic_iR ), ( "Hydrogen Bonding (Long Range, Backbone)", r.rosetta_hbond_lr_bb ), ( "Hydrogen Bonding (Short Range, Backbone)", r.rosetta_hbond_sr_bb ), ( "Hydrogen Bonding (Backbone-Sidechain)", r.rosetta_hbond_bb_sc ), ( "Hydrogen Bonding (Sidechain-Sidechain)", r.rosetta_hbond_sc ), ( "Disulfide Bridges", r.rosetta_disulfides ), ( "Backbone Torsion Preference", r.rosetta_backbone_torsion_preference ), ( "Amino Acid Propensity", r.rosetta_aa_propensity ), ( "Dunbrack Rotamer", r.rosetta_dunbrack_rotamer ), ( "Omega Penalty", r.rosetta_omega ), ( "Open Proline Penalty", r.rosetta_pro_close ), ( "Tyrosine χ3 Dihedral Angle Penalty", r.rosetta_yhh_planarity ) ]

        buildDetailsFromPairs pairs =
            List.map kvToDetail pairs
    in
    case model.activeEnergyTab of
        Rosetta ->
            el [ width (px tableWidth) ] (detailsTable (buildDetailsFromPairs rosettaPairs) (tableWidth - 40))

        EvoEF2 ->
            el [ width (px tableWidth) ] (detailsTable (buildDetailsFromPairs evoef2Pairs) (tableWidth - 40))

        BUDE ->
            el [ width (px tableWidth) ] (detailsTable (buildDetailsFromPairs budePairs) (tableWidth - 40))

        DFIRE2 ->
            el [ width (px tableWidth) ] (detailsTable (buildDetailsFromPairs dfire2Pairs) (tableWidth - 40))


designDetailsBodyStructure : ProteinDesign -> Int -> Int -> Element Msg
designDetailsBodyStructure proteinDesign screenWidth screenHeight =
    let
        picHeight =
            round <| toFloat screenHeight * 0.7
    in
    column
        [ width fill
        , spacing 20
        ]
        [ column
            [ width (fill |> maximum screenWidth)
            , spacing 20
            ]
            [ column
                Style.h2Font
                [ text "Structure"
                ]
            ]
        , column
            [ spacing 20
            , centerX
            ]
            [ Keyed.el
                [ width (fill |> maximum screenWidth)
                , height <| px picHeight
                ]
                ( proteinDesign.pdb
                , Html.node "ngl-viewer"
                    [ HAtt.id "viewer"
                    , HAtt.style "width" (String.fromInt screenWidth ++ "px")
                    , HAtt.style "height" (String.fromInt picHeight ++ "px")
                    , HAtt.style "align" "center"
                    , HAtt.alt "3D structure"
                    , HAtt.attribute "pdb-string" proteinDesign.pdb
                    ]
                    []
                    |> html
                )
            ]
        ]


designDetailsBodySequence : ProteinDesign -> Int -> Element Msg
designDetailsBodySequence proteinDesign screenWidth =
    let
        -- We calculate relative percentages to ensure columns share the space
        -- but we use px limits to ensure they are readable.
        totalPadding =
            60

        tableWidth =
            screenWidth - totalPadding

        narrowWidth =
            max (tableWidth // 10) 80

        wideWidth =
            tableWidth - (4 * narrowWidth)

        columnStyles =
            Style.monospacedFont
                ++ [ height fill
                   , scrollbarX
                   , paddingXY 15 10
                   , width (px narrowWidth)
                   ]

        headerStyle =
            [ Font.bold
            , paddingXY 15 10
            , Border.widthEach { bottom = 2, top = 2, left = 0, right = 0 }
            , Border.color <| rgb255 220 220 220
            ]
    in
    column
        [ spacing 20
        , width fill
        , centerX

        -- This ensures the entire Sequence block doesn't push the page wide
        , htmlAttribute (HAtt.style "max-width" "calc(100vw - 60px)")
        ]
        [ paragraph
            Style.h2Font
            [ text "Sequence" ]
        , table
            [ width fill
            , spacing 0
            ]
            { data = proteinDesign.chains
            , columns =
                [ { header = el headerStyle (text "Chain ID")
                  , width = px narrowWidth
                  , view = \chain -> el columnStyles (text chain.chain_id)
                  }
                , { header = el headerStyle (text "Type")
                  , width = px narrowWidth
                  , view = \chain -> el columnStyles (text chain.chain_type)
                  }
                , { header = el headerStyle (text "Source")
                  , width = px narrowWidth
                  , view = \chain -> el columnStyles (text <| String.toLower chain.chain_source)
                  }
                , { header = el headerStyle (text "Sequence")
                  , width = px wideWidth
                  , view = \chain -> el columnStyles (text chain.chain_seq_unnat)
                  }
                , { header = el headerStyle (text "Length")
                  , width = px narrowWidth
                  , view = \chain -> el columnStyles (text <| String.fromInt chain.chain_length)
                  }
                ]
            }
        ]


designDetailsBodyParagraphs : Model -> ProteinDesign -> Int -> Element Msg
designDetailsBodyParagraphs model proteinDesign screenWidth =
    let
        isWide =
            screenWidth > 860

        -- If wide, we split width between AA list and the vertical SS bar
        tableWidth =
            if isWide then
                screenWidth - 150

            else
                screenWidth - 60

        aaData =
            proteinDesign.physicochem.aa_composition

        ssData =
            proteinDesign.physicochem.ss_composition

        -- Helper to render AA/SS sections with smart width and stacking decisions
        aaHeaderText =
            "Amino acid composition"

        ssHeaderText =
            "2° structure composition"

        charPx =
            9

        aaHeaderPx =
            (String.length aaHeaderText * charPx) + 128

        ssHeaderPx =
            (String.length ssHeaderText * charPx) + 128

        gapPx =
            40

        aaRows =
            List.length aaData

        aaHeightPx =
            max 180 (aaRows * 32 + 40)

        -- Decide whether headers can sit side-by-side without wrapping
        canSideBySide =
            isWide && (tableWidth >= (aaHeaderPx + ssHeaderPx + gapPx))

        -- If side-by-side, allocate secondary structure column just enough for its header,
        -- and let AA fill the remaining space. If not side-by-side, stack SS below AA.
        ( aaPxWidth, ssPxWidth, ssIsVertical ) =
            if canSideBySide then
                ( tableWidth - ssHeaderPx, ssHeaderPx, True )

            else
                ( tableWidth, tableWidth, False )

        aaSection =
            column [ width (px aaPxWidth), alignTop, height <| px aaHeightPx ]
                [ if List.length aaData == 0 then
                    text ""

                  else
                    paragraph (Style.h2Font ++ [ width (px aaPxWidth), htmlAttribute (HAtt.style "white-space" "nowrap") ]) [ text aaHeaderText ]
                , el [ width (px aaPxWidth), height fill, paddingXY 0 20 ] (html <| renderAminoAcidHtml aaData aaPxWidth)
                ]

        -- Helper to render SS section; header width limited to ssPxWidth so it won't wrap
        ssSection =
            column [ alignTop, spacing 4, width (px ssPxWidth) ]
                [ if List.length ssData == 0 then
                    text ""

                  else
                    paragraph (Style.h2Font ++ [ width (px ssPxWidth), htmlAttribute (HAtt.style "white-space" "nowrap") ]) [ text ssHeaderText ]

                -- Match stacked bar width to header width; pass ssIsVertical flag so renderer knows orientation
                , el [ centerX, width (px ssPxWidth), height fill, paddingXY 0 20 ] (html <| renderSecondaryStructureHtml ssData (toFloat aaHeightPx) ssPxWidth ssIsVertical)
                ]

        -- Dynamic layout container: row when side-by-side, column when stacked
        compositionLayout =
            if isWide then
                row [ width fill, spacing 40, alignTop, paddingXY 0 20 ] [ aaSection, ssSection ]

            else
                column [ width fill, spacing 80, paddingEach { top = 0, bottom = 20, left = 0, right = 0 } ] [ aaSection, ssSection ]
    in
    column
        [ width fill, spacing 20 ]
        [ compositionLayout
        , if List.all (\comment -> comment == "") proteinDesign.review_comment then
            text ""

          else
            paragraph
                (paddingXY 0 20 :: Style.h2Font)
                [ text "Curation comments"
                ]
        , column
            (Style.monospacedFont
                ++ [ Font.justify
                   , width (fill |> maximum screenWidth)
                   , spacing 10
                   , paddingEach { bottom = 20, left = 0, right = 0, top = 0 }
                   ]
            )
            (if List.all (\comment -> comment == "") proteinDesign.review_comment then
                [ text "" ]

             else
                List.map
                    (\comment ->
                        wrappedRow
                            [ width fill
                            ]
                            [ row
                                [ width fill ]
                                [ text <| comment ]
                            ]
                    )
                    proteinDesign.review_comment
            )
        ]


designDetailsBodyTop : Shared.Model -> Model -> ProteinDesign -> String -> Int -> Int -> Int -> Element Msg
designDetailsBodyTop shared model proteinDesign layoutMode leftWidth rightWidth contentHeight =
    (case layoutMode of
        "stacked" ->
            column

        _ ->
            row
    )
        [ width fill, spacing 10 ]
        [ column
            [ width <| px leftWidth
            , height <| px contentHeight
            , Background.color <| rgb255 250 250 250
            , Border.rounded 8
            , Border.width 1
            , Border.color <| rgb255 220 220 220
            , paddingXY 20 20
            , alignTop
            ]
            [ detailsTabBar model.activeDetailsTab leftWidth
            , el [ width fill, height <| px (contentHeight - 60), scrollbarY, paddingXY 0 4 ] (detailsTabContent model proteinDesign leftWidth (contentHeight - 60))
            ]
        , column
            [ alignTop
            , height <| px contentHeight
            , width <| px rightWidth
            ]
            [ el
                [ padding 2
                , Border.width 1
                , Border.color <| rgb255 220 220 220
                , Border.rounded 8
                , width fill
                , height fill
                , Background.color <| rgb255 255 255 255
                ]
                (image
                    [ centerX
                    , centerY
                    , width (fill |> maximum (min rightWidth 500))
                    , height (fill |> maximum (min (contentHeight - 60) 500))
                    ]
                    { src = proteinDesign.picture_path
                    , description = "Structure of " ++ proteinDesign.pdb
                    }
                )
            , el
                [ height shrink
                , width fill
                ]
                (downloadArea shared proteinDesign.pdb layoutMode)
            ]
        ]


designDetailsBody : Shared.Model -> Model -> ProteinDesign -> Int -> Int -> Element Msg
designDetailsBody shared model proteinDesign screenWidth screenHeight =
    let
        -- reserve a little padding space from the available height
        topAreaHeight =
            max 220 screenHeight

        -- table width should match the previous logic used elsewhere
        leftWidth =
            if screenWidth > 1200 then
                screenWidth * 2 // 3

            else if screenWidth < 900 then
                screenWidth - 60

            else
                (screenWidth * 2) // 3 - 60

        rightWidth =
            if screenWidth < 900 then
                screenWidth - 60

            else
                screenWidth - leftWidth - 60

        -- unified height for BOTH table and image column
        contentHeight =
            min 600 topAreaHeight

        layoutMode =
            if screenWidth > 1450 then
                "wide-row"

            else if screenWidth > 900 then
                "mid-row"

            else
                "stacked"
    in
    column
        (Style.bodyFont
            ++ [ width (fill |> maximum screenWidth)
               , paddingXY 30 0
               , spacing 30
               , centerX
               ]
        )
        [ designDetailsBodyTop shared model proteinDesign layoutMode leftWidth rightWidth contentHeight
        , designDetailsBodySequence proteinDesign screenWidth
        , designDetailsBodyParagraphs model proteinDesign screenWidth
        , designDetailsBodyStructure proteinDesign screenWidth screenHeight
        ]
