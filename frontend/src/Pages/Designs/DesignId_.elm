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


type Tab
    = Publication
    | StructureTab
    | Similarity
    | Solubility


type alias Model =
    { designId : String
    , design : RemoteData Http.Error ProteinDesign
    , errors : List AppError
    , mScreenWidthF : Maybe Float
    , mScreenHeightF : Maybe Float
    , replotTime : Int
    , renderPlotState : RenderPlotState
    , dataDownload : RemoteData Http.Error String
    , activeTab : Tab
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
      , activeTab = Publication
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
    | SelectTab Tab


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

                SelectTab tab ->
                    ( { model | activeTab = tab }, Effect.none )

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
         , width (fill |> maximum screenWidth)
         , height fill
         , paddingXY 0 0
         ]
            ++ Style.bodyFont
        )
        [ designDetailsHeader "Design Details" "/designs/" proteinDesign screenWidth
        , downloadArea shared proteinDesign.pdb screenWidth
        , designDetailsBody model proteinDesign screenWidth screenHeight
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


downloadArea : Shared.Model -> String -> Int -> Element Msg
downloadArea shared designId screenWidth =
    let
        widthButton =
            if screenWidth < 600 then
                Element.fill |> maximum (screenWidth - 10)

            else
                Element.px 200

        buttonAttributes =
            if screenWidth < 600 then
                [ Border.widthEach { bottom = 1, top = 1, left = 0, right = 0 }
                , Border.color <| rgb255 220 220 220
                ]

            else
                [ centerX
                , Font.center
                ]

        elementType =
            if screenWidth < 600 then
                column

            else
                row
    in
    elementType
        [ width (fill |> maximum screenWidth)
        , Font.bold
        , Border.widthEach { bottom = 2, top = 2, left = 0, right = 0 }
        , Border.color <| rgb255 220 220 220
        , centerX
        ]
        [ downloadButton widthButton buttonAttributes (Just <| RequestSelectedDesignData ProteinDesign.Csv) (text "Download CSV")
        , downloadButton widthButton buttonAttributes (Just <| RequestSelectedDesignData ProteinDesign.Json) (text "Download JSON")
        , if Set.member designId shared.designsToDownload then
            downloadButton widthButton buttonAttributes (Just RemoveFromDownloadList) (text "Remove from download")

          else
            downloadButton widthButton buttonAttributes (Just AddToDownloadList) (text "Add to download list")
        ]


designDetailsHeader : String -> String -> ProteinDesign -> Int -> Element msg
designDetailsHeader title path { previous_design, next_design } screenWidth =
    row
        [ width (fill |> maximum screenWidth) ]
        [ row
            [ centerX
            , spacing 30
            , paddingXY 0 30
            ]
            [ link
                []
                { url = path ++ previous_design
                , label =
                    el []
                        (html <|
                            FeatherIcons.toHtml [ HAtt.align "center" ] <|
                                FeatherIcons.withSize 36 <|
                                    FeatherIcons.withStrokeWidth 1.2 <|
                                        FeatherIcons.arrowLeftCircle
                        )
                }
            , paragraph
                (Style.h2Font ++ [ Font.center ])
                [ text title ]
            , link
                []
                { url = path ++ next_design
                , label =
                    el []
                        (html <|
                            FeatherIcons.toHtml [] <|
                                FeatherIcons.withSize 36 <|
                                    FeatherIcons.withStrokeWidth 1.2 <|
                                        FeatherIcons.arrowRightCircle
                        )
                }
            ]
        ]


tabBar : Tab -> Int -> Element Msg
tabBar activeTab screenWidth =
    let
        widthButton =
            if screenWidth < 600 then
                Element.fill |> maximum (screenWidth - 10)

            else
                Element.px 200

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
            downloadButton widthButton attrs (Just (SelectTab tab)) (text label)
    in
    row
        [ width fill
        , Border.widthEach { bottom = 2, top = 2, left = 0, right = 0 }
        , Border.color <| rgb255 220 220 220
        , alignLeft
        ]
        [ buttonFor Publication "Publication"
        , buttonFor StructureTab "Structure"
        , buttonFor Similarity "Similarity"
        , buttonFor Solubility "Solubility"
        ]


detailsTable : List (ProteinDesign.DesignDetails Msg) -> Int -> Element Msg
detailsTable detailsList tableWidth =
    table
        [ width <| fillPortion 2 ]
        { data = detailsList
        , columns =
            [ { header = paragraph [ Font.bold, paddingXY 5 10, Border.widthEach { bottom = 2, top = 2, left = 0, right = 0 }, Border.color <| rgb255 220 220 220 ] [ text "Attribute" ]
              , width = px (tableWidth // 5)
              , view = \category -> paragraph Style.monospacedFont [ column [ width (px (tableWidth // 5 - 20)), height fill, scrollbarX, paddingXY 5 10 ] [ text category.header ] ]
              }
            , { header = paragraph [ Font.bold, paddingXY 10 10, Border.widthEach { bottom = 2, top = 2, left = 0, right = 0 }, Border.color <| rgb255 220 220 220 ] [ text "Value" ]
              , width = px (tableWidth * 4 // 5)
              , view = \detail -> paragraph Style.monospacedFont [ column [ width (px (tableWidth * 4 // 5)), height fill, scrollbarX, paddingXY 10 10 ] [ detail.property ] ]
              }
            ]
        }


designDetailsBodyTable : ProteinDesign -> Int -> Element Msg
designDetailsBodyTable proteinDesign screenWidth =
    let
        limitingScreenSize =
            900

        elPadding =
            40

        tableWidth =
            if screenWidth < limitingScreenSize then
                screenWidth - elPadding

            else
                (screenWidth * 2) // 3
    in
    detailsTable (ProteinDesign.designDetailsFromProteinDesign proteinDesign) tableWidth


flattenEnergy : Dict.Dict String (List ( String, Float )) -> List ( String, Float )
flattenEnergy d =
    d
        |> Dict.toList
        |> List.concatMap (\( k, lst ) -> List.map (\( sk, v ) -> ( k ++ "." ++ sk, v )) lst)


renderKeyValueTable : List ( String, Float ) -> Int -> Element Msg
renderKeyValueTable kvs tableWidth =
    table
        [ width <| fillPortion 2 ]
        { data = kvs
        , columns =
            [ { header = paragraph [ Font.bold, paddingXY 5 10, Border.widthEach { bottom = 2, top = 2, left = 0, right = 0 }, Border.color <| rgb255 220 220 220 ] [ text "Property" ]
              , width = px (tableWidth // 2)
              , view = \( k, v ) -> paragraph Style.monospacedFont [ column [ width (px (tableWidth // 2 - 20)), height fill, scrollbarX, paddingXY 5 10 ] [ text k ] ]
              }
            , { header = paragraph [ Font.bold, paddingXY 10 10, Border.widthEach { bottom = 2, top = 2, left = 0, right = 0 }, Border.color <| rgb255 220 220 220 ] [ text "Value" ]
              , width = px (tableWidth * 4 // 5)
              , view = \( k, v ) -> paragraph Style.monospacedFont [ column [ width (px (tableWidth // 2)), height fill, scrollbarX, paddingXY 10 10 ] [ text <| String.fromFloat v ] ]
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
                    [ HAtt.style "background-color" "#eee", HAtt.style "width" (String.fromInt (tableWidth - 160) ++ "px"), HAtt.style "padding" "4px", HAtt.style "border-radius" "3px" ]
                    [ Html.node "div"
                        [ HAtt.style "position" "relative" ]
                        [ Html.node "div" [ HAtt.attribute "style" barStyle ] []
                        , Html.node "div" [ HAtt.style "position" "absolute", HAtt.style "left" "8px", HAtt.style "top" "0px", HAtt.style "height" "20px", HAtt.style "line-height" "20px", HAtt.style "color" "white", HAtt.style "font-weight" "bold" ] [ Html.text pctText ]
                        ]
                    ]
                ]
    in
    Html.node "div" [] (List.map rowFor enriched)


renderSecondaryStructureHtml : List ( String, Float ) -> Int -> Html.Html Msg
renderSecondaryStructureHtml kvs tableWidth =
    let
        -- structure color mapping
        colorFor s =
            case s of
                "alpha_helix" ->
                    "#1f77b4"

                "beta_strand" ->
                    "#ff7f0e"

                "beta_bridge" ->
                    "#2ca02c"

                "3_10_helix" ->
                    "#d62728"

                "pi_helix" ->
                    "#9467bd"

                "turn" ->
                    "#8c564b"

                "bend" ->
                    "#e377c2"

                _ ->
                    "#7f7f7f"

        total =
            kvs |> List.map Tuple.second |> List.sum

        multiplier =
            if total <= 1.01 then
                100.0

            else
                1.0

        enriched =
            List.map (\tuple -> ( Tuple.first tuple, Tuple.second tuple * multiplier )) kvs

        segments =
            enriched
                |> List.map
                    (\( name, v ) ->
                        let
                            px =
                                String.fromInt (Basics.round (toFloat tableWidth * (v / 100.0))) ++ "px"

                            titleAttr =
                                name ++ ": " ++ String.fromFloat (toFloat (Basics.round (v * 100.0)) / 100.0) ++ "%"

                            color =
                                colorFor name
                        in
                        Html.node "div" [ HAtt.style "display" "inline-block", HAtt.style "height" "30px", HAtt.style "width" px, HAtt.style "background-color" color, HAtt.title titleAttr ] []
                    )
    in
    Html.node "div" [] segments


tabContent : Model -> ProteinDesign -> Int -> Int -> Int -> Int -> Element Msg
tabContent model proteinDesign screenWidth screenHeight tableWidth contentHeight =
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
        kvToDetail ( k, v ) =
            { header = k
            , property = paragraph Style.monospacedFont [ text <| String.fromFloat v ]
            }
    in
    case model.activeTab of
        Publication ->
            el [ width (px tableWidth), height <| px contentHeight, scrollbarY ] (detailsTable (select publicationHeaders) tableWidth)

        StructureTab ->
            let
                numResiduesF =
                    Maybe.withDefault 0.0 proteinDesign.physicochem.num_residues

                massF =
                    Maybe.withDefault proteinDesign.formula_weight proteinDesign.physicochem.mass

                chargeF =
                    Maybe.withDefault 0.0 proteinDesign.physicochem.charge

                pIF =
                    Maybe.withDefault 0.0 proteinDesign.physicochem.isoelectric_point

                packingF =
                    Maybe.withDefault 0.0 proteinDesign.physicochem.packing_density

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

        Similarity ->
            el [ width (px tableWidth), height <| px contentHeight, scrollbarY ] (detailsTable (select similarityHeaders) tableWidth)

        Solubility ->
            let
                hyd =
                    Maybe.withDefault 0.0 proteinDesign.physicochem.hydrophobic_fitness

                sol_total =
                    Maybe.withDefault 0.0 (Maybe.map .total proteinDesign.physicochem.solubility)

                sol_avg =
                    Maybe.withDefault 0.0 (Maybe.map .avg proteinDesign.physicochem.solubility)

                sol_min =
                    Maybe.withDefault 0.0 (Maybe.map .min proteinDesign.physicochem.solubility)

                sol_max =
                    Maybe.withDefault 0.0 (Maybe.map .max proteinDesign.physicochem.solubility)

                solDetails =
                    [ ( "Hydrophobic fitness", hyd ), ( "Total aggregation propensity", sol_total ), ( "Average aggregation propensity", sol_avg ), ( "Minimum aggregation propensity", sol_min ), ( "Maximum aggregation propensity", sol_max ) ]
                        |> List.map kvToDetail
            in
            el [ width (px tableWidth), height <| px contentHeight, scrollbarY ] (detailsTable solDetails tableWidth)


designDetailsBodyStructure : ProteinDesign -> Int -> Int -> Element Msg
designDetailsBodyStructure proteinDesign screenWidth screenHeight =
    let
        picHeight =
            screenHeight // 2
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
        elPadding =
            10

        narrowColumnWidth =
            screenWidth // 10 - elPadding

        wideColumnWidth =
            screenWidth - 4 * (narrowColumnWidth + elPadding)
    in
    column [ spacing 20 ]
        [ paragraph
            Style.h2Font
            [ text "Sequence"
            ]
        , table
            [ padding 2
            , width fill
            ]
            { data = proteinDesign.chains
            , columns =
                [ { header =
                        wrappedRow
                            [ Font.bold
                            , paddingXY 5 10
                            , Border.widthEach { bottom = 2, top = 2, left = 0, right = 0 }
                            , Border.color <| rgb255 220 220 220
                            ]
                            [ text "Chain ID" ]
                  , width = px narrowColumnWidth
                  , view =
                        \chain ->
                            wrappedRow
                                (Style.monospacedFont
                                    ++ [ width (fill |> maximum narrowColumnWidth)
                                       , height fill
                                       , scrollbarX
                                       , paddingXY 5 10
                                       ]
                                )
                                [ text chain.chain_id ]
                  }
                , { header =
                        wrappedRow
                            [ Font.bold
                            , paddingXY 5 10
                            , Border.widthEach { bottom = 2, top = 2, left = 0, right = 0 }
                            , Border.color <| rgb255 220 220 220
                            ]
                            [ text "Type" ]
                  , width = px narrowColumnWidth
                  , view =
                        \chain ->
                            wrappedRow
                                (Style.monospacedFont
                                    ++ [ width (fill |> maximum narrowColumnWidth)
                                       , height fill
                                       , scrollbarX
                                       , paddingXY 5 10
                                       ]
                                )
                                [ text chain.chain_type ]
                  }
                , { header =
                        wrappedRow
                            [ Font.bold
                            , paddingXY 5 10
                            , Border.widthEach { bottom = 2, top = 2, left = 0, right = 0 }
                            , Border.color <| rgb255 220 220 220
                            ]
                            [ text "Source" ]
                  , width = px narrowColumnWidth
                  , view =
                        \chain ->
                            wrappedRow
                                (Style.monospacedFont
                                    ++ [ width (fill |> maximum narrowColumnWidth)
                                       , height fill
                                       , scrollbarX
                                       , paddingXY 5 10
                                       ]
                                )
                                [ text <| String.toLower chain.chain_source ]
                  }
                , { header =
                        wrappedRow
                            [ Font.bold
                            , paddingXY 10 10
                            , Border.widthEach { bottom = 2, top = 2, left = 0, right = 0 }
                            , Border.color <| rgb255 220 220 220
                            ]
                            [ text "Sequence" ]
                  , width = px wideColumnWidth
                  , view =
                        \chain ->
                            wrappedRow
                                (Style.monospacedFont
                                    ++ [ width (fill |> maximum wideColumnWidth)
                                       , height fill
                                       , scrollbarX
                                       , paddingXY 5 10
                                       ]
                                )
                                [ text chain.chain_seq_unnat ]
                  }
                , { header =
                        wrappedRow
                            [ Font.bold
                            , paddingXY 5 10
                            , Border.widthEach { bottom = 2, top = 2, left = 0, right = 0 }
                            , Border.color <| rgb255 220 220 220
                            ]
                            [ text "Length" ]
                  , width = px narrowColumnWidth
                  , view =
                        \chain ->
                            wrappedRow
                                (Style.monospacedFont
                                    ++ [ width (fill |> maximum narrowColumnWidth)
                                       , height fill
                                       , scrollbarX
                                       , paddingXY 5 10
                                       ]
                                )
                                [ text <| String.fromInt chain.chain_length ]
                  }
                ]
            }
        ]


designDetailsBodyParagraphs : ProteinDesign -> Int -> Element Msg
designDetailsBodyParagraphs proteinDesign screenWidth =
    column
        [ width fill
        , spacing 20
        ]
        [ paragraph Style.h2Font [ text "Amino acid composition" ]
        , let
            tableWidth =
                if screenWidth < 900 then
                    screenWidth - 40

                else
                    (screenWidth * 2) // 3

            aaData =
                proteinDesign.physicochem.aa_composition
          in
          html <|
            Html.node "div"
                [ HAtt.style "width" (String.fromInt tableWidth ++ "px") ]
                [ renderAminoAcidHtml aaData tableWidth |> Html.map identity ]
        , paragraph Style.h2Font [ text "Secondary structure composition" ]
        , let
            tableWidth =
                if screenWidth < 900 then
                    screenWidth - 40

                else
                    (screenWidth * 2) // 3

            ssData =
                proteinDesign.physicochem.ss_composition
          in
          html <|
            Html.node "div"
                [ HAtt.style "width" (String.fromInt tableWidth ++ "px") ]
                [ renderSecondaryStructureHtml ssData tableWidth |> Html.map identity ]
        , paragraph Style.h2Font [ text "Energy" ]
        , let
            tableWidth =
                if screenWidth < 900 then
                    screenWidth - 40

                else
                    (screenWidth * 2) // 3
          in
          renderKeyValueTable (flattenEnergy proteinDesign.physicochem.energy)
            tableWidth
        , paragraph
            Style.h2Font
            [ text "Description"
            ]
        , paragraph
            (Style.monospacedFont
                ++ [ Font.justify
                   , width (fill |> maximum screenWidth)
                   ]
            )
            [ proteinDesign.abstract
                |> text
            ]
        , paragraph
            Style.h2Font
            [ text "Curation comments"
            ]
        , column
            (Style.monospacedFont
                ++ [ Font.justify
                   , width (fill |> maximum screenWidth)
                   , spacing 10
                   ]
            )
            (if proteinDesign.review_comment == [ "" ] then
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


designDetailsBody : Model -> ProteinDesign -> Int -> Int -> Element Msg
designDetailsBody model proteinDesign screenWidth screenHeight =
    let
        -- reserve a little padding space from the available height
        topAreaHeight =
            max 220 (screenHeight - 120)

        -- table width should match the previous logic used elsewhere
        tableWidth =
            if screenWidth < 900 then
                screenWidth - 40

            else
                (screenWidth * 2) // 3

        cardPadding =
            16

        -- height available for the tab content inside the card (subtract tabbar + paddings)
        contentHeight =
            max 120 (topAreaHeight - 80)
    in
    column
        (Style.bodyFont
            ++ [ width (fill |> maximum screenWidth)
               , paddingXY 30 20
               , spacing 30
               , centerX
               ]
        )
        [ -- card: tabs + their content
          column
            [ width (px tableWidth)
            , Background.color <| rgb255 250 250 250
            , Border.rounded 8
            , Border.width 1
            , Border.color <| rgb255 220 220 220
            , paddingXY cardPadding cardPadding
            ]
            [ tabBar model.activeTab screenWidth
            , el [ width fill, height <| px contentHeight, scrollbarY, paddingXY 0 4 ] (tabContent model proteinDesign screenWidth screenHeight tableWidth contentHeight)
            ]
        , designDetailsBodyStructure proteinDesign screenWidth screenHeight
        , designDetailsBodySequence proteinDesign screenWidth
        , designDetailsBodyParagraphs proteinDesign screenWidth
        ]
