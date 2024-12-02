module Plots exposing (PlotData, RenderPlotState(..), timelinePlotDesigns, timelinePlotStubs, timelinePlotView)

import Date exposing (Unit(..))
import Element exposing (..)
import Element.Keyed as Keyed
import Get exposing (getScreenWidthInt, getScreenWidthString)
import Html
import Html.Attributes as HAtt
import ProteinDesign exposing (ProteinDesign, ProteinDesignStub)
import Time exposing (Month(..))
import Vega exposing (..)


type alias PlotData =
    { plotId : String, spec : Spec }


type RenderPlotState
    = AwaitingRender Int
    | Rendered
    | WillRender



---- VIEWS ----


timelinePlotId : String
timelinePlotId =
    "timeline-plot"


timelinePlotView : Maybe Float -> Element msg
timelinePlotView widthF =
    let
        widthL =
            px (getScreenWidthInt widthF)

        widthS =
            getScreenWidthString widthF
    in
    column
        [ spacing 15, Element.width widthL, centerX ]
        [ Keyed.el [ centerX ]
            ( timelinePlotId
            , Html.div
                [ HAtt.id timelinePlotId
                , HAtt.style "width" widthS
                ]
                [ Html.div
                    [ HAtt.style "border-radius" "5px"
                    , HAtt.style "width" widthS
                    , HAtt.style "background-color" "#d3d3d3"
                    ]
                    []
                ]
                |> html
            )
        ]



---- VEGA SPECS ----


timelinePlotDesigns : Float -> List ProteinDesign -> PlotData
timelinePlotDesigns widthF designs =
    let
        ds =
            let
                designsTable =
                    dataFromColumns "designs" []
                        << dataColumn "year"
                            (List.map .release_date designs
                                |> List.map Date.year
                                |> List.map toFloat
                                |> vNums
                            )
                        << dataColumn "month"
                            (List.map .release_date designs
                                |> List.map Date.monthNumber
                                |> List.map toFloat
                                |> vNums
                            )
                        << dataColumn "id"
                            (List.map .pdb designs
                                |> vStrs
                            )
                        << dataColumn "href"
                            (List.map (\d -> "designs/" ++ d.pdb) designs
                                |> vStrs
                            )
            in
            dataSource [ designsTable [] ]

        si =
            signals
                << signal "cx" [ siUpdate "width / 2" ]
                << signal "cy" [ siUpdate "height / 2 + 100" ]
                << signal "radius" [ siValue (vNum 4) ]
                << signal "collide" [ siValue (vNum 1) ]
                << signal "gravityX" [ siValue (vNum 10) ]
                << signal "gravityY" [ siValue (vNum 0.03) ]
                << signal "static" [ siValue vTrue ]

        sc =
            scales
                << Vega.scale "xScale"
                    [ scType scLinear
                    , scRange raWidth
                    , scDomain (doData [ daDataset "designs", daField (field "year") ])
                    , scZero false
                    ]
                << Vega.scale "cScale"
                    [ scType scOrdinal
                    , scRange (raScheme (str "category20c") [])
                    ]

        ax =
            axes
                << axis "xScale"
                    siTop
                    [ axDomain false
                    , axFormat <| str ".4"
                    ]

        mk =
            marks
                << mark symbol
                    [ mName "nodes"
                    , mFrom [ srData (str "designs") ]
                    , mEncode
                        [ enEnter
                            [ maFill [ vScale "cScale", vField (field "year") ]
                            , maCustom "xFocus" [ vScale "xScale", vField (field "year"), vBand (num 0.5) ]
                            , maCustom "yFocus" [ vSignal "cy" ]
                            ]
                        , enUpdate
                            [ maSize [ vSignal "100" ]
                            , maStroke [ white ]
                            , maStrokeWidth [ vNum 1 ]
                            , maZIndex [ vNum 0 ]
                            ]
                        , enHover
                            [ maStroke [ vStr "purple" ]
                            , maStrokeWidth [ vNum 3 ]
                            , maZIndex [ vNum 1 ]
                            , maCursor [ cursorValue cuPointer ]
                            , maTooltip [ vField (field "id") ]
                            , maHRef [ vField (field "href") ]
                            ]
                        ]
                    , mTransform
                        [ trForce
                            [ fsIterations (num 50)
                            , fsStatic (booSignal "static")
                            , fsForces
                                [ foCollide (numSignal "radius") [ fpIterations (numSignal "collide") ]
                                , foX (field "xFocus") [ fpStrength (numSignal "gravityX") ]
                                , foY (field "yFocus") [ fpStrength (numSignal "gravityY") ]
                                ]
                            ]
                        ]
                    ]
    in
    { plotId = timelinePlotId
    , spec =
        toVega
            [ Vega.width (0.85 * widthF), Vega.height 300, ds, si [], sc [], ax [], mk [] ]
    }


timelinePlotStubs : Float -> Float -> List ProteinDesignStub -> PlotData
timelinePlotStubs widthF heightF designs =
    let
        ds =
            let
                designsTable =
                    dataFromColumns "designs" []
                        << dataColumn "year"
                            (List.map .release_date designs
                                |> List.map Date.year
                                |> List.map toFloat
                                |> vNums
                            )
                        << dataColumn "month"
                            (List.map .release_date designs
                                |> List.map Date.monthNumber
                                |> List.map toFloat
                                |> vNums
                            )
                        << dataColumn "id"
                            (List.map .pdb designs
                                |> vStrs
                            )
                        << dataColumn "href"
                            (List.map (\d -> "designs/" ++ d.pdb) designs
                                |> vStrs
                            )
            in
            dataSource [ designsTable [] ]

        si =
            signals
                << signal "cx" [ siUpdate "width / 2" ]
                << signal "cy" [ siUpdate "height / 2 + 100" ]
                << signal "radius" [ siValue (vNum 4) ]
                << signal "collide" [ siValue (vNum 1) ]
                << signal "gravityX" [ siValue (vNum 10) ]
                << signal "gravityY" [ siValue (vNum 0.03) ]
                << signal "static" [ siValue vTrue ]

        sc =
            scales
                << Vega.scale "xScale"
                    [ scType scLinear
                    , scRange raWidth
                    , scDomain (doData [ daDataset "designs", daField (field "year") ])
                    , scZero false
                    ]
                << Vega.scale "cScale"
                    [ scType scOrdinal
                    , scRange (raScheme (str "category20c") [])
                    ]

        ax =
            axes
                << axis "xScale"
                    siTop
                    [ axDomain false
                    , axFormat <| str ".4"
                    , axTickCount <| num 16
                    , axOffset <| vNum 250.0
                    , axLabelFontSize <| num 12.0
                    ]

        mk =
            marks
                << mark symbol
                    [ mName "nodes"
                    , mFrom [ srData (str "designs") ]
                    , mEncode
                        [ enEnter
                            [ maFill [ vScale "cScale", vField (field "year") ]
                            , maCustom "xFocus" [ vScale "xScale", vField (field "year"), vBand (num 0.5) ]
                            , maCustom "yFocus" [ vSignal "cy" ]
                            ]
                        , enUpdate
                            [ maSize [ vSignal "100" ]
                            , maStroke [ white ]
                            , maStrokeWidth [ vNum 1 ]
                            , maZIndex [ vNum 0 ]
                            ]
                        , enHover
                            [ maStroke [ vStr "purple" ]
                            , maStrokeWidth [ vNum 3 ]
                            , maZIndex [ vNum 1 ]
                            , maCursor [ cursorValue cuPointer ]
                            , maTooltip [ vField (field "id") ]
                            , maHRef [ vField (field "href") ]
                            ]
                        ]
                    , mTransform
                        [ trForce
                            [ fsIterations (num 50)
                            , fsStatic (booSignal "static")
                            , fsForces
                                [ foCollide (numSignal "radius") [ fpIterations (numSignal "collide") ]
                                , foX (field "xFocus") [ fpStrength (numSignal "gravityX") ]
                                , foY (field "yFocus") [ fpStrength (numSignal "gravityY") ]
                                ]
                            ]
                        ]
                    ]
    in
    { plotId = timelinePlotId
    , spec =
        toVega
            [ Vega.width (0.85 * widthF), Vega.height (0.5 * heightF), Vega.padding 50, ds, si [], sc [], ax [], mk [] ]
    }
