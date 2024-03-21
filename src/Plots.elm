module Plots exposing (PlotData, timelinePlotData, timelinePlotView)

import Date exposing (Unit(..))
import Element exposing (..)
import Element.Font as Font
import Element.Keyed as Keyed
import Html
import Html.Attributes as HAtt
import ProteinDesign exposing (ProteinDesign)
import Time exposing (Month(..))
import Vega exposing (..)


type alias PlotData =
    { plotId : String, spec : Spec }



---- VIEWS ----


timelinePlotId : String
timelinePlotId =
    "timeline-plot"


timelinePlotView : Element msg
timelinePlotView =
    column
        [ spacing 15, Element.width fill ]
        [ Keyed.el [ centerX ]
            ( timelinePlotId
            , Html.div
                [ HAtt.id timelinePlotId
                , HAtt.style "width" "100%"
                , HAtt.style "width" "100%"
                ]
                [ Html.div
                    -- [ HAtt.style "height" "200px"
                    -- , HAtt.style "width" "100%"
                    [ HAtt.style "border-radius" "5px"
                    , HAtt.style "background-color" "#d3d3d3"
                    ]
                    []
                ]
                |> html
            )
        ]



---- VEGA SPECS ----


timelinePlotData : List ProteinDesign -> PlotData
timelinePlotData designs =
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
            in
            dataSource [ designsTable [] ]

        si =
            signals
                << signal "cx" [ siUpdate "width / 2" ]
                << signal "cy" [ siUpdate "height / 2" ]
                << signal "radius" [ siValue (vNum 8) ]
                << signal "collide" [ siValue (vNum 1) ]
                << signal "gravityX" [ siValue (vNum 0.5) ]
                << signal "gravityY" [ siValue (vNum 0.1) ]
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
                    siBottom
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
                            [ maSize [ vSignal "pow(2 * radius, 2)" ]
                            , maStroke [ white ]
                            , maStrokeWidth [ vNum 1 ]
                            , maZIndex [ vNum 0 ]
                            ]
                        , enHover
                            [ maStroke [ vStr "purple" ]
                            , maStrokeWidth [ vNum 3 ]
                            , maZIndex [ vNum 1 ]
                            ]
                        ]
                    , mTransform
                        [ trForce
                            [ fsIterations (num 300)
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
            [ Vega.width 800, Vega.height 100, Vega.padding 30, autosize [ asNone ], ds, si [], sc [], ax [], mk [] ]
    }
