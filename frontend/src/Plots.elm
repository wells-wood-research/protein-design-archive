module Plots exposing (PlotData, RenderPlotState(..), timelinePlotStubs, timelinePlotView)

import Date exposing (Unit(..))
import Element exposing (..)
import Element.Keyed as Keyed
import Get exposing (getScreenWidthString)
import Html
import Html.Attributes as HAtt
import ProteinDesign exposing (ProteinDesignStub)
import Time exposing (Month(..))
import Urls
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


timelinePlotView : Float -> Float -> Element msg
timelinePlotView widthF heightF =
    let
        widthS =
            getScreenWidthString (Just widthF)
    in
    column
        [ spacing 15
        , Element.width (px (round widthF))
        , centerX
        , Element.height (px (round heightF))

        -- This prevents the "bleeding" onto the download area
        , htmlAttribute (HAtt.style "overflow" "hidden")
        ]
        [ Keyed.el [ centerX ]
            ( timelinePlotId
            , Html.div
                [ HAtt.id timelinePlotId
                , HAtt.style "width" widthS
                , HAtt.style "height" (String.fromInt (round heightF) ++ "px")
                ]
                []
                |> html
            )
        ]



---- VEGA SPECS ----


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
                            (List.map (\d -> Urls.internalRelatedLink ++ d.pdb) designs
                                |> vStrs
                            )
            in
            dataSource [ designsTable [] ]

        si =
            signals
                << signal "cx" [ siUpdate "width / 2" ]
                << signal "cy" [ siUpdate "(height / 2) + 200" ]
                << signal "radius" [ siValue (vNum 5) ]
                << signal "collide" [ siValue (vNum 1) ]
                << signal "gravityX" [ siValue (vNum 8) ]
                << signal "gravityY" [ siValue (vNum 0.05) ]
                << signal "static" [ siValue vTrue ]

        ax =
            axes
                << axis "xScale"
                    siTop
                    -- Labels at the top
                    [ axDomain false
                    , axFormat <| str ".4"
                    , axTickCount <| num 16
                    , axLabelFontSize <| num 12.0

                    -- This ensures the ticks and labels have a small buffer from the top edge
                    , axOffset (vNum 5)
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
                            -- Match visual size to collision radius (Area = pi * r^2)
                            [ maSize [ vSignal "pow(radius, 2) * 3.14" ]
                            , maStroke [ white ]
                            , maStrokeWidth [ vNum 0.5 ]
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
                            [ fsIterations (num 80) -- Reduce from 150. Too many iterations "crystallizes" the cloud.
                            , fsStatic (booSignal "static")
                            , fsForces
                                [ -- Reduce fpIterations to 1 or 2 to allow for a "looser" feel
                                  foCollide (numSignal "radius") [ fpIterations (num 1) ]
                                , foX (field "xFocus") [ fpStrength (numSignal "gravityX") ]
                                , foY (field "yFocus") [ fpStrength (numSignal "gravityY") ]
                                ]
                            ]
                        ]
                    ]

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
    in
    { plotId = timelinePlotId
    , spec =
        toVega
            [ Vega.width (0.85 * widthF)
            , Vega.height (heightF - 100) -- Give more room for top/bottom margins

            -- Increase top padding specifically for the siTop axis labels
            , Vega.paddings 40 20 40 20
            , ds
            , si []
            , sc []
            , ax []
            , mk []
            ]
    }
