{-# LANGUAGE OverloadedStrings #-}

module Common.Component.NavigationBar.View where

import Miso (View, (=:))
import Miso.Html.Property
import Miso.Property
import Miso.Html
import Miso.Svg.Property hiding (path_)
import Miso.Svg.Element
import qualified Miso.CSS as CSS

navbar :: View model action
navbar = div_
    [class_ "navbar"]
    [ div_
        [class_ "menu_button"]
        [div_ [class_ "menu_button--burger-icon"] []]
    , div_
        [class_ "breadcrumbs--wrapper"]
        [ div_
            [class_ "breadcrumbs"]
            [ div_
                [class_ "breadcrumb breadcrumb--clickable"]
                [ span_ [] ["Example"]
                , svg_
                    [class_ "breadcrumb--chevron-svg-forward"]
                    [use_ [href_ "#svg-chevron-right-forward"]]
                , div_
                    [class_ "breadcrumb--dots"]
                    [ svg_
                        [class_ "breadcrumb--dots-dot"]
                        [use_ [href_ "#svg-dot"]]
                    , svg_
                        [class_ "breadcrumb--dots-dot"]
                        [use_ [href_ "#svg-dot"]]
                    , svg_
                        [class_ "breadcrumb--dots-dot"]
                        [use_ [href_ "#svg-dot"]]
                    ]
                ]
            , div_
                [class_ "breadcrumb breadcrumb--clickable"]
                [ svg_
                    [class_ "breadcrumb--chevron-svg-aft"]
                    [use_ [href_ "#svg-chevron-right-aft"]]
                , span_ [] ["/b/, and 5 other boards"]
                , svg_
                    [class_ "breadcrumb--chevron-svg-forward"]
                    [use_ [href_ "#svg-chevron-right-forward"]]
                , div_
                    [class_ "breadcrumb--dots"]
                    [ svg_
                        [class_ "breadcrumb--dots-dot"]
                        [use_ [href_ "#svg-dot"]]
                    , svg_
                        [class_ "breadcrumb--dots-dot"]
                        [use_ [href_ "#svg-dot"]]
                    , svg_
                        [class_ "breadcrumb--dots-dot"]
                        [use_ [href_ "#svg-dot"]]
                    ]
                ]
            , div_
                [class_ "breadcrumb"]
                [ svg_
                    [class_ "breadcrumb--chevron-svg-aft"]
                    [use_ [href_ "#svg-chevron-right-aft"]]
                , span_ [] ["13337.html"]
                ]
            ]
        ]
    ]

supportingSvgs :: View model action
supportingSvgs = section_
    [CSS.style_ ["display" =: "none"]]
    [ svg_
        [ preserveAspectRatio_ "none"
        , viewBox_ "0 0 100 100"
        , id_ "svg-chevron-right-forward"
        ]
        [ path_
            [ strokeLinejoin_ "miter"
            , textProp "vector-effect" "non-scaling-stroke"
            , d_ "M 0 8 L 15 8 L 92 50 L 15 92 L 0 92"
            ]
        ]
    , svg_
        [ preserveAspectRatio_ "none"
        , viewBox_ "0 0 100 100"
        , id_ "svg-chevron-right-aft"
        ]
        [ path_
            [ strokeLinejoin_ "miter"
            , textProp "vector-effect" "non-scaling-stroke"
            , d_ "M 25 8 L 15 8 L 92 50 L 15 92 L 25 92"
            ]
        ]
    , svg_
        [viewBox_ "0 0 10 10", id_ "svg-dot"]
        [ path_
            [ strokeLinecap_ "round"
            , textProp "vector-effect" "non-scaling-stroke"
            , d_ "M 5 5 h0"
            ]
        ]
    ]
