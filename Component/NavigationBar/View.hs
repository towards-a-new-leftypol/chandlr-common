{-# LANGUAGE OverloadedStrings #-}

module Common.Component.NavigationBar.View where

import Miso (View, (=:))
import Miso.Html.Property
import Miso.Property
import Miso.Html
import Miso.Svg.Property hiding (path_)
import Miso.Svg.Element
import qualified Miso.CSS as CSS

import Common.Component.NavigationBar.Action

navbar :: View model Action
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
                [ class_ "breadcrumb breadcrumb--clickable"
                , onClick ClickSites
                ]
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
                [ class_ "breadcrumb breadcrumb--clickable"
                , onClick ClickBoards
                ]
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
supportingSvgs = svg_
    [ CSS.style_ ["display" =: "none"]
    , aria_ "hidden" "true"
    ]
    [ symbol_
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
    , symbol_
        [ preserveAspectRatio_ "none"
        , viewBox_ "0 0 100 100"
        , id_ "svg-chevron-right-aft"
        ]
        [ path_
            [ strokeLinejoin_ "miter"
            , textProp "vector-effect" "non-scaling-stroke"
            , d_ "M 15 8 L 92 50 L 15 92"
            ]
        ]
    , symbol_
        [viewBox_ "0 0 10 10", id_ "svg-dot"]
        [ path_
            [ strokeLinecap_ "round"
            , textProp "vector-effect" "non-scaling-stroke"
            , d_ "M 5 5 h0"
            ]
        ]
    ]

{-
Hamburger menu from youtube:
<svg xmlns="http://www.w3.org/2000/svg" height="24" viewBox="0 0 24 24" width="24" focusable="false" aria-hidden="true" style="pointer-events: none; display: inherit; width: 100%; height: 100%;">
<path d="M20 5H4a1 1 0 000 2h16a1 1 0 100-2Zm0 6H4a1 1 0 000 2h16a1 1 0 000-2Zm0 6H4a1 1 0 000 2h16a1 1 0 000-2Z"></path>
</svg>

maybe it should be a logo instead, ie a picture of Chandler maybe with a css filter on it?

Also probably need something like this in the head to prevent the svgs breadcrumb--chevron-svg-forward
from flashing:

  <style>
    /* 1. Declare order: 'critical' first, 'main' second */
    @layer critical, main;

    /* 2. Critical rules to neutralize the black triangle flash */
    @layer critical {
      .breadcrumb--chevron-svg-forward use {
        visibility: hidden; /* prevents the fallback shape from painting */
        fill: transparent;
        width: 1em;         /* reserve layout space */
        height: 1em; /* or whatever from newstyle.css */
        display: inline-block;
      }
    }
  </style>
-}
