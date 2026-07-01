{-# LANGUAGE OverloadedStrings #-}

module Common.Component.NavigationBar.View where

import Miso (View, (=:), MisoString, toMisoString, URI (..), text)
import Miso.Html.Property
import Miso.Property
import Miso.Html
import Miso.Svg.Property hiding (path_)
import Miso.Svg.Element
import qualified Miso.CSS as CSS
import qualified Data.Set as Set
import Data.Proxy (Proxy (..))
import Servant.API hiding (URI)
import Common.FrontEnd.Routes (Route)
import Servant.Miso.Router (route)
import Data.Either (fromRight)

import Common.Component.NavigationBar.Action
import Common.Component.NavigationBar.Model
import qualified Common.Network.BoardType as Board
import qualified Common.Network.SiteType as Site

navbar :: Model -> View Model Action
navbar m = div_
    [ class_ "navbar" ]
    [ div_
        [ class_ "menu_button" ]
        [ div_ [ class_ "menu_button--burger-icon" ] [] ]
    , div_
        [ class_ "breadcrumbs--wrapper" ]
        [ div_
            [ class_ "breadcrumbs" ]
            ([ div_
                [ class_ "breadcrumb breadcrumb--clickable"
                , onClick ClickSites
                ]
                [ span_ [] [ text $ sitesText m ]
                , svg_
                    [class_ "breadcrumb--chevron-svg-forward"]
                    [use_ [href_ "#svg-chevron-right-forward"]]
                , div_
                    [class_ "breadcrumb--dots"]
                    [ svg_ [class_ "breadcrumb--dots-dot"] [use_ [href_ "#svg-dot"]]
                    , svg_ [class_ "breadcrumb--dots-dot"] [use_ [href_ "#svg-dot"]]
                    , svg_ [class_ "breadcrumb--dots-dot"] [use_ [href_ "#svg-dot"]]
                    ]
                ]
            , div_
                [ class_ "breadcrumb breadcrumb--clickable"
                , onClick ClickBoards
                ]
                [ svg_
                    [class_ "breadcrumb--chevron-svg-aft"]
                    [use_ [href_ "#svg-chevron-right-aft"]]
                , span_ [] [ text $ boardsText m ]
                , svg_
                    [class_ "breadcrumb--chevron-svg-forward"]
                    [use_ [href_ "#svg-chevron-right-forward"]]
                , div_
                    [class_ "breadcrumb--dots"]
                    [ svg_ [class_ "breadcrumb--dots-dot"] [use_ [href_ "#svg-dot"]]
                    , svg_ [class_ "breadcrumb--dots-dot"] [use_ [href_ "#svg-dot"]]
                    , svg_ [class_ "breadcrumb--dots-dot"] [use_ [href_ "#svg-dot"]]
                    ]
                ]
            ]
            ++ maybeThreadCrumb m
            )
        ]
    ]

sitesText :: Model -> MisoString
sitesText m
    | allBoardsSelected m = "All websites"
    | otherwise =
        case currentSites m of
            All -> "All sites"
            CurrentSites sSet ->
                if Set.null sSet
                then "<Nothing>"
                else
                    let n = Set.size sSet
                    in
                        Site.name (Set.findMin sSet)
                        <> " +" <> toMisoString n
                        <> (if n > 1 then " sites" else " site")


boardsText :: Model -> MisoString
boardsText m
    | allBoardsSelected m = "All boards"
    | otherwise =
        if Set.null boards
        then "<Nothing>"
        else
            let n = Set.size boards
            in
                Board.pathpart (Set.findMin boards)
                <> " +" <> toMisoString n
                <> (if n > 1 then " boards" else " board")
        where
            boards = selectedBoards m

maybeThreadCrumb :: Model -> [ View Model Action ]
maybeThreadCrumb m =
    case maybeBoardThreadId (currentUri m) of
        Nothing -> []
        Just x ->
            [ div_
                [ class_ "breadcrumb" ]
                [ svg_
                    [ class_ "breadcrumb--chevron-svg-aft" ]
                    [ use_ [ href_ "#svg-chevron-right-aft" ] ]
                , span_ [] [ text x ]
                ]
            ]


maybeBoardThreadId :: URI -> Maybe MisoString
maybeBoardThreadId = fromRight Nothing . routeResult

    where
        routeResult uri = route (Proxy :: Proxy (Route (View () ()))) handlers (const uri) undefined

        handlers = hLatest :<|> hThread :<|> hBoard :<|> hSearch

        hLatest :: a -> h -> m -> Maybe MisoString
        hLatest = const $ const $ const Nothing

        hThread :: a -> a -> Integer -> m -> Maybe MisoString
        hThread _ _ x _ = Just $ toMisoString  $ show x <> ".html"

        hSearch :: Maybe String -> m -> Maybe MisoString
        hSearch = const $ const Nothing

        hBoard :: a -> a -> m -> Maybe MisoString
        hBoard = const $ const $ const Nothing


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
