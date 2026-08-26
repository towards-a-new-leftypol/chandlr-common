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
import Data.Text (Text)

import Common.Component.NavigationBar.Action
import Common.Component.NavigationBar.Model
import qualified Common.Network.BoardType as Board
import qualified Common.Network.SiteType as Site

data ThreadPath = ThreadPath
    { threadPathSite  :: MisoString
    , threadPathBoard :: MisoString
    , threadPathId    :: MisoString
    }


navbar :: Props -> Model -> View context Action
navbar p m =
    let
        mThreadPath = maybeThreadPath (currentUri p)
        sitesLabel  = maybe (sitesText m) threadPathSite mThreadPath
        boardsLabel = maybe
            [ text $ boardsText m ]
            (slashes . threadPathBoard)
            mThreadPath
    in div_
        [ class_ "navbar" ]
        [ div_
            [ class_ "menu_button" ]
            [ div_ [ class_ "menu_button--burger-icon" ] [] ]
        , div_
            [ class_ "breadcrumbs--wrapper" ]
            [ div_
                [ class_ "breadcrumbs" ]
                ([ sitesCrumb sitesLabel
                 , boardsCrumb boardsLabel
                 ]
                 ++ maybeThreadCrumb mThreadPath)
            ]
        ]

    where
        slashes pathpart =
            [ span_ [ class_ "breadcrumb--boardslash" ] [ "/" ]
            , text pathpart
            , span_ [ class_ "breadcrumb--boardslash" ] [ "/" ]
            ]



sitesCrumb :: MisoString -> View context Action
sitesCrumb label = div_
    [ class_ "breadcrumb breadcrumb--clickable"
    , onClick ClickSites
    ]
    [ span_ [] [ text label ]
    , chevronForward
    , crumbDots
    ]


boardsCrumb :: [ View context Action ] -> View context Action
boardsCrumb label = div_
    [ class_ "breadcrumb breadcrumb--clickable"
    , onClick ClickBoards
    ]
    [ chevronAft
    , span_ [] label
    , chevronForward
    , crumbDots
    ]


maybeThreadCrumb :: Maybe ThreadPath -> [ View context Action ]
maybeThreadCrumb Nothing = []
maybeThreadCrumb (Just tp) =
    [ div_
        [ class_ "breadcrumb" ]
        [ chevronAft
        , span_ [] [ text $ threadPathId tp ]
        ]
    ]


chevronForward :: View model action
chevronForward = svg_
    [ class_ "breadcrumb--chevron-svg-forward"
    , textProp "width" "0"
    , textProp "height" "0"
    , textProp "fill" "none"
    , textProp "stroke" "none"
    , aria_ "hidden" "true"
    ]
    [ use_ [ href_ "#svg-chevron-right-forward" ] ]


chevronAft :: View model action
chevronAft = svg_
    [ class_ "breadcrumb--chevron-svg-aft"
    , textProp "width" "0"
    , textProp "height" "0"
    , textProp "fill" "none"
    , textProp "stroke" "none"
    , aria_ "hidden" "true"
    ]
    [ use_ [ href_ "#svg-chevron-right-aft" ] ]


crumbDots :: View model action
crumbDots = div_
    [ class_ "breadcrumb--dots" ]
    [ dot, dot, dot ]
    where
        dot = svg_
            [ class_ "breadcrumb--dots-dot"
            , textProp "width" "0"
            , textProp "height" "0"
            , textProp "fill" "none"
            , textProp "stroke" "none"
            , aria_ "hidden" "true"
            ]
            [ use_ [ href_ "#svg-dot" ] ]


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
                        name = Site.name (Set.findMin sSet)
                    in
                        case n of
                            1 -> name
                            _ -> name <> " +" <> toMisoString (n - 1)
                                <> (if n > 2 then " sites" else " site")


boardsText :: Model -> MisoString
boardsText m
    | allBoardsSelected m = "All boards"
    | otherwise =
        if Set.null boards
        then "<Nothing>"
        else
            let n = Set.size boards
                name = Board.pathpart (Set.findMin boards)
            in
                case n of
                    1 -> name
                    _ -> name <> " +" <> toMisoString (n - 1)
                        <> (if n > 2 then " boards" else " board")
        where
            boards = selectedBoards m


maybeThreadPath :: URI -> Maybe ThreadPath
maybeThreadPath = fromRight Nothing . routeResult

    where
        routeResult uri = route (Proxy :: Proxy (Route (View () ()))) handlers (const uri) undefined

        handlers = hLatest :<|> hThread :<|> hBoard :<|> hSearch

        hLatest :: a -> h -> m -> Maybe ThreadPath
        hLatest = const $ const $ const Nothing

        hThread :: Text -> Text -> Integer -> h -> m -> Maybe ThreadPath
        hThread site board x _ _ =
            Just $ ThreadPath (toMisoString site) (toMisoString board) $ toMisoString (show x <> ".html")

        hBoard :: a -> a -> h -> m -> Maybe ThreadPath
        hBoard = const $ const $ const $ const Nothing

        hSearch :: Maybe String -> h -> m -> Maybe ThreadPath
        hSearch = const $ const $ const Nothing


supportingSvgs :: View model action
supportingSvgs = svg_
    [ CSS.style_ ["display" =: "none"]
    , aria_ "hidden" "true"
    ]
    [ symbol_
        [ preserveAspectRatio_ "none"
        , viewBox_ "0 0 100 100"
        , id_ "svg-chevron-right-forward"
        , textProp "overflow" "visible"
        ]
        [ path_
            [ strokeLinejoin_ "miter"
            , textProp "vector-effect" "non-scaling-stroke"
            , d_ "M 0 0 L 10 0 L 100 50 L 10 100 L 0 100"
            ]
        ]
    , symbol_
        [ preserveAspectRatio_ "none"
        , viewBox_ "0 0 100 100"
        , id_ "svg-chevron-right-aft"
        , textProp "overflow" "visible"
        ]
        [ path_
            [ strokeLinejoin_ "miter"
            , textProp "vector-effect" "non-scaling-stroke"
            , d_ "M 15 0 L 92 50 L 15 100"
            ]
        ]
    , symbol_
        [ viewBox_ "0 0 10 10"
        , id_ "svg-dot"
        , textProp "overflow" "visible"
        ]
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
