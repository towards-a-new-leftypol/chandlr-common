{-# LANGUAGE OverloadedStrings #-}

module Common.Component.NavigationBar.NavMenu where

import Miso
    ( View
    , MisoString
    , text
    , vfrag
    , toMisoString
    , Attribute
    )
import Miso.Html hiding (style_)
import Miso.Html.Property hiding (label_)
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as L

import Common.Component.NavigationBar.Action
import Common.Component.NavigationBar.Model
import qualified Common.Component.Modal as Modal
import qualified Common.Network.SiteType as Site
import qualified Common.Network.BoardType as Board

navmenu :: Model -> View Model Action
navmenu Model { menuState = Closed } = vfrag []
navmenu m = div_ [ class_ "modal-dialog" ]
    [ Modal.view
        Modal.Model
            { Modal.cancel = CancelMenu
            , Modal.submit = SubmitMenuChoice
            , Modal.content = content m
            , Modal.title = title m
            , Modal.action = "Apply"
            }
    ]

    where
        content :: Model -> View Model Action
        content Model { menuState = Closed } = vfrag []
        content m_@Model { menuState = ChooseBoards } = div_
                [ class_ "modal-dialog__content" ]
                [ chooseBoards m_ ]
        content m_@Model { menuState = ChooseSites } = div_
                [ class_ "modal-dialog__content" ]
                [ div_ [ class_ "modal-dialog__inline-content" ] allOrNone
                , chooseSites m_
                ]

        title :: Model -> MisoString
        title Model { menuState = ChooseSites } = "Choose Sites"
        title Model { menuState = ChooseBoards } = "Choose Boards"
        title _ = ""


allOrNone :: [ View model Action ]
allOrNone =
    [ div_ [ class_ "button" ] [ span_ [] [ "All" ] ]
    , div_ [ class_ "button" ] [ span_ [] [ "None" ] ]
    ]


chooseBoards ::  Model -> View Model Action
chooseBoards model = vfrag $
    [ siteBoardsSection (site, L.toList (Site.boards site))
    | site <- sites
    ]

    where
        sites :: [ Site.Site ]
        sites =
            case currentSites model of
                All -> sitesAndBoards model
                CurrentSites selectedSites -> Set.toList selectedSites

        siteBoardsSection :: (Site.Site, [ Board.Board ]) -> View Model Action
        siteBoardsSection (s, bs) = vfrag
            [ div_ [ class_ "modal-dialog__inline-content" ]
                (h2_ [] [ text $ Site.name s ] : allOrNone)
            , div_ [ class_ "modal-dialog__grid-column-content" ] (map pickBoard bs)
            ]

        pickBoard :: Board.Board -> View Model Action
        pickBoard
         b = div_
                classes
                [ input_
                    [ name_ "include-board"
                    , id_ ident
                    , type_ "checkbox"
                    , onChange $ const $ ToggleBoard b
                    -- , checked_ False
                    ]
                , label_
                    [ for_ ident ]
                    [ text $ "/" <> Board.pathpart b <> "/"
                    ]
                ]

            where
                ident :: MisoString
                ident = "board-" <> toMisoString (Board.board_id b)

                selected :: Bool
                selected = False

                classes :: [ Attribute a ]
                classes
                    | selected = class_ "site-choice__selected" : boardChoiceClass
                    | otherwise = boardChoiceClass

                boardChoiceClass :: [ Attribute a ]
                boardChoiceClass = [ class_ "site-choice", class_ "site-choice__board" ]


chooseSites ::  Model -> View Model Action
chooseSites model = vfrag $ map pickSite (sitesAndBoards model)
    where
        pickSite :: Site.Site -> View Model Action
        pickSite s =
            div_
                classes
                [ input_
                    [ name_ "include-site"
                    , id_ ident
                    , type_ "checkbox"
                    , onChange $ const $ ToggleSite s
                    , checked_ selected
                    ]
                , label_ [ for_ ident ] [ text $ Site.name s ]
                ]

            where
                ident :: MisoString
                ident = "site-" <> toMisoString (Site.site_id s)

                selected :: Bool
                selected =
                    case currentSites model of
                        All -> True
                        CurrentSites sitesSet -> Set.member s sitesSet

                classes :: [ Attribute a ]
                classes
                    | selected = class_ "site-choice__selected" : siteChoiceClass
                    | otherwise = siteChoiceClass

                siteChoiceClass :: [ Attribute a ]
                siteChoiceClass = [ class_ "site-choice" ]
