module Common.FrontEnd.Model where

import Miso (URI)
import Miso.String (MisoString)
import Miso.Lens (Lens, LensCore (..))

import Common.FrontEnd.Action (Action)
import qualified Common.Component.Thread.Types  as Thread
import Common.Network.CatalogPostType (CatalogPost)
import qualified Common.Component.Search.SearchTypes as Search
import Common.Network.SiteType (Site)
import Common.Network.BoardType (Board)
import Common.FrontEnd.Types (Time)

data Model = Model
    { current_uri :: URI
    , media_root_ :: MisoString
    , current_time :: Time
    , search_term :: MisoString
    , page_title :: MisoString
    , on_client_mounted_initial_actions ::  [ Action ]
    , thread_message :: Maybe Thread.Message
    , pg_api_root :: MisoString
    , client_fetch_count :: Int
    , catalog_posts :: [ CatalogPost ]
    , between_pages :: Bool
    , admin :: Bool
    -- , initialized :: Bool
    , client_mounted :: Bool
    , search_mounted :: Bool
    , search_message :: Maybe Search.MessageIn
    , all_sites_and_boards :: [ Site ]
    , hydrated :: Bool
    , sites_and_boards_loaded :: Bool
    , selected_boards :: Maybe [ Board ]
    } deriving Eq

getSetCatalogPosts :: Lens Model [ CatalogPost ]
getSetCatalogPosts =
    Lens
        catalog_posts
        (\x model -> model { catalog_posts = x })

getSetMediaRoot :: Lens Model MisoString
getSetMediaRoot =
    Lens
        media_root_
        (\x model -> model { media_root_ = x })

getSetSearchTerm :: Lens Model MisoString
getSetSearchTerm =
    Lens
        search_term
        (\s model -> model { search_term = s })

getSetAdmin :: Lens Model Bool
getSetAdmin =
    Lens
        admin
        (\s model -> model { admin = s })

getSetSitesAndBoards :: Lens Model [ Site ]
getSetSitesAndBoards =
    Lens
        all_sites_and_boards
        (\xs model -> model { all_sites_and_boards = xs })

getSetCurrentUri :: Lens Model URI
getSetCurrentUri =
    Lens
        current_uri
        (\x model -> model { current_uri = x })
