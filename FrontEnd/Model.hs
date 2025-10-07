module Common.FrontEnd.Model where

import Miso (URI)
import Miso.String (MisoString)
import Data.Time.Clock (UTCTime)
import Miso.Lens (Lens (..))

import Common.FrontEnd.Action (Action)
import qualified Common.Component.Thread.Types  as Thread
import Common.Network.CatalogPostType (CatalogPost)

data Model = Model
    { current_uri :: URI
    , media_root_ :: MisoString
    , current_time :: UTCTime
    , search_term :: MisoString
    , initial_action :: Action
    , thread_message :: Maybe Thread.Message
    , pg_api_root :: MisoString
    , client_fetch_count :: Int
    , catalog_posts :: [ CatalogPost ]
    } deriving Eq

getSetCatalogPosts :: Lens Model [ CatalogPost ]
getSetCatalogPosts =
    Lens
        catalog_posts
        (\x model -> model { catalog_posts = x })
