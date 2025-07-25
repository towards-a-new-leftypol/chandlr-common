module Common.FrontEnd.Action where

import Data.Time.Clock (UTCTime)
import Miso.String (MisoString)
import Miso (URI, ComponentId)
import Data.Aeson (Result)

import Common.Network.CatalogPostType (CatalogPost)
import qualified Common.Network.CatalogPostType as CatalogPost
import qualified Common.Component.Search.SearchTypes as Search
import qualified Common.Network.ClientTypes as Client
import qualified Common.Component.CatalogGrid.GridTypes as Grid

data Action
    = GetThread Client.GetThreadArgs
    | GridMessage (Result Grid.OutMessage)
    | ClientResponse (Result Client.MessageOut)
    | GoToTime UTCTime
    | ChangeURI URI
    | SearchResults MisoString
    | NotifySearch Search.Message
    | ClientMounted
    | ClientUnmounted
    | ThreadViewMounted
    | NoAction
    | HaveOwnComponentId ComponentId
    | Initialize
    deriving Eq


mkGetThread :: CatalogPost -> Client.GetThreadArgs
mkGetThread post = Client.GetThreadArgs
    { Client.website = CatalogPost.site_name post
    , Client.board_pathpart = CatalogPost.pathpart post
    , Client.board_thread_id = CatalogPost.board_thread_id post
    }
