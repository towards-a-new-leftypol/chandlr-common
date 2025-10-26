module Common.FrontEnd.Action where

import Data.Time.Clock (UTCTime)
import Miso.String (MisoString)
import Miso (URI)

import Common.Network.CatalogPostType (CatalogPost)
import qualified Common.Network.CatalogPostType as CatalogPost
import qualified Common.Component.Search.SearchTypes as Search
import qualified Common.Network.ClientTypes as Client
import qualified Common.Component.CatalogGrid.GridTypes as Grid
import Common.FrontEnd.Types (InitCtxRef)

data Action
    = GetThread Client.GetThreadArgs
    | GridMessage Grid.OutMessage
    | ClientResponse Client.ReturnTopicName Client.MessageOut
    | OnErrorMessage MisoString
    | GoToTime UTCTime
    | ChangeURI URI
    | SearchResults Search.MessageOut
    | NotifySearch Search.MessageIn
    | ClientMounted
    | ClientUnmounted
    | ThreadViewMounted
    | NoAction
    | Initialize InitCtxRef
    deriving Eq


mkGetThread :: CatalogPost -> Client.GetThreadArgs
mkGetThread post = Client.GetThreadArgs
    { Client.website = CatalogPost.site_name post
    , Client.board_pathpart = CatalogPost.pathpart post
    , Client.board_thread_id = CatalogPost.board_thread_id post
    }
