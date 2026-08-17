module Common.Component.Catalog where

import Data.Sequence (empty)
import Miso
    ( Component
    , component
    , vfrag
    , View
    , mountWithProps
    , MisoString
    )
import Data.Time.Clock (UTCTime)
import qualified Common.Component.CatalogGrid as Grid
import qualified Common.Component.CatalogGrid.GridTypes as Grid

import Common.Network.CatalogPostType (CatalogPost)
import Common.FrontEnd.Types (Pages (..))

type CatalogPages = Pages (UTCTime, Maybe UTCTime) [] CatalogPost

newtype Model = Model
    { pages :: CatalogPages
    } deriving Eq

initialModel :: Model
initialModel = Model { pages = Pages empty }

newtype Props = Props { mediaRoot :: MisoString }
    deriving Eq

type Action = ()

app :: Eq context => Component context Props Model Action
app = component initialModel (const $ return ()) view

view :: Eq context => context -> Props -> Model -> View context Action
view _ props model = vfrag [ mountWithProps (mkGridProps model props) Grid.app ]

mkGridProps :: Model -> Props -> Grid.Props (Pages (UTCTime, Maybe UTCTime) [])
mkGridProps m p = Grid.Props (pages m) (mediaRoot p)
