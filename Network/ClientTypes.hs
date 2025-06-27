{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Common.Network.ClientTypes where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Time.Clock (UTCTime)
import Miso.String (MisoString)
import Miso (Component)
import GHC.TypeLits (KnownSymbol)
import Data.Typeable (Typeable)

import qualified Common.Network.HttpTypes as Http
import qualified Common.FrontEnd.Action as A

type Action = (SomeInterface, ActionVerb)

data ActionVerb where
    Connect :: (FromJSON a, Typeable a) => Http.HttpActionResult a -> ActionVerb
    FetchLatest :: UTCTime -> ActionVerb
    GetThread :: A.GetThreadArgs -> ActionVerb
    Search :: MisoString -> ActionVerb

data Model = Uninitialized | Model
  { pgApiRoot :: MisoString
  , fetchCount :: Int
  } deriving Eq

data SomeInterface = forall a. (FromJSON a, Typeable a) =>
    SomeInterface (Interface a)

data Interface a = forall name model action.
  (KnownSymbol name) =>
  Interface
    -- { returnResult :: forall b. (FromJSON b) => Http.HttpResult b -> action
    { returnResult :: Http.HttpResult a -> action
    , notifyComponent :: Component name model action
    }

data FetchCatalogArgs = FetchCatalogArgs
  { max_time :: UTCTime
  , max_row_read :: Int
  } deriving (Generic, ToJSON)

data SearchPostsArgs = SearchPostsArgs
  { search_text :: MisoString
  , max_rows :: Int
  } deriving (Generic, ToJSON)
