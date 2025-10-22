{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Common.Component.Thread.Model where

import Data.Time.Clock (UTCTime)
import Miso.String (MisoString)
import Miso.Lens (Lens (..))
import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)

import Common.Network.SiteType (Site, emptySite)
import Common.Network.PostType (Post)
import Common.Parsing.PostPartType (PostPart)
import Common.Utils (fakeTime)

type PostWithBody = (Post, [ PostPart ])

data Model
  = Model
    { site :: Site
    , media_root :: MisoString
    , post_bodies :: [ PostWithBody ]
    , current_time :: UTCTime
    , admin :: Bool
    } deriving (Eq, Generic, FromJSON, ToJSON)


emptyModel :: Model
emptyModel = Model
    { site = emptySite
    , media_root = ""
    , post_bodies = []
    , current_time = fakeTime
    , admin = False
    }


getSetAdmin :: Lens Model Bool
getSetAdmin =
    Lens
        admin
        (\s model -> model { admin = s })


getSetMediaRoot :: Lens Model MisoString
getSetMediaRoot =
    Lens
        media_root
        (\x model -> model { media_root = x })
