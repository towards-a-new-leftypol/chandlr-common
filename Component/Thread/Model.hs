{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Common.Component.Thread.Model where

import Data.Time.Clock (UTCTime)
import Miso.String (MisoString)
import GHC.Generics
import Miso.JSON (FromJSON, ToJSON)

import Common.Network.SiteType (Site, emptySite)
import Common.Network.PostType (Post)
import Common.Parsing.PostPartType (PostPart)
import Common.Utils (fakeTime)

type PostWithBody = (Post, [ PostPart ])

data Model
  = Model
    { site :: Site
    , post_bodies :: [ PostWithBody ]
    , current_time :: UTCTime
    } deriving (Eq, Generic, FromJSON, ToJSON)

data Props = Props
  { admin :: Bool
  , media_root :: MisoString
  } deriving Eq

emptyModel :: Model
emptyModel = Model
    { site = emptySite
    , post_bodies = []
    , current_time = fakeTime
    }
