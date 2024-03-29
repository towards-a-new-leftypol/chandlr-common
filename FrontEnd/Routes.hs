{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Common.FrontEnd.Routes where

import Data.Text (Text)
import Data.Int (Int64)
import Miso (View)
import Servant.API

import Common.FrontEnd.Action

type Route
    =    R_Latest
    :<|> R_Thread
    :<|> R_SearchResults

type R_Latest = View Action

-- Show selected thread
--  - <website>/<board>/<thread_id>
type R_Thread
    =  Capture "website" Text
    :> Capture "board"   Text
    :> Capture "board_thread_id" BoardThreadId
    :> View Action

type R_SearchResults
    = "search"
    :> QueryParam "search" Text
    :> View Action

type BoardThreadId = Int64
