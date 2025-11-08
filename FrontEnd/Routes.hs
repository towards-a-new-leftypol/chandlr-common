{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Common.FrontEnd.Routes where

import Data.Text (Text)
import Servant.API

type Route a
    =    R_Latest a
    :<|> R_Thread a
    :<|> R_SearchResults a

type R_Latest a = a

-- Show selected thread
--  - <website>/<board>/<thread_id>
type R_Thread a
    =  Capture "website" Text
    :> Capture "board"   Text
    :> Capture "board_thread_id" BoardThreadId
    :> a

type R_SearchResults a
    = "search"
    :> QueryParam "q" String
    :> a

type BoardThreadId = Integer
