module Common.FrontEnd.MainComponent where

import Miso
    ( Component
    , Effect
    )

import Common.FrontEnd.Model (Model)
import Common.FrontEnd.Action (Action)

type MainComponent = Component Effect Model Action ()
