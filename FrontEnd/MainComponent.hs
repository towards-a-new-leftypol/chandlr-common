module Common.FrontEnd.MainComponent where

import Miso
    ( App
    )

import Common.FrontEnd.Model (Model)
import Common.FrontEnd.Action (Action)

type MainComponent = App Model Action
