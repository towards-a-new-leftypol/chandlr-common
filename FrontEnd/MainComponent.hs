{-# LANGUAGE DataKinds #-}

module Common.FrontEnd.MainComponent where

import Miso
    ( Component
    )

import Common.FrontEnd.Model (Model)
import Common.FrontEnd.Action (Action)

type MainComponent = Component "main" Model Action
