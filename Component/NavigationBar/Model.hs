module Common.Component.NavigationBar.Model where

newtype Model = Model
    { menuState :: MenuState
    } deriving Eq

data MenuState
    = ChooseSites
    | ChooseBoards
    | Closed
    deriving Eq
