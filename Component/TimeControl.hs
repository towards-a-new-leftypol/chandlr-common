{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Common.Component.TimeControl where

import Control.Monad.IO.Class (liftIO)
import Miso
    ( View
    , div_
    , class_
    , input_
    , step_
    , min_
    , max_
    , type_
    , value_
    , consoleLog
    , Effect
    , onInput
    , onChange
    , Component
    , defaultEvents
    , modify
    , io_
    , notify
    )

import GHC.TypeLits (KnownSymbol)
import qualified Miso as M
import Miso.String (MisoString, toMisoString, fromMisoString)
import Data.Time.Clock
  ( UTCTime (..)
  , getCurrentTime
  , diffUTCTime
  , addUTCTime
  , secondsToDiffTime
  )
import Data.Time.Calendar (fromGregorian)

data Time
  = Now
  | SlideInput MisoString
  | SlideChange MisoString
  deriving Show

data Interface a = Interface
    { passAction :: Time -> a
    , goTo :: UTCTime -> a
    }

data Model = Model
  { whereAt :: Integer
  } deriving Eq

type TimeControl = Component "time-controls" Model Time

view :: Model -> View Time
view m =
    div_
        [ class_ "time-control"
        ]
        [ input_
            [ class_ "time-slider"
            , type_ "range"
            , min_ "-500"
            , max_ "0"
            , step_ "1"
            , value_ $ toMisoString $ show (whereAt m)
            , onInput SlideInput
            , onChange SlideChange
            ]
        ]

type TimeChangeCallback name = forall model action.
    ( Component name model action
    , UTCTime -> action
    )

update
    :: (KnownSymbol name)
    => TimeChangeCallback name
    -> Time
    -> Effect Model Time
update _ (SlideInput nstr) = io_ $
  consoleLog $ "Input: " <> nstr

update (callback_component, callback_action) (SlideChange nstr) = do
  modify (\model ->  model { whereAt = n })

  io_ $ do
      consoleLog $ "Change: " <> nstr

      now <- liftIO getCurrentTime

      let newTime = interpolateTimeHours n now

      notify callback_component $ callback_action newTime

  where
    n :: Integer
    n = read $ fromMisoString nstr


earliest :: UTCTime
--earliest = UTCTime (fromGregorian 2020 12 20) (secondsToDiffTime 82643)
earliest = UTCTime (fromGregorian 2020 12 20) (secondsToDiffTime 82644)


-- Linear interpolation function using hours
interpolateTimeHours :: Integer -> UTCTime -> UTCTime
interpolateTimeHours n currentTime
  | n == 0 = currentTime
  | n == -500 = earliest
  | otherwise = addUTCTime (fromIntegral hoursToAdjust * secondsInHour) currentTime

  where
    -- Calculate the total number of hours between the current time and the target date
    totalHours = diffUTCTime currentTime earliest / secondsInHour

    -- Calculate the number of hours to adjust based on linear interpolation
    hoursToAdjust :: Integer
    hoursToAdjust = round $ totalHours * (fromIntegral n / 500.0)

    -- One hour in seconds
    secondsInHour = 3600

app
    :: (KnownSymbol name)
    => Integer
    -> TimeChangeCallback name
    -> TimeControl
app t callback_info = M.Component
    { M.model = Model t
    , M.update = update callback_info
    , M.view = view
    , M.subs = []
    , M.events = defaultEvents
    , M.styles = []
    , M.initialAction = Nothing
    , M.mountPoint = Nothing
    , M.logLevel = M.DebugAll
    }
