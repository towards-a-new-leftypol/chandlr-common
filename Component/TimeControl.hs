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
    , io
    , io_
    , Topic
    , topic
    , publish
    )

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
  | Publish Message
  deriving Show

data Model = Model
  { whereAt :: Integer
  } deriving Eq

type TimeControl = Component Model Time

type Message = UTCTime

timeControlTopic :: Topic Message
timeControlTopic = topic "time-control"

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

update
    :: Time
    -> Effect Model Time
update (SlideInput nstr) = io_ $
  consoleLog $ "Input: " <> nstr

update (SlideChange nstr) = do
  modify (\model ->  model { whereAt = n })

  io $ do
    consoleLog $ "Change: " <> nstr

    now <- liftIO getCurrentTime

    let newTime = interpolateTimeHours n now

    return $ Publish newTime

  where
    n :: Integer
    n = read $ fromMisoString nstr

update (Publish t) = publish timeControlTopic t

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
    :: Integer
    -> TimeControl
app t = M.Component
    { M.model = Model t
    , M.update = update
    , M.view = view
    , M.subs = []
    , M.events = defaultEvents
    , M.styles = []
    , M.initialAction = Nothing
    , M.mountPoint = Nothing
    , M.logLevel = M.DebugAll
    , M.scripts = []
    , M.mailbox = const Nothing
    }
