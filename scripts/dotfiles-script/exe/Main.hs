{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import qualified Chronos
import Control.DeepSeq (NFData)
import qualified Data.Aeson as Aeson
import Data.Bool (bool)
import qualified Data.ByteString.Lazy as BS
import Data.Foldable (traverse_)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics (Generic)
import Shh
import qualified Torsor

$(loadEnv SearchPath)

main :: IO ()
main = do
  khal "list" "--notstarted" "--json" "title" "--json" "start" "--json" "calendar" "today" "today"
    |> readInput (pure . Aeson.decode @[CalendarEntry])
    >>= maybe (print "cannot parse khal output") (traverse_ go)
 where
  go :: CalendarEntry -> IO ()
  go entry = do
    now <-
      date "+%d/%m/%Y %R"
        |> readInput
          ( maybe (error "cannot determine system time") pure
              . Chronos.decode_DmyHMS_opt_S_lenient
              . T.decodeUtf8Lenient
              . BS.toStrict
              . trim
          )
    case Chronos.decode_DmyHMS_opt_S_lenient . T.pack . start $ entry of
      Nothing -> pure ()
      Just when -> bool mempty (notify entry) . shouldNotify when $ now

shouldNotify :: Chronos.Datetime -> Chronos.Datetime -> Bool
shouldNotify event now =
  let
    fifteenMinutes = Torsor.scale 15 Chronos.minute
    eventTime = Chronos.datetimeToTime event
    nowTime = Chronos.datetimeToTime now
    diff = Torsor.difference eventTime (Torsor.add fifteenMinutes nowTime)
  in
    diff < Chronos.minute && diff >= mempty

data CalendarEntry = CalendarEntry
  { start :: String
  , title :: String
  , calendar :: String
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData, Aeson.ToJSON, Aeson.FromJSON)

notify :: CalendarEntry -> IO ()
notify CalendarEntry {..} =
  exe "notify-send" (title <> " at " <> start)
