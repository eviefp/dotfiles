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
import Control.Monad (when)
import qualified Control.Monad.State as S
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Foldable (foldlM)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Shh
import qualified Torsor

$(loadEnv SearchPath)

main :: IO ()
main = do
  today <- getToday
  let
    nextWeek = Chronos.timeToDatetime . Torsor.add (Torsor.scale 7 Chronos.day) . Chronos.datetimeToTime $ today

  khal
    "list"
    "--format"
    "[{calendar}] {cancelled}{start-end-time-style} {title}{repeat-symbol}"
    "now"
    (T.unpack . Chronos.encode_Dmy (Just '/') . Chronos.datetimeDate $ nextWeek)
    |> readInputLines printJsonSummary

printJsonSummary :: [BS.ByteString] -> IO ()
printJsonSummary input =
  writeOutput
    . Aeson.encode
    . Aeson.object
    $ [ "text" .= text
      , "alt" .= ""
      , "tooltip" .= tooltip
      , "class"
          .= if text == "\61747"
            then "today"
            else "none"
      ]
 where
  text :: Text
  text =
    case TL.words tooltip of
      ("Today" : _) ->
        case TL.lines tooltip of
          (_ : event : _) ->
            "\61555 " <> event
          _ -> "\61747"
      _ -> "\61747"

  tooltip :: Text
  tooltip =
    TL.strip
      . flip S.evalState []
      . foldlM parseLine TL.empty
      . fmap TL.decodeUtf8
      $ input

  parseLine :: Text -> Text -> S.State [Text] Text
  parseLine acc bs =
    case TL.head bs of
      '[' ->
        S.gets (bs `elem`) >>= \case
          True -> pure acc
          False -> do
            when (TL.elem '↔' bs) $ S.modify (<> [bs])
            pure $ acc <> replaceCalendarNames bs <> "\n"
      _ -> pure $ acc <> "\n<b>" <> bs <> "</b>\n"

  replaceCalendarNames :: Text -> Text
  replaceCalendarNames =
    TL.replace "s810p67l2bi1168j8luka5nic0@group.calendar.google.com" "Every"
      . TL.replace "alexaeviest@gmail.com" "gmail"

getToday :: IO Chronos.Datetime
getToday =
  date "+%d/%m/%Y %R"
    |> readInput
      ( maybe (error "cannot determine system time") pure
          . Chronos.decode_DmyHMS_opt_S_lenient
          . T.decodeUtf8Lenient
          . BS.toStrict
          . trim
      )
