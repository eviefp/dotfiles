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

import Control.DeepSeq (NFData)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Bool (bool)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import GHC.Generics (Generic)
import Shh
import qualified System.Environment as Env
import qualified Text.Layout.Table as Table

$(loadEnv SearchPath)

main :: IO ()
main = do
  Env.getArgs >>= \case
    [tag] -> run tag
    _ -> error "expected a single argument: notmuch tag"

run :: String -> IO ()
run tag = do
  let
    searchTerm = "tag:" <> tag
  count <- notmuch "count" searchTerm |> readInput pure
  emails <-
    notmuch "search" "--limit=20" "--format=json" searchTerm
      |> readInput (Aeson.throwDecode @[EmailSummary])
  printJsonSummary count emails

data EmailSummary = EmailSummary
  { date_relative :: String
  , authors :: String
  , subject :: String
  , tags :: [String]
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData, Aeson.ToJSON, Aeson.FromJSON)

printJsonSummary :: ByteString -> [EmailSummary] -> IO ()
printJsonSummary count emails =
  writeOutput
    . Aeson.encode
    . Aeson.object
    $ [ "text" .= LBS.unpack count
      , "alt" .= ""
      , "tooltip" .= formatTable (fmap formatEmail $ emails)
      , "class" .= (bool "none" "unread" $ count == "0")
      ]
 where
  formatEmail :: EmailSummary -> [String]
  formatEmail EmailSummary {..} =
    [ date_relative
    , authors
    , subject
    , unwords tags
    ]

  formatTable :: [[String]] -> String
  formatTable =
    Table.tableString
      . Table.headerlessTableS
        [Table.def, Table.numCol]
        Table.unicodeRoundS
      . fmap Table.rowG
