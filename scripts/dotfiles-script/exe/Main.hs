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

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Bool (bool)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text.Lazy.Encoding as TL
import Shh
import qualified System.Environment as Env

$(loadEnv SearchPath)

main :: IO ()
main = do
  Env.getArgs >>= \case
    ["status"] -> hyprshade "current" |> readInputLines status
    ["toggle"] -> hyprshade "toggle" "blue-light-filter"
    [msg] -> error $ "unexpected command: " <> msg
    _ -> error "expected a single argument"

status :: [ByteString] -> IO ()
status bs =
  writeOutput
    . Aeson.encode
    . Aeson.object
    $ [ "text" .= bool "\61829" "\61830" hasAnyFilter
      , "alt" .= ""
      , "tooltip" .= (TL.decodeUtf8 . BSL.unlines $ bs)
      , "class" .= bool "disabled" "enabled" hasAnyFilter
      ]
 where
  hasAnyFilter :: Bool
  hasAnyFilter = bs /= []
