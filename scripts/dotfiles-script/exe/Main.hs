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
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List.Extra (nubOrdOn)
import GHC.Generics (Generic)
import Shh
import System.FilePath ((</>))

$(loadEnv SearchPath)

deviceRoot, runtimeEnabled, runtimeStatus, cameraIcon :: String
deviceRoot = "/sys/class/video4linux"
runtimeEnabled = "device/power/runtime_enabled"
runtimeStatus = "device/power/runtime_status"
cameraIcon = "\61488"

main :: IO ()
main =
  ls deviceRoot
    |> readInputLinesP (traverse getDeviceInfo)
    >>= printJsonSummary . nubOrdOn name

data DeviceInfo = DeviceInfo
  { name :: String
  , enabled :: Bool
  , active :: Bool
  }
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (Aeson.ToJSON, NFData)

getDeviceInfo :: ByteString -> Proc DeviceInfo
getDeviceInfo videoDevice = do
  let
    root = deviceRoot </> BS.unpack videoDevice
  name <- cat (root </> "name") |> readInput (pure . BS.unpack . trim)
  enabled <- cat (root </> runtimeEnabled) |> readInput (pure . (== "enabled") . trim)
  active <- cat (root </> runtimeStatus) |> readInput (pure . (== "active") . trim)
  pure DeviceInfo {..}

printJsonSummary :: [DeviceInfo] -> IO ()
printJsonSummary devices =
  writeOutput
    . Aeson.encode
    . Aeson.object
    $ [ "text" .= cameraIcon
      , "alt" .= ""
      , "tooltip" .= (unlines . fmap formatDevice $ devices)
      , "class"
          .= if any active devices
            then "active"
            else bool "none" "enabled" . any enabled $ devices
      ]
 where
  formatDevice :: DeviceInfo -> String
  formatDevice DeviceInfo {..} =
    unwords
      [ cameraIcon
      , name
      , bool "disabled" "enabled" enabled
      , bool "active" "suspended" active
      ]
