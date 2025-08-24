{ pkgs, ... }:
let
  my-lib = import ./../lib { inherit pkgs; };
  haskellLanguagePragmas = ''
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
  '';
in
{
  get-active-window = pkgs.writeShellApplication {
    name = "get-active-window";
    runtimeInputs = [ pkgs.socat pkgs.jq ];
    text = ''
      #!/usr/bin/env bash

      hyprctl activewindow -j | jq '.class' | awk '{print tolower($0)}'

      socat -u UNIX-CONNECT:"$XDG_RUNTIME_DIR"/hypr/"$HYPRLAND_INSTANCE_SIGNATURE"/.socket2.sock - |
        stdbuf -o0 awk -F '>>|,' -f ${./../scripts/active-window-parse.awk}
    '';
  };

  tv-toggle = pkgs.writeShellApplication {
    name = "tv-toggle";
    runtimeInputs = [ pkgs.jq ];
    text = ''
      #!/usr/bin/env bash

      num_monitors=$(hyprctl monitors -j | jq length)

      if [ "$num_monitors" -lt "4" ]; then
        hyprctl keyword monitor "HDMI-A-1,1920x1080@60,0x395,1"
      else
        hyprctl keyword monitor "HDMI-A-1,disabled"
      fi
    '';
  };

  tv-status = my-lib.nuShellScript {
    name = "tv-status";
    runtimeInputs = [ pkgs.jq ];
    text = ''
      #!/usr/bin/env nu

      # This is hard-coded for thelxinoe
      let num_monitors = hyprctl monitors -j | jq length
      let alt = if $num_monitors == "4" {
                  "on"
                } else {
                  "off"
                }

      { alt: $alt
      } | to json -r
    '';
  };

  waybar-khal = pkgs.writers.writePython3Bin "waybar-khal" { } ''
    # https://gist.github.com/bjesus/178a9bd3453470d74803945dbbf9ed40
    import subprocess
    import datetime
    import json
    from html import escape

    data = {}

    today = datetime.date.today().strftime("%A, %d %B %Y")

    next_week = (datetime.date.today() +
                 datetime.timedelta(days=7)).strftime("%d/%m/%Y")

    output = subprocess.check_output("khal list now "+next_week, shell=True)
    output = output.decode("utf-8")

    lines = output.split("\n")
    new_lines = []
    for line in lines:
        clean_line = escape(line).split(" ::")[0]
        if len(clean_line) and not clean_line[0] in ['0', '1', '2']:
            clean_line = "\n<b>"+clean_line+"</b>"
        new_lines.append(clean_line)
    output = "\n".join(new_lines).strip()

    if today in output:
        data['text'] = " " + output.split('\n')[1]
    else:
        data['text'] = ""

    data['tooltip'] = output

    print(json.dumps(data))
  '';

  calendar-notify = pkgs.writers.writeHaskellBin
    "calendar-notify"
    {
      libraries = with pkgs.haskellPackages; [ aeson bytestring chronos deepseq shh text torsor ];
    }
    ''
      ${haskellLanguagePragmas}

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

      loadFromBins ["${pkgs.khal}", "${pkgs.libnotify}", "${pkgs.coreutils}"]

      main :: IO ()
      main = do
        khal "list" "--notstarted" "--json" "title" "--json" "start" "--json" "calendar" "today" "today"
          |> readInput (pure . Aeson.decode @[CalendarEntry])
          >>= maybe (print "cannot parse khal output") (traverse_ go)
        where
          go :: CalendarEntry -> IO ()
          go entry = do
            now <-
              date "+%d/%m/%Y %T"
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
        exe "${pkgs.lib.getExe pkgs.libnotify}" (title <> " at " <> start)
    '';

  webcam-status = pkgs.writers.writeHaskellBin
    "webcam-status"
    {
      libraries = with pkgs.haskellPackages; [ aeson bytestring deepseq extra filepath shh ];
    }
    ''
      ${haskellLanguagePragmas}

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

      loadFromBins ["${pkgs.coreutils}"]

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
    '';

  email-status = pkgs.writers.writeHaskellBin
    "webcam-status"
    {
      libraries = with pkgs.haskellPackages; [ aeson bytestring deepseq table-layout shh ];
    }
    ''
      ${haskellLanguagePragmas}

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

      loadFromBins ["${pkgs.notmuch}"]

      main :: IO ()
      main = do
        Env.getArgs >>= \case
          [tag] -> run tag
          _ -> error "expected a single argument: notmuch tag"

      run :: String -> IO ()
      run tag = do
        let
          searchTerm = "tag:" <> tag
        count <- notmuch "count" searchTerm |> captureTrim
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
            , "tooltip" .= bool "" (formatTable . fmap formatEmail $ emails) (not $ null emails)
            , "class" .= (bool "unread" "none" $ count == "0")
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
          fmap replaceNewline
            . Table.tableString
            . Table.headerlessTableS
              [Table.def, Table.def, Table.def, Table.def]
              Table.unicodeRoundS
            . fmap Table.rowG

        replaceNewline :: Char -> Char
        replaceNewline = \case
          '\n' -> '\r'
          c -> c
    '';
}
