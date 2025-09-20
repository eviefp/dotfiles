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
    {-# LANGUAGE MultiWayIf #-}
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

      loadFromBins ["${pkgs.khal}", "${pkgs.libnotify}", "${pkgs.coreutils}", "${pkgs.mpv}"]

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
      notify CalendarEntry {..} = do
        exe "${pkgs.lib.getExe pkgs.libnotify}" "-a" "calendar-notify" (title <> " at " <> start)
        mpv " --terminal=no" "~/.config/swaync/sounds/calendar.mp3"
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

  calendar-status = pkgs.writers.writeHaskellBin
    "calendar-status"
    {
      libraries = with pkgs.haskellPackages; [ bytestring chronos mtl shh text torsor ];
    }
    ''
      ${haskellLanguagePragmas}

      module Main where

      import qualified Chronos
      import Control.Monad (when)
      import qualified Control.Monad.State as S
      import Data.Aeson ((.=))
      import qualified Data.Aeson as Aeson
      import Data.Bool (bool)
      import qualified Data.ByteString.Lazy.Char8 as BS
      import Data.Foldable (foldlM)
      import qualified Data.Text as T
      import qualified Data.Text.Encoding as T
      import Data.Text.Lazy (Text)
      import qualified Data.Text.Lazy as TL
      import qualified Data.Text.Lazy.Encoding as TL
      import Shh
      import qualified Torsor

      loadFromBins ["${pkgs.coreutils}", "${pkgs.khal}"]

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
            ("Today," : _) -> S.evalState (go . drop 1 . TL.lines $ tooltip) "\61747"
            _ -> "\61747"

        go :: [Text] -> S.State Text Text
        go = \case -- ⟳
          (x:xs) ->
            if | (fst <$> TL.uncons x) /= Just '[' ->  S.get
               | '↔' `TL.elem` x  -> S.modify (\st -> ("\61555 " <>) . skipCalendar . bool st x . TL.elem '↔' $ st) *> go xs
               | '⟳' `TL.elem` x  -> S.put ("\61555 " <> skipCalendar x) *> go xs
               | otherwise        -> pure ("\61555 " <> skipCalendar x)
          [] -> S.get

        skipCalendar :: Text -> Text
        skipCalendar e = case TL.words e of
                           [] -> e
                           [single] -> single
                           (_cal:rest) -> TL.unwords rest

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
            _ -> pure $ acc <> "\n" <> bs <> "\n"

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
    '';

  deploy = pkgs.writers.writeHaskellBin
    "deploy"
    {
      libraries = with pkgs.haskellPackages; [ ansi-terminal bytestring deepseq optparse-applicative shh ];
    }
    ''
      ${haskellLanguagePragmas}

      module Main where

      import Control.Applicative ((<**>))
      import Control.DeepSeq (NFData)
      import Control.Monad (unless, when)
      import Data.Bool (bool)
      import Data.ByteString.Lazy.Char8 (ByteString)
      import qualified Data.ByteString.Lazy.Char8 as BS
      import Data.Foldable (traverse_)
      import Data.Functor (($>))
      import Data.Maybe (catMaybes)
      import GHC.Generics (Generic)
      import qualified GHC.IO.StdHandles as H
      import qualified Options.Applicative as Opt
      import Shh
      import qualified System.Console.ANSI as Ansi
      import System.Exit (exitFailure)
      import qualified System.IO as H

      loadFromBins ["${pkgs.coreutils}", "${pkgs.git}", "${pkgs.hostname}", "${pkgs.nvd}", "${pkgs.openssh}"]

      data Deploy = Deploy
        { fast :: Bool
        , unattended :: Bool
        , remote :: Bool
        , target :: ByteString
        , localhost :: ByteString
        }
        deriving stock (Generic)

      instance NFData Deploy

      parseDeploy :: ByteString -> Opt.ParserInfo Deploy
      parseDeploy localhost =
        Opt.info
          (parser <**> Opt.helper)
          ( Opt.fullDesc
              <> Opt.progDesc "Deploy nix configurations remotely"
              <> Opt.header "depoy nixos configs"
          )
        where
          parser :: Opt.Parser Deploy
          parser =
            Deploy
              <$> Opt.switch
                (Opt.long "fast" <> Opt.short 'f' <> Opt.help "Skip evaluating twice but also skip diffs")
              <*> Opt.switch
                (Opt.long "unattended" <> Opt.short 'u' <> Opt.help "Disable asking for confirmation")
              <*> Opt.switch
                (Opt.long "remote" <> Opt.short 'r' <> Opt.help "Run everything on remote. Makes target required")
              <*> Opt.strArgument
                (Opt.value localhost <> Opt.metavar "target" <> Opt.help "Host to deploy to")
              <*> pure localhost

      -- TODO: investigate using this, probably if/when switching to stand-alone home-manager
      -- nvd "diff" "~/.local/state/nix/profiles/home-manager" path
      main :: IO ()
      main = do
        localhost <- hostname |> captureTrim
        (Deploy isFast isUnattended buildRemotely target _) <- Opt.execParser (parseDeploy localhost)
        let
          isLocal = target == localhost
          package = ".#nixosConfigurations." <> target <> ".config.system.build.toplevel"
          hostPackage = ".#" <> target
          sshTarget = mkSshTarget target
          runSshCwd cmd = ssh sshTarget ("cd code/dotfiles; " <> cmd)

        when (isLocal && buildRemotely) do
          output Ansi.Red "Cannot build remotely and deploy locally.\n"
          exitFailure

        traverse_ (uncurry output)
          . catMaybes
          $ [ Just (Ansi.Magenta, "-> ")
            , Just (Ansi.Red, "deploy ")
            , Just . bool (Ansi.Green, "locally ") (Ansi.Red, "remotely ") $ buildRemotely
            , Just (Ansi.White, "for ")
            , Just (Ansi.Magenta, sshTarget)
            , bool Nothing (Just (Ansi.Cyan, "fast ")) isFast
            , bool Nothing (Just (Ansi.Cyan, "unattended ")) isUnattended
            , Just (Ansi.White, "\n")
            ]

        cd (getDotfilesPath localhost)

        -- Print detailed local git status. This is relevant in all cases:
        --   - build local, deploying local: dirty local state
        --   - build local, deploying remote: same
        --   - remote local: will deploy whatever origin/main is!
        git "status" "--porcelain" "-b"
          |> fmap parseGitStatus captureLines
          >>= gitStatus localhost

        -- 'ssh git pull' for remote builds
        when buildRemotely do
          traverse_
            (uncurry output)
            [ (Ansi.White, "[")
            , (Ansi.Magenta, sshTarget)
            , (Ansi.White, "] git pull\n")
            ]
          runSshCwd "git pull" &> devNull
          runSshCwd "git status --porcelain -b"
            |> fmap parseGitStatus captureLines
            >>= gitStatus target

        unless isFast do
          traverse_
            (uncurry output)
            [ (Ansi.White, "[")
            , (Ansi.Magenta, sshTarget)
            , (Ansi.White, "] nix build ")
            , (Ansi.Green, package)
            , (Ansi.White, "\n")
            ]
          path <- case buildRemotely of
            False -> do
              exe "nix" "build" "--quiet" "--quiet" package &> devNull
              readlink "./result" |> captureTrim
            True -> do
              runSshCwd ("nix build --quiet --quiet " <> package) &> devNull &!> devNull
              runSshCwd "readlink ./result" |> captureTrim

          case (isLocal, buildRemotely) of
            (False, False) -> do
              traverse_
                (uncurry output)
                [ (Ansi.White, "[")
                , (Ansi.Magenta, sshTarget)
                , (Ansi.White, "] nix copy ")
                , (Ansi.Green, package)
                , (Ansi.White, "\n")
                ]
              exe "nix" "copy" "-L" package "--no-check-sigs" "--to" ("ssh-ng://" <> sshTarget) &> devNull
              ssh sshTarget "nvd" "diff" "/run/current-system" path
            (True, False) ->
              nvd "diff" "/run/current-system" path
            (_, True) ->
              runSshCwd $ "nvd diff /run/current-system " <> path

        shouldUpdate <-
          if isUnattended
            then pure True
            else do
              traverse_
                (uncurry output)
                [ (Ansi.White, "\n")
                , (Ansi.White, "[")
                , (Ansi.Magenta, sshTarget)
                , (Ansi.White, "] start ")
                , (Ansi.Red, "deploy ")
                , (Ansi.White, "(y/n): ")
                ]
              H.hSetBuffering H.stdin H.NoBuffering
              H.getChar >>= \case
                'y' -> output Ansi.White "\n" $> True
                _ -> output Ansi.White "\n" $>  False

        when shouldUpdate do
          traverse_
            (uncurry output)
            [ (Ansi.White, "[")
            , (Ansi.Magenta, sshTarget)
            , (Ansi.White, "] ")
            , (Ansi.Red, "deploy")
            , (Ansi.White, "ing ")
            , (Ansi.Green, package)
            , (Ansi.White, " ... \n")
            ]
          case (isLocal, buildRemotely) of
            (True, False) -> exe "sudo" "nixos-rebuild" "switch" "--quiet" "--quiet" "--flake" hostPackage
            (False, True) -> runSshCwd $ "sudo nixos-rebuild switch --quiet --quiet --flake " <> hostPackage
            _ ->
              exe
                "nixos-rebuild"
                "--quiet"
                "--quiet"
                "--flake"
                hostPackage
                "--target-host"
                sshTarget
                "--use-remote-sudo"
                "switch"

      output :: (ExecArg s) => Ansi.Color -> s -> IO ()
      output color text = do
        Ansi.setSGR [Ansi.SetColor Ansi.Foreground Ansi.Vivid color]
        writeOutput text

      mkSshTarget :: BS.ByteString -> BS.ByteString
      mkSshTarget =
        \case
          "apate" -> error "Apate is not yet supported"
          "arche" -> "every@arche"
          sshTarget -> "evie@" <> sshTarget

      getDotfilesPath :: ByteString -> FilePath
      getDotfilesPath =
        \case
          "apate" -> error "Apate is not yet supported"
          "arche" -> "/home/every/code/dotfiles"
          _ -> "/home/evie/code/dotfiles"

      gitStatus :: ByteString -> GitStatus -> IO ()
      gitStatus remote =
        \case
          Clean ->
            traverse_
              (uncurry output)
              [ (Ansi.White, "[")
              , (Ansi.Magenta, remote)
              , (Ansi.White, ":")
              , (Ansi.Cyan, BS.pack $ getDotfilesPath remote)
              , (Ansi.White, "] git status ")
              , (Ansi.Green, "clean\n")
              ]
          Dirty aheadOrBehind branch xs ->
            traverse_
              (uncurry output)
              [ (Ansi.White, "[")
              , (Ansi.Magenta, remote)
              , (Ansi.White, ":")
              , (Ansi.Cyan, BS.pack $ getDotfilesPath remote)
              , (Ansi.White, "] git status ")
              , (Ansi.Red, "dirty ")
              , maybe (Ansi.White, "") ((Ansi.Cyan,) . (<> " ")) branch
              , maybe (Ansi.White, "") (Ansi.Yellow,) aheadOrBehind
              , bool (Ansi.White, "\n") (Ansi.White, "") . null $ xs
              , (Ansi.Yellow, trim . BS.unlines . fmap trim $ xs)
              , (Ansi.White, "\n")
              ]

      data GitStatus = Clean | Dirty (Maybe ByteString) (Maybe ByteString) [ByteString]

      parseGitStatus :: [ByteString] -> GitStatus
      parseGitStatus =
        \case
          [] -> error "Unexpected empty 'git status' output."
          [branchStatus] ->
            case parseHeader branchStatus of
              (Nothing, Nothing) -> Clean
              (aheadOrBehind, branch) -> Dirty aheadOrBehind branch []
          (branchStatus : rest) ->
            let
              (aheadOrBehind, branch) = parseHeader branchStatus
            in
              Dirty aheadOrBehind branch rest
        where
          parseHeader :: ByteString -> (Maybe ByteString, Maybe ByteString)
          parseHeader branchStatus =
            case BS.words branchStatus of
              ("##" : "main...origin/main" : []) -> (Nothing, Nothing)
              ("##" : br : []) -> (Nothing, Just br)
              ("##" : "main...origin/main" : "[ahead" : nr : []) -> (Just $ BS.dropEnd 1 nr <> " ahead", Nothing)
              ("##" : "main...origin/main" : "[behind" : nr : []) -> (Just $ BS.dropEnd 1 nr <> " behind", Nothing)
              ("##" : br : "[ahead" : nr : []) -> (Just $ BS.dropEnd 1 nr <> " ahead", Just br)
              ("##" : br : "[behind" : nr : []) -> (Just $ BS.dropEnd 1 nr <> " behind", Just br)
              xs -> error $ "Unexpected 'git status' header: " <> show xs
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

  hyprshade-ctl = pkgs.writers.writeHaskellBin
    "hyprshade-ctl"
    {
      libraries = with pkgs.haskellPackages; [ aeson bytestring text shh ];
    }
    ''
      ${haskellLanguagePragmas}

      module Main where

      import Data.Aeson ((.=))
      import qualified Data.Aeson as Aeson
      import Data.Bool (bool)
      import Data.ByteString.Lazy (ByteString)
      import qualified Data.ByteString.Lazy.Char8 as BSL
      import qualified Data.Text.Lazy as TL
      import qualified Data.Text.Lazy.Encoding as TL
      import Shh
      import qualified System.Environment as Env

      loadFromBins ["${pkgs.hyprshade}"]

      main :: IO ()
      main = do
        Env.getArgs >>= \case
          ["status"] -> hyprshade "current" |> readInputLines (status)
          ["toggle"] -> hyprshade "toggle" "blue-light-filter"
          [msg] -> error $ "unexpected command: " <> msg
          _ -> error "expected a single argument"

      status :: [ByteString] -> IO ()
      status bs =
        writeOutput
          . Aeson.encode
          . Aeson.object
          $ [ "text" .= ""
            , "alt" .= bool "off" "on" hasAnyFilter
            , "tooltip" .= (TL.strip . TL.decodeUtf8 . BSL.unlines $ bs)
            , "class" .= bool "disabled" "enabled" hasAnyFilter
            ]
          where
            hasAnyFilter :: Bool
            hasAnyFilter = bs /= []
    '';
}
