{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
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

$(loadEnv SearchPath)

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
    isMac = target == "apate"
    package = ".#nixosConfigurations." <> target <> ".config.system.build.toplevel"
    hostPackage = ".#" <> target
    sshTarget = mkSshTarget target
    runSshCwd cmd = ssh sshTarget ("cd code/dotfiles; " <> cmd)

  when (isLocal && buildRemotely) do
    output Ansi.Red "Cannot build remotely and deploy locally.\n"
    exitFailure

  when (not isLocal && not buildRemotely && isMac) do
    traverse_
      (uncurry output)
      [ (Ansi.Red, "Cannot locally build for ")
      , (Ansi.Cyan, "apate ")
      , (Ansi.Red, ".")
      ]
    exitFailure

  traverse_ (uncurry output)
    . catMaybes
    $ [ Just (Ansi.Magenta, "-> ")
      , Just (Ansi.Red, "deploy ")
      , Just . bool (Ansi.Green, "locally ") (Ansi.Red, "remotely ") $ buildRemotely
      , Just (Ansi.White, "for ")
      , Just (Ansi.Magenta, sshTarget)
      , Just (Ansi.White, " ")
      , bool Nothing (Just (Ansi.Cyan, "fast ")) isFast
      , bool Nothing (Just (Ansi.Cyan, "unattended ")) isUnattended
      , Just (Ansi.White, "\n\n")
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

  unless (isFast || isMac) do
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
          _ -> output Ansi.White "\n" $> False

  when shouldUpdate do
    traverse_
      (uncurry output)
      [ (Ansi.White, "[")
      , (Ansi.Magenta, sshTarget)
      , (Ansi.White, "] ")
      , (Ansi.Red, "deploy")
      , (Ansi.White, "ing ")
      , (Ansi.Green, ".#" <> target)
      , (Ansi.White, " ... \n")
      ]
    case (isLocal, buildRemotely, isMac) of
      (True, False, False) -> exe "sudo" "nixos-rebuild" "switch" "--quiet" "--quiet" "--flake" hostPackage
      (True, False, True) -> exe "sudo" "darwin-rebuild" "switch" "--flake" hostPackage
      (False, True, False) -> runSshCwd $ "sudo nixos-rebuild switch --quiet --quiet --flake " <> hostPackage
      (False, True, True) -> runSshCwd $ "sudo darwin-rebuild switch --flake " <> hostPackage
      _ ->
        -- the target cannot be macos, so this is always correct.
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

mkPackage :: ByteString -> ByteString
mkPackage =
  \case
    "apate" -> ".#darwinConfigurations.apate"
    target -> ".#nixosConfigurations." <> target <> ".config.system.build.toplevel"

output :: (ExecArg s) => Ansi.Color -> s -> IO ()
output color text = do
  Ansi.setSGR [Ansi.SetColor Ansi.Foreground Ansi.Vivid color]
  writeOutput text

mkSshTarget :: BS.ByteString -> BS.ByteString
mkSshTarget =
  \case
    "arche" -> "every@arche"
    sshTarget -> "evie@" <> sshTarget

getDotfilesPath :: ByteString -> FilePath
getDotfilesPath =
  \case
    "apate" -> "/Users/evie"
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
        ["##", "main...origin/main"] -> (Nothing, Nothing)
        ["##", br] -> (Nothing, Just br)
        ["##", "main...origin/main", "[ahead", nr] -> (Just $ BS.dropEnd 1 nr <> " ahead", Nothing)
        ["##", "main...origin/main", "[behind", nr] -> (Just $ BS.dropEnd 1 nr <> " behind", Nothing)
        ["##", br, "[ahead", nr] -> (Just $ BS.dropEnd 1 nr <> " ahead", Just br)
        ["##", br, "[behind", nr] -> (Just $ BS.dropEnd 1 nr <> " behind", Just br)
        xs -> error $ "Unexpected 'git status' header: " <> show xs
