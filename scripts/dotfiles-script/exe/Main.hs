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

import Control.Applicative ((<**>))
import Control.DeepSeq (NFData)
import Control.Monad (unless, when)
import Data.Bool (bool)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import GHC.Generics (Generic)
import qualified GHC.IO.StdHandles as H
import qualified Options.Applicative as Opt
import Shh
import System.Exit (exitFailure)

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

deploy :: ByteString -> Opt.ParserInfo Deploy
deploy localhost =
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
  localhost <- hostname |> readInput (pure . trim)
  (Deploy isFast isUnattended buildRemotely target _) <- Opt.execParser (deploy localhost)
  let
    isLocal = target == localhost
    package = ".#nixosConfigurations." <> target <> ".config.system.build.toplevel"
    hostPackage = ".#" <> target
    kind = bool "locally" "remotely" buildRemotely
    sshTarget = mkSshTarget target
    runSshCwd cmd = ssh sshTarget ("cd code/dotfiles; " <> cmd)

  when (isLocal && buildRemotely) do
    BS.putStrLn
      "Cannot build remotely and deploy locally. You probably want to run this on a different host."
    exitFailure

  BS.putStrLn $ "Running " <> kind <> " for " <> sshTarget

  when buildRemotely do
    BS.putStrLn "Pulling latest version on remote..."
    runSshCwd "git pull"

  when isLocal do cd "~/code/dotfiles"

  unless isFast do
    BS.putStrLn "Building... "
    path <- case buildRemotely of
      False -> do
        exe "nix" "build" package
        readlink "./result" |> captureTrim
      True -> do
        runSshCwd $ "nix build " <> package
        runSshCwd "readlink ./result" |> captureTrim

    case (isLocal, buildRemotely) of
      (False, False) -> do
        echo "Copying to remote host..."
        exe "nix" "copy" "-L" package "--no-check-sigs" "--to" ("ssh-ng://" <> sshTarget)
        ssh sshTarget "nvd" "diff" "/run/current-system" path
      (True, False) ->
        nvd "diff" "/run/current-system" path
      (_, True) ->
        runSshCwd $ "nvd diff /run/current-system " <> path

  shouldUpdate <-
    if isUnattended
      then pure True
      else do
        BS.putStr $
          "Update [local="
            <> bool "n" "y" isLocal
            <> ", remote="
            <> sshTarget
            <> ", buildRemotely="
            <> bool "n" "y" buildRemotely
            <> "] (y/n): "
        BS.hGet H.stdin 1 >>= \case
          "y" -> pure True
          _ -> pure False

  when shouldUpdate do
    case (isLocal, buildRemotely) of
      (True, False) -> exe "sudo" "nixos-rebuild" "switch" "--flake" hostPackage
      (False, True) -> runSshCwd $ "sudo nixos-rebuild switch --flake " <> hostPackage
      _ ->
        exe
          "nixos-rebuild"
          "--flake"
          hostPackage
          "--target-host"
          sshTarget
          "--use-remote-sudo"
          "switch"

mkSshTarget :: BS.ByteString -> BS.ByteString
mkSshTarget =
  \case
    "apate" -> error "Apate is not yet supported"
    "arche" -> "every@arche"
    sshTarget -> "evie@" <> sshTarget
