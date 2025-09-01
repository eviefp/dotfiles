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

import Data.Bool (bool)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified GHC.IO.StdHandles as H
import Shh
import qualified System.Environment as Env

$(loadEnv SearchPath)

main :: IO ()
main = do
  (deployTo, isLocal) <-
    Env.getArgs >>= \case
      [] -> hostname |> readInput (pure . (,True) . trim)
      [arg] -> pure $ (BS.pack arg, False)
      _ -> error "Expected either no arguments or a single argument, the hostname"

  let
    package = ".#nixosConfigurations." <> deployTo <> ".config.system.build.toplevel"
    hostPackage = ".#" <> deployTo
    remote = mkRemote deployTo

  BS.putStrLn $ "Running for " <> remote

  BS.putStrLn "Building... "
  exe "nix" "build" package

  path <- readlink "./result" |> readInput (pure . trim)

  if (not isLocal)
    then do
      echo "Copying..."
      exe "nix" "copy" "-L" package "--no-check-sigs" "--to" ("ssh-ng://" <> remote)
      ssh remote "nvd" "diff" "/run/current-system" path
    else do
      nvd "diff" "/run/current-system" path

  BS.putStr $ "Update [local=" <> bool "n" "y" isLocal <> ", remote=" <> remote <> "] (y/n): "
  BS.hGet H.stdin 1 >>= \case
    "y" ->
      if isLocal
        then exe "sudo" "nixos-rebuild" "switch" "--flake" hostPackage
        else
          exe
            "nixos-rebuild"
            "--flake"
            hostPackage
            "--target-host"
            remote
            "--use-remote-sudo"
            "switch"
    _ -> pure ()

mkRemote :: BS.ByteString -> BS.ByteString
mkRemote =
  \case
    "arche" -> "every@arche"
    remote -> "evie@" <> remote
