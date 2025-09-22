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
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (fromMaybe)
import qualified Options.Applicative as Opt
import Shh

$(loadEnv SearchPath)

data Dotfiles
  = Nix2hs Nix2hsOptions
  | Hs2nix Hs2NixOptions

newtype Nix2hsOptions = Nix2hsOptions
  { name :: ByteString
  }

newtype Hs2NixOptions = Hs2NixOptions
  { name :: ByteString
  }
parseNameFor :: (ByteString -> a) -> Opt.Parser a
parseNameFor ctor = ctor <$> Opt.strArgument (Opt.metavar "name" <> Opt.help "name of the script")

commandParser :: Opt.ParserInfo Dotfiles
commandParser =
  Opt.info
    (parser <**> Opt.helper)
    ( Opt.fullDesc
        <> Opt.progDesc "Dotfiles helper -- see --help for more info"
        <> Opt.header "helps with dotfiles stuff"
    )
  where
    parser :: Opt.Parser Dotfiles
    parser =
      Opt.hsubparser
        . mconcat
        $ [ Opt.command
              "nix2hs"
              ( Opt.info
                  (Nix2hs <$> parseNameFor Nix2hsOptions)
                  (Opt.progDesc "copy script contents from haskell.nix to Main.hs")
              )
          , Opt.command
              "hs2nix"
              ( Opt.info
                  (Hs2nix <$> parseNameFor Hs2NixOptions)
                  (Opt.progDesc "copy script contents from Main.hs to haskell.nix")
              )
          ]

main :: IO ()
main = do
  Opt.execParser commandParser >>= \case
    Nix2hs opts -> nix2hs opts
    Hs2nix opts -> hs2nix opts

mainHs :: ByteString
mainHs = "/home/evie/code/dotfiles/scripts/dotfiles-script/exe/Main.hs"

haskellNix :: ByteString
haskellNix = "/home/evie/code/dotfiles/packages/haskell.nix"

tmpHaskellNix :: ByteString
tmpHaskellNix = "/home/evie/code/dotfiles/packages/haskell-tmp.nix"

hs2nix :: Hs2NixOptions -> IO ()
hs2nix Hs2NixOptions {..} = do
  program <- sed "-n" "/module Main where/,$p" mainHs |> capture
  nix "eval" "--file" haskellNix "--apply" (replaceText program)
    |> exe "alejandra" "--quiet"
      &> Truncate tmpHaskellNix
  mv tmpHaskellNix haskellNix
  where
    replaceText :: ByteString -> ByteString
    replaceText program =
      mconcat
        [ "p: "
        , "let pkgs = import <nixpkgs>{};"
        , "in pkgs.lib.attrsets.recursiveUpdate p {"
        , "programs."
        , name
        , ".text = "
        , orDefault (BS.stripPrefix "\\\"") . orDefault (BS.stripSuffix "\\\"") . BS.pack . show $ program
        , ";"
        , "}"
        ]
    orDefault :: forall a. (a -> Maybe a) -> a -> a
    orDefault fn = fromMaybe <*> fn

nix2hs :: Nix2hsOptions -> IO ()
nix2hs Nix2hsOptions {..} =
  nix
    "eval"
    "--raw"
    "--file"
    haskellNix
    "--apply"
    ("p: p.haskellLanguagePragmas + \"\n\" + p.programs." <> name <> ".text")
    &> Truncate mainHs
