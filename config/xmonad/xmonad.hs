{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}

import Data.List (isPrefixOf)
import Data.Map qualified as M
import System.FilePath
import System.IO
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar qualified as SB
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.NoBorders
import XMonad.Prelude (isSuffixOf)
import XMonad.StackSet qualified as W
import XMonad.Util.Loggers
import Prelude

import XMonad.Layout.Circle
import XMonad.Layout.Roledex
import XMonad.Util.EZConfig qualified as EZ
import qualified XMonad.Actions.DynamicWorkspaceOrder as DWO
import qualified XMonad.Layout.Drawer as Drawer
import qualified XMonad.Util.WindowProperties as Prop
import Data.Kind (Type)
import qualified XMonad.Layout.LayoutModifier as LM
import GHC.TypeLits (Symbol)
import qualified XMonad.Layout.Dwindle as DW

myManageHook :: ManageHook
myManageHook =
    composeAll
        [ manageHook gnomeConfig
        , resource =? "stalonetray" --> doIgnore
        , manageDocks
        ]

myStartupHook :: X ()
myStartupHook = do
    installSignalHandlers
    spawn "stalonetray"

screenshotCommand :: String
screenshotCommand = "/usr/bin/env fish --command clip"

workspaceList :: [Int]
workspaceList = [1..20]

keybindings :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keybindings conf =
    EZ.mkKeymap conf
      $ mconcat
        [ xmonadKeys
        , withWorkspaces "" W.greedyView
        , withWorkspaces "C-" W.shift
        , rofi
        , xrandr
        , brightness
        , audio
        , extra
        ]

  where
    xmonadKeys :: [(String, X ())]
    xmonadKeys =
      [ ("M-<Right>", nextWS)
      , ("M-<Left>", prevWS)
      , ("M-S-l", nextWS)
      , ("M-S-h", prevWS)

      , ("M-w", viewScreen horizontalScreenOrderer (P 0))
      , ("M-e", viewScreen horizontalScreenOrderer (P 1))
      , ("M-r", viewScreen horizontalScreenOrderer (P 2))

      , ("M-j", windows W.focusUp)
      , ("M-k", windows W.focusDown)
      , ("M-m", windows W.focusMaster)

      , ("M-S-j", windows W.swapUp)
      , ("M-S-k", windows W.swapDown)
      , ("M-<Return>", windows W.swapMaster)

      , ("M-l", sendMessage Expand)
      , ("M-h", sendMessage Shrink)

      , ("M-t", withFocused $ windows . W.sink)

      , ("M-S-c", kill)
      , ("M-q", spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")

      , ("M-<Space>", sendMessage NextLayout)
      , ("M-S-<Space>", setLayout $ layoutHook conf)
      , ("M-S-<Return>", spawn $ terminal conf)
      ]

    withWorkspaces :: String -> (String -> WindowSet -> WindowSet) -> [(String, X ())]
    withWorkspaces extraPrefix go =
      mconcat
        [ [ ("M-" <> extraPrefix <> show n, DWO.withNthWorkspace go (n - 1))
          | n <- [1 .. 9]
          ]
        , [ ("M-" <> extraPrefix <> "0", DWO.withNthWorkspace go 9) ]
        , [ ("M-S-" <> extraPrefix <> show n, DWO.withNthWorkspace go (n + 9))
          | n <- [1 .. 9]
          ]
        , [ ("M-S-" <> extraPrefix <> "0", DWO.withNthWorkspace go 19) ]
        ]

    rofi :: [(String, X ())]
    rofi =
      [ ("M-p", spawn "rofi -show run")
      , ("M-s", spawn "rofi -show ssh -terminal wezterm")
      , ("M-o", spawn "rofi-pass")
      ]

    xrandr :: [(String, X ())]
    xrandr =
      [ ("M-a", spawn "xrandr --output HDMI-1 --mode 1920x1080 --right-of DP-2")
      , ("M-z", spawn "xrandr --output HDMI-1 --off")
      , ("M-x", spawn "xrandr --output DP-0 --mode 1920x1080 --rate 239.76 --left-of DP-2 --primary"
                  *> spawn "xrandr --output DP-2 --mode 1920x1080 --rate 239.76")
      ]

    brightness :: [(String, X ())]
    brightness =
      [ ("M-g", spawn "xbacklight +10")
      , ("M-S-g", spawn "xbacklight -10")
      ]

    audio :: [(String, X ())]
    audio =
      [ ("M-y", spawn "amixer set Master 1+ toggle")
      , ("M-u", spawn "amixer set Master 10%-")
      , ("M-S-u", spawn "amixer set Master 10%+")
      , ("M-S-y", spawn "amixer set Capture toggle")
      ]

    extra :: [(String, X ())]
    extra =
      [ ("<Print>", spawn screenshotCommand)
      ]

main :: IO ()
main = do
    xmonad . ewmhFullscreen . ewmh . withUrgencyHook NoUrgencyHook . withXmobar $
        def
            { manageHook = insertPosition Below Newer <+> myManageHook
            , workspaces = show <$> workspaceList
            , layoutHook =
                avoidStruts . smartBorders $
                      (LM.ModifiedLayout (Wrapper @"Drawer Circle") $ Drawer.drawer 0.01 0.8 (Prop.ClassName "org.wezfurlong.wezterm") dwindle `Drawer.onTop` Circle )
                      ||| Circle
                      ||| (LM.ModifiedLayout (Wrapper @"Drawer Tall") $ Drawer.drawer 0.01 0.6 (Prop.ClassName "firefox") tall `Drawer.onTop` tall)
                      ||| (LM.ModifiedLayout (Wrapper @"Drawer Full") $ Drawer.drawer 0.01 0.8 (Prop.ClassName "org.wezfurlong.wezterm") dwindle `Drawer.onTop` noBorders Full)
                      ||| Roledex
            , startupHook = myStartupHook
            , modMask = mod4Mask
            , keys = keybindings
            , terminal = "wezterm"
            , handleEventHook =
                mconcat
                    [ -- this is deprecated; should figure out how to remove it
                      docksEventHook
                    , handleEventHook def
                    ]
            }
  where
    tall :: Tall a
    tall = Tall 1 (3 / 100) (3 / 4)

    dwindle :: DW.Dwindle a
    dwindle = DW.Dwindle DW.D DW.CCW (23 / 7) (3 / 100)

type Wrapper :: Symbol -> Type -> Type
data Wrapper s a = Wrapper
  deriving stock (Read, Show)

instance LM.LayoutModifier (Wrapper "Drawer Circle") a where
  modifyDescription :: Wrapper s a -> l a -> String
  modifyDescription _ _ = "Drawer Circle"

instance LM.LayoutModifier (Wrapper "Drawer Tall") a where
  modifyDescription :: Wrapper s a -> l a -> String
  modifyDescription _ _ = "Drawer Tall"

instance LM.LayoutModifier (Wrapper "Drawer Full") a where
  modifyDescription :: Wrapper s a -> l a -> String
  modifyDescription _ _ = "Drawer Full"

withXmobar :: XConfig _ -> XConfig _
withXmobar =
    SB.withEasySB
        (SB.statusBarProp "xmobar" (pure pp))
        SB.defToggleStrutsKey
  where
    pp :: PP
    pp =
        def
            { ppSep = magenta " • "
            , ppVisible = blue . wrap "(" ")"
            , ppCurrent = magenta . wrap "[" "]" . xmobarBorder "Bottom" "#8be9fd" 2
            , ppHidden = white . wrap "" ""
            , ppHiddenNoWindows = id
            , ppRename = \s _ -> s
            , ppOrder = \case
                [ws, l, _activeWin, allWindows] -> [ws, l, allWindows]
                xs -> xs
            , ppExtras = [logTitles formatFocused formatUnfocused]
            , ppWsSep = " "
            , ppTitle = shorten 80
            , ppTitleSanitize = xmobarStrip
            }

    formatFocused :: String -> String
    formatFocused = wrap (white "[") (white "]") . magenta . ppWindow

    formatUnfocused :: String -> String
    formatUnfocused = wrap (lowWhite "(") (lowWhite ")") . blue . ppWindow

    ppWindow :: String -> String
    ppWindow = xmobarRaw . cleanWindowTitle -- . shorten 30
    cleanWindowTitle :: String -> String
    cleanWindowTitle s
        | isPrefixOf "All good, " s = "ghcid"
        | isPrefixOf "Slack " s = "slack"
        | isPrefixOf "Discord " s = "discord"
        | s == "Signal" = "signal"
        | s == "Steam" = "steam"
        | s == "Volume Control" = "volume"
        | isPrefixOf "vi " s = "vi " <> takeBaseName (drop 3 s)
        | isSuffixOf " — Mozilla Firefox" s = reverse . drop (length @([]) " — Mozilla Firefox") . reverse $ s
        | s == [] = "untitled"
        | otherwise = s

    blue, lowWhite, magenta, white :: String -> String
    magenta = xmobarColor "#ff79c6" ""
    blue = xmobarColor "#bd93f9" ""
    white = xmobarColor "#f8f8f2" ""
    lowWhite = xmobarColor "#bbbbbb" ""
