{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

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
import XMonad.Layout.NoBorders
import XMonad.StackSet qualified as W
import XMonad.Util.Loggers
import Prelude

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ manageHook gnomeConfig,
      resource =? "stalonetray" --> doIgnore,
      manageDocks
    ]

myStartupHook :: X ()
myStartupHook = do
  installSignalHandlers
  spawn "stalonetray"

screenshotCommand :: String
screenshotCommand = "/usr/bin/env fish --command clip"

newBindings :: XConfig l -> [((KeyMask, KeySym), X ())]
newBindings x =
  [ ((modMask x, xK_Right), nextWS),
    ((modMask x, xK_Left), prevWS),
    ((modMask x, xK_p), spawn "rofi -show run"),
    ((modMask x, xK_s), spawn "rofi -show ssh -terminal wezterm"),
    ((modMask x, xK_o), spawn "rofi-pass"),
    ((modMask x, xK_a), spawn "xrandr --output HDMI-1 --mode 1920x1080 --right-of DP-2"),
    ((modMask x, xK_z), spawn "xrandr --output HDMI-1 --off"),
    ((0, 0x1008ff12), spawn "amixer set Master 1+ toggle"),
    ((0, 0x1008ff11), spawn "amixer set Master 10%-"),
    ((0, 0x1008ff13), spawn "amixer set Master 10%+"),
    ((0, 0x1008ffb2), spawn "amixer set Capture toggle"),
    ((0, 0x1008ff03), spawn "xbacklight -10"),
    ((0, 0x1008ff02), spawn "xbacklight +10"),
    ((modMask x, xK_F1), spawn "autorandr --change"),
    ((0, xK_Print), spawn screenshotCommand),
    ((modMask x, xK_j), windows W.focusUp),
    ((modMask x, xK_k), windows W.focusDown),
    ((modMask x, xK_w), viewScreen horizontalScreenOrderer (P 0)),
    ((modMask x, xK_e), viewScreen horizontalScreenOrderer (P 1)),
    ((modMask x, xK_r), viewScreen horizontalScreenOrderer (P 2))
  ]

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys x = M.union (M.fromList (newBindings x)) (keys def x)

main :: IO ()
main = do
  xmonad . ewmhFullscreen . ewmh . withXmobar $
    def
      { manageHook = insertPosition Below Newer <+> myManageHook,
        layoutHook = avoidStruts . smartBorders $ layoutHook def,
        startupHook = myStartupHook,
        modMask = mod4Mask,
        keys = myKeys,
        terminal = "wezterm",
        handleEventHook =
          mconcat
            [ -- this is deprecated; should figure out how to remove it
              docksEventHook,
              handleEventHook def
            ]
      }

withXmobar :: XConfig _ -> XConfig _
withXmobar =
  SB.withEasySB
    (SB.statusBarProp "xmobar" (pure pp))
    SB.defToggleStrutsKey
  where
    pp :: PP
    pp =
      def
        { ppSep = magenta " • ",
          ppVisible = blue . wrap "(" ")",
          ppCurrent = magenta . wrap "[" "]" . xmobarBorder "Top" "#8be9fd" 2,
          ppHidden = white . wrap " " "",
          ppHiddenNoWindows = id,
          ppRename = \s _ -> s,
          ppUrgent = red . wrap (yellow "!") (yellow "!"),
          ppOrder = \case
            [ws, l, _activeWin, allWindows] -> [ws, l, allWindows]
            xs -> xs,
          ppExtras = [logTitles formatFocused formatUnfocused],
          ppWsSep = " ",
          ppTitle = shorten 80,
          ppTitleSanitize = xmobarStrip
        }

    formatFocused :: String -> String
    formatFocused = wrap (white "[") (white "]") . magenta . ppWindow

    formatUnfocused :: String -> String
    formatUnfocused = wrap (lowWhite "(") (lowWhite ")") . blue . ppWindow

    ppWindow :: String -> String
    ppWindow = xmobarRaw . cleanWindowTitle . shorten 30

    cleanWindowTitle :: String -> String
    cleanWindowTitle =
      \case
        'A' : 'l' : 'l' : ' ' : 'g' : 'o' : 'o' : 'd' : ',' : ' ' : _xs -> "ghcid"
        'v' : 'i' : ' ' : rest -> "vi " <> takeBaseName rest
        [] -> "untitled"
        xs -> xs

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta = xmobarColor "#ff79c6" ""
    blue = xmobarColor "#bd93f9" ""
    white = xmobarColor "#f8f8f2" ""
    yellow = xmobarColor "#f1fa8c" ""
    red = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
