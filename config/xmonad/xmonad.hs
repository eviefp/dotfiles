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
import Data.Maybe (catMaybes)
import qualified Data.IORef as Ref

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
    -- spawn "stalonetray"

screenshotCommand :: String
screenshotCommand = "/usr/bin/env fish --command clip"

workspaceList :: [Int]
workspaceList = [1..20]

type KeyAction :: Type
data KeyAction
  = WSNext
  | WSPrev
  | ToScreen Int
  | FocusUp
  | FocusDown
  | FocusMaster
  | SwapUp
  | SwapDown
  | SwapMaster
  | WExpand
  | WShrink
  | Sink
  | Kill
  | Restart
  | LayoutNext
  | LayoutReset
  | LaunchTerminal
  | ShowHelp
  | GotoWorkspace Int
  | MoveToWorkspace Int
  | ToggleWorkProfile
  | RofiRun
  | RofiSsh
  | RofiPass
  | Xrandr String
  | BrightnessUp
  | BrightnessDown
  | SoundToggle
  | VolumeUp
  | VolumeDown
  | MicrophoneToggle
  | Screenshot

evaluateKeyAction :: Ref.IORef Bool -> XConfig Layout -> KeyAction -> X ()
evaluateKeyAction workProfile conf =
  \case
    WSNext -> nextWS
    WSPrev -> prevWS
    ToScreen idx -> viewScreen horizontalScreenOrderer $ P idx
    FocusUp -> windows W.focusUp
    FocusDown -> windows W.focusDown
    FocusMaster -> windows W.focusMaster
    SwapUp -> windows W.swapUp
    SwapDown -> windows W.swapDown
    SwapMaster -> windows W.swapMaster
    WExpand -> sendMessage Expand
    WShrink -> sendMessage Shrink
    Sink -> withFocused $ windows . W.sink
    Kill -> kill
    Restart -> spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"
    LayoutNext -> sendMessage NextLayout
    LayoutReset -> setLayout $ layoutHook conf
    LaunchTerminal -> spawn $ terminal conf
    ToggleWorkProfile -> liftIO $ Ref.modifyIORef workProfile not
    ShowHelp -> xmessage mkHelp
    GotoWorkspace idx -> (liftIO $ Ref.readIORef workProfile) >>= DWO.withNthWorkspace W.greedyView . updateIndex idx
    MoveToWorkspace idx -> (liftIO $ Ref.readIORef workProfile) >>= DWO.withNthWorkspace W.shift . updateIndex idx
    RofiRun -> spawn "rofi -show run"
    RofiSsh -> spawn "rofi -show ssh -terminal wezterm"
    RofiPass -> spawn "rofi-pass"
    Xrandr command -> spawn command
    BrightnessUp -> spawn "sudo light -A 10"
    BrightnessDown -> spawn "sudo light -U 10"
    SoundToggle -> spawn "amixer set Master 1+ toggle"
    VolumeUp -> spawn "amixer set Master 10%+"
    VolumeDown -> spawn "amixer set Master 10%-"
    MicrophoneToggle -> spawn "amixer set Capture toggle"
    Screenshot -> spawn screenshotCommand
  where
    mkHelp = unlines . catMaybes . fmap (uncurry evaluateHelp) $ allKeys

updateIndex :: Int -> Bool -> Int
updateIndex originalIndex False = originalIndex
updateIndex originalIndex True
  | originalIndex < 10 = originalIndex + 10
  | otherwise = originalIndex - 10

evaluateHelp :: String -> KeyAction -> Maybe String
evaluateHelp key action =
  go key
    $ case action of
      WSNext -> Just "next workspace"
      WSPrev -> Just "prev workspace"
      ToScreen idx -> Just $ "to screen " <> show idx
      FocusUp -> Just "focus up"
      FocusDown -> Just "focus down"
      FocusMaster -> Just "focus master"
      SwapUp -> Just "swap up"
      SwapDown -> Just "swap down"
      SwapMaster -> Just "swap master"
      WExpand -> Just "expand"
      WShrink -> Just "shrink"
      Sink -> Just "sink"
      Kill -> Just "kill"
      Restart -> Just "restart"
      LayoutNext -> Just "next layout"
      LayoutReset -> Just "reset layout"
      LaunchTerminal -> Just "terminal"
      ShowHelp -> Just "show this message"
      GotoWorkspace idx
        | idx == 0 -> Just $ "goto workspace 1"
        | otherwise -> Nothing
      MoveToWorkspace idx
        | idx == 0 -> Just $ "move to workspace 1"
        | otherwise -> Nothing
      ToggleWorkProfile -> Just "toggle between personal and work"
      RofiRun -> Just "rofi run"
      RofiSsh -> Just "rofi ssh"
      RofiPass -> Just "rofi pass"
      Xrandr command -> Just $ "xrandr " <> command
      BrightnessUp -> Just "brightness up"
      BrightnessDown -> Just "brightness down"
      SoundToggle -> Just "sound toggle"
      VolumeUp -> Just "volume up"
      VolumeDown -> Just "volume down"
      MicrophoneToggle -> Just "microphone toggle"
      Screenshot -> Just "screenshot"
  where
    go :: String -> Maybe String -> Maybe String
    go k = fmap (padRight 20 k <>)

    padRight :: Int -> String -> String
    padRight desired_length s = s ++ replicate (desired_length - length s) ' '

allKeys :: [(String, KeyAction)]
allKeys =
  mconcat
        [ xmonadKeys
        , withWorkspaces "" GotoWorkspace
        , withWorkspaces "C-" MoveToWorkspace
        , rofi
        , xrandr
        , brightness
        , audio
        , extra
        ]
  where
    xmonadKeys :: [(String, KeyAction)]
    xmonadKeys =
      [ ("M-<Right>", WSNext)
      , ("M-<Left>", WSPrev)
      , ("M-S-l", WSNext)
      , ("M-S-h", WSPrev)

      , ("M-w", ToScreen 0)
      , ("M-e", ToScreen 1)
      , ("M-r", ToScreen 2)

      , ("M-j", FocusUp)
      , ("M-k", FocusDown)
      , ("M-m", FocusMaster)

      , ("M-S-j", SwapUp)
      , ("M-S-k", SwapDown)
      , ("M-<Return>", SwapMaster)

      , ("M-l", WExpand)
      , ("M-h", WShrink)

      , ("M-t", Sink)

      , ("M-S-c", Kill)
      , ("M-q", Restart)

      , ("M-<Space>", LayoutNext)
      , ("M-S-<Space>", LayoutReset)
      , ("M-S-<Return>", LaunchTerminal)

      , ("M-/", ShowHelp)
      , ("M-=", ToggleWorkProfile)
      ]

    withWorkspaces :: String -> (Int -> KeyAction) -> [(String, KeyAction)]
    withWorkspaces extraPrefix go =
      mconcat
        [ [ ("M-" <> extraPrefix <> show n, go (n - 1))
          | n <- [1 .. 9]
          ]
        , [ ("M-" <> extraPrefix <> "0", go 9) ]
        , [ ("M-S-" <> extraPrefix <> show n, go (n + 9))
          | n <- [1 .. 9]
          ]
        , [ ("M-S-" <> extraPrefix <> "0", go 19) ]
        ]

    rofi :: [(String, KeyAction)]
    rofi =
      [ ("M-p", RofiRun)
      , ("M-s", RofiSsh)
      , ("M-o", RofiPass)
      ]

    xrandr :: [(String, KeyAction)]
    xrandr =
      [ ("M-a", Xrandr "xrandr --output HDMI-1 --mode 1920x1080 --right-of DP-2")
      , ("M-z", Xrandr "xrandr --output HDMI-1 --off")
      , ("M-x", Xrandr "xrandr --output DP-0 --mode 1920x1080 --rate 239.76 --left-of DP-2 --primary; xrandr --output DP-2 --mode 1920x1080 --rate 239.76")
      ]

    brightness :: [(String, KeyAction)]
    brightness =
      [ ("M-g", BrightnessUp)
      , ("M-S-g", BrightnessDown)
      ]

    audio :: [(String, KeyAction)]
    audio =
      [ ("M-y", SoundToggle)
      , ("M-u", VolumeDown)
      , ("M-S-u", VolumeUp)
      , ("M-S-y", MicrophoneToggle)
      ]

    extra :: [(String, KeyAction)]
    extra =
      [ ("<Print>", Screenshot)
      ]

keybindings :: Ref.IORef Bool -> XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keybindings workProfile conf =
    EZ.mkKeymap conf
      . (fmap . fmap) (evaluateKeyAction workProfile conf) $ allKeys

  where

main :: IO ()
main = do
    workProfile <- Ref.newIORef False
    xmonad . ewmhFullscreen . ewmh . withUrgencyHook NoUrgencyHook . withXmobar $
        def
            { manageHook = insertPosition Below Newer <+> myManageHook
            , workspaces = show <$> workspaceList
            , layoutHook =
                avoidStruts . smartBorders $
                      tall
                      ||| (LM.ModifiedLayout (Wrapper @"Drawer Circle") $ Drawer.drawer 0.01 0.8 (Prop.ClassName "org.wezfurlong.wezterm") dwindle `Drawer.onTop` Circle )
                      ||| (LM.ModifiedLayout (Wrapper @"Drawer Tall") $ Drawer.drawer 0.01 0.6 (Prop.ClassName "org.wezfurlong.wezterm") tall `Drawer.onTop` tall)
                      ||| noBorders Full
                      ||| Roledex
            , startupHook = myStartupHook
            , modMask = mod4Mask
            , keys = keybindings workProfile
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
