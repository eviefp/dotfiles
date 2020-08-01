--
import Data.List
import Data.Maybe
--

import qualified Data.Map                       as M
import           System.IO
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Actions.PhysicalScreens
import           XMonad.Config.Gnome
import           XMonad.Core
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.NoBorders
import           XMonad.Operations
import qualified XMonad.StackSet                as W
import           XMonad.Util.Dzen
import           XMonad.Util.EZConfig           (additionalKeys, additionalKeysP)
import           XMonad.Util.Run                (spawnPipe)
import           XMonad.Util.SpawnOnce

--
-- Get the screen that a workspace is fixed to.
workspaceScreen :: (WorkspaceId -> Int) -> WorkspaceId -> WindowSet -> W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail
workspaceScreen f ws s =
    screens !! index
      where
        screens = sortOn W.screen $ W.screens s
        index = if length screens < f ws then 0 else f ws

-- Focus the monitor that a workspace is fixed to, without viewing the workspace.
focusScreen :: (WorkspaceId -> Int) -> WorkspaceId -> WindowSet -> WindowSet
focusScreen f ws s = W.view (W.tag . W.workspace $ workspaceScreen f ws s) s

-- View a workspace on a predefined monitor.
fixedView :: (WorkspaceId -> Int) -> WorkspaceId -> WindowSet -> WindowSet
fixedView f ws s
    -- Workspace already has focus, switch to last used workspace for this screen.
    | W.currentTag s == ws =
        let toShow = find (\h -> W.screen (workspaceScreen f (W.tag h) s) == W.screen (W.current s)) (W.hidden s)
        in maybe s (\a -> W.view (W.tag a) s) toShow
    -- Workspace does not have focus (visible or hidden).
    | otherwise = W.view ws . focusScreen f ws $ s

--

myManageHook = composeAll (
    [ manageHook gnomeConfig
    , resource  =? "stalonetray"       --> doIgnore
    , manageDocks
    ])

myStartupHook = pure ()

screenshotCommand = "/usr/bin/env fish --command clip"

newBindings x = [ ((modMask x, xK_Right                  ), nextWS)
                , ((modMask x, xK_Left                   ), prevWS)
                -- , ((modMask x, xK_BackSpace              ), spawn "gnome-screensaver-command -l")
                -- , ((modMask x .|. shiftMask, xK_BackSpace), spawn "systemctl suspend -i")
                , ((0        , 0x1008ff12), spawn "amixer set Master 1+ toggle")
                , ((0        , 0x1008ff11), spawn "amixer set Master 10%-")
                , ((0        , 0x1008ff13), spawn "amixer set Master 10%+")
                , ((0        , 0x1008ffb2), spawn "amixer set Capture toggle")
                , ((0        , 0x1008ff03), spawn "xbacklight -10")
                , ((0        , 0x1008ff02), spawn "xbacklight +10")
                , ((modMask x, xK_F1     ), spawn "autorandr --change")
                , ((0        , xK_Print  ), spawn screenshotCommand)
                , ((modMask x, xK_j      ), windows W.focusUp)
                , ((modMask x, xK_k      ), windows W.focusDown)
                , ((modMask x, xK_w      ), viewScreen horizontalScreenOrderer (P 0))
                , ((modMask x, xK_e      ), viewScreen horizontalScreenOrderer (P 1))
                ]

myKeys x = M.union (M.fromList (newBindings x)) (keys defaultConfig x)

myOtherKeys =
    [ ("M-" ++ ws,   windows $ fixedView workspaceScreens ws)
      | ws <- myWorkspaces ]

myWorkspaces = map show [1 .. 9]

workspaceScreens ws
  | ws == "8" = 1
  | ws == "9" = 2
  | otherwise = 0


main = do
    xmproc <- spawnPipe "xmobar"

    xmonad $ gnomeConfig
        { manageHook = insertPosition Below Newer <+> myManageHook
        , layoutHook = avoidStruts  $ smartBorders $ layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , startupHook = myStartupHook
        , modMask = mod4Mask
        , keys = myKeys
        , terminal = "kitty"
        , workspaces = myWorkspaces
        , handleEventHook =
          mconcat [ docksEventHook
                  , handleEventHook defaultConfig
                  ]
        } `additionalKeysP` myOtherKeys

