import qualified Data.Map                    as M
import           System.IO
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Config.Gnome
import           XMonad.Core
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.NoBorders
import           XMonad.Util.Dzen
import           XMonad.Util.EZConfig        (additionalKeys)
import           XMonad.Util.Run             (spawnPipe)
import           XMonad.Util.SpawnOnce

myManageHook = composeAll (
    [ manageHook gnomeConfig
    , resource  =? "stalonetray"   --> doShift "1. dev" -- >>= doIgnore
    , className =? "Google-chrome" --> doShift "2. web"
    , className =? "slack"         --> doShift "3. slack"
    , className =? "Steam"         --> doShift "4. steam"
    , className =? "steam"         --> doIgnore
    , className =? "vlc"           --> doFullFloat
    , manageDocks
    ])

myStartupHook = do
  spawnOnce "stalonetray --dockapp-mode simple"
  spawnOnce "unity-settings-daemon"
  spawnOnce "nm-applet"

newBindings x = [ ((modMask x, xK_Right  ), nextWS)
                , ((modMask x, xK_Left   ), prevWS)
                , ((0        , 0x1008ff12), spawn "amixer -D pulse sset Master 1+ toggle")
                , ((0        , 0x1008ff11), spawn "amixer -D pulse sset Master 10%-")
                , ((0        , 0x1008ff13), spawn "amixer -D pulse sset Master 10%+")
                ]

myKeys x = M.union (keys defaultConfig x) (M.fromList (newBindings x))

myWorkspaces = map show [1 .. 9]
  -- [1. dev", "2. web", "3. slack", "4. steam", "5. fullscreen" ] ++ map show [6 .. 9]

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
        , terminal = "gnome-terminal"
        , workspaces = myWorkspaces
        , handleEventHook =
          mconcat [ docksEventHook
                  , handleEventHook defaultConfig
                  ]
        }

