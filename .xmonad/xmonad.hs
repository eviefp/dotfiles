import qualified Data.Map as M
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.SpawnOnce
import System.IO

myManageHook = composeAll (
    [ manageHook gnomeConfig
    , className =? "Google-chrome" --> doFloat
    , manageDocks
    ])

myStartupHook = do
  spawnOnce "stalonetray --dockapp-mode simple"
  spawnOnce "unity-settings-daemon"
  spawnOnce "nm-applet"

newBindings x = [ ((modMask x, xK_Right), nextWS)
                , ((modMask x, xK_Left),  prevWS)
                ]

myKeys x = M.union (keys defaultConfig x) (M.fromList (newBindings x))


main = do
    xmproc <- spawnPipe "xmobar"

    xmonad $ defaultConfig
        { manageHook = myManageHook
        , layoutHook = avoidStruts  $ smartBorders $ layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , startupHook = myStartupHook
        , modMask = mod4Mask
        , keys = myKeys
        , terminal = "gnome-terminal"
        , handleEventHook =
          mconcat [ docksEventHook
                  , handleEventHook defaultConfig
                  ]
        }

