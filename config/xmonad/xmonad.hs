import Prelude

import qualified Data.Map                       as M
import           System.IO
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Actions.PhysicalScreens
import           XMonad.Config.Gnome
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.NoBorders
import qualified XMonad.StackSet                as W
import           XMonad.Util.Run
    (spawnPipe)

myManageHook :: ManageHook
myManageHook = composeAll
    [ manageHook gnomeConfig
    , resource  =? "stalonetray"       --> doIgnore
    , manageDocks
    ]

myStartupHook :: X ()
myStartupHook = do
    installSignalHandlers
    spawn "stalonetray"
    spawn "nm-applet"

screenshotCommand :: String
screenshotCommand = "/usr/bin/env fish --command clip"

newBindings :: XConfig l -> [((KeyMask, KeySym), X ())]
newBindings x = [ ((modMask x, xK_Right                  ), nextWS)
                , ((modMask x, xK_Left                   ), prevWS)
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

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys x = M.union (M.fromList (newBindings x)) (keys def x)

myWorkspaces :: [String]
myWorkspaces = fmap (show @Int) [1 .. 9]

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar"

    xmonad $ gnomeConfig
        { manageHook = insertPosition Below Newer <+> myManageHook
        , layoutHook = avoidStruts . smartBorders $ layoutHook def
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 100
                        }
        , startupHook = myStartupHook
        , modMask = mod4Mask
        , keys = myKeys
        , terminal = "kitty"
        , workspaces = myWorkspaces
        , handleEventHook =
          mconcat [ docksEventHook
                  , handleEventHook def
                  ]
        }

