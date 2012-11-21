import XMonad
import XMonad.Layout
import XMonad.Layout.Spacing
import XMonad.Layout.PerWorkspace
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.LimitWindows
import XMonad.Layout.FixedColumn
import XMonad.Layout.ThreeColumns
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import XMonad
import Data.Ratio ((%))

myManageHook :: ManageHook
myManageHook = composeAll
                [ manageDocks
                , className =? "URxvt"                    --> doShift "term"
                , className =? "oracle-ide-boot-Launcher" --> doShift "sql"
                , className =? "Gajim.py"                 --> doShift "chat"
                , className =? "Firefox"                  --> doShift "web"
                , className =? "Google-chrome"            --> doShift "web"]

gridLayout = spacing 1 $ Grid
gajimLayout = withIM (1%7) (Title "Gajim") gridLayout
termLayout = Full ||| threecols ||| twocols
  where
    threecols  = ThreeColMid nmaster delta ratio
    twocols    = Tall nmaster delta fiftyratio
    nmaster    = 1
    delta      = 3/100
    ratio      = 1/3
    fiftyratio = 50/100

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/gjones/.xmobarrc"
    xmonad $ defaultConfig
        { modMask         = mod4Mask
        , borderWidth     = 2
        , terminal        = "urxvt"
        , layoutHook      = avoidStruts $ onWorkspace "chat" gajimLayout $ onWorkspace "term" termLayout $ layoutHook defaultConfig
        , logHook         = dynamicLogWithPP $ xmobarPP
                            { ppOutput = hPutStrLn xmproc
                            , ppTitle = xmobarColor "gray" "" . shorten 50
                            }
        , normalBorderColor = "black"
        , focusedBorderColor = "#9FD091"
        , manageHook      = myManageHook
        , workspaces      = ["term","web","sql","chat","5","6","7","8","9"]
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
        ]

