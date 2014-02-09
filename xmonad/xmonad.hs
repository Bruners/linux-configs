-- vim: set sw=4 sts=4 et tw=80 :

import System.Exit
import System.IO

import XMonad

import XMonad.Actions.CycleWS

import XMonad.Layout.DecorationMadness
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Tabbed
import XMonad.Layout.ComboP
--import XMonad.Layout.Fullscreen
--import XMonad.Layout.Maximize
--import XMonad.Layout.Minimize
import XMonad.Layout.Named
import XMonad.Layout.SimplestFloat
import XMonad.Layout.NoBorders

import XMonad.Prompt
import XMonad.Prompt.Shell

import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (hPutStrLn, spawnPipe)
import XMonad.Util.Font
import XMonad.Util.Loggers

import XMonad.Hooks.DynamicLog hiding (dzen)
import XMonad.Hooks.ManageHelpers -- doCenterFloat, doFullFloat, isFullscreen
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicHooks
import XMonad.Hooks.ManageDocks

import Data.List    -- isInfixOf
import Data.Monoid

import qualified XMonad.StackSet as W

myWorkspaces :: [WorkspaceId]
myWorkspaces = [ wS1, wS2, wS3, wS4, wS5, wS6, wS7, wS8, wS9, wS10 ]

wS1  = "1:code"
wS2  = "2:www"
wS3  = "3:irssi"
wS4  = "4:code"
wS5  = "5:mp3"
wS6  = "6:video"
wS7  = "7:gimp"
wS8  = "8:games"
wS9  = "9:wine"
wS10 = "0:p2p"

dzenFg = "#909090"
dzenBg = "#202020"
myDarkGrey = "#333333"
myPaleGrey = "#666666"
myOrange = "#ff9900"
myGreen = "#99cc66"
myBlue = "#006699"
myCyan = "#66ccff"

myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
                { XMonad.Prompt.bgColor             = myDarkGrey
                , XMonad.Prompt.fgColor             = myGreen
                , XMonad.Prompt.bgHLight            = myPaleGrey
                , XMonad.Prompt.fgHLight            = myOrange
                , XMonad.Prompt.promptBorderWidth   = 0
                , XMonad.Prompt.position            = Bottom
                , XMonad.Prompt.height              = 13
                , XMonad.Prompt.historySize         = 256
                , XMonad.Prompt.showCompletionOnTab = True
                }
myTabConfig :: Theme
myTabConfig = defaultTheme
                { activeColor = dzenBg
                , activeBorderColor = myDarkGrey
                , activeTextColor = myCyan
                , inactiveColor = myDarkGrey
                , inactiveBorderColor = myDarkGrey
                , inactiveTextColor = dzenFg
                , urgentTextColor = myOrange
                , fontName = "xft:PragmataPro:regular:size=8:autohint=1"
                , decoHeight = 20
                , decoWidth = 20
                , windowTitleAddons = [ ("│", AlignRight), ("│", AlignLeft) ]
                }

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
                { ppCurrent = dzenColor myCyan myDarkGrey . pad
                , ppHidden = dzenColor "white" "" . pad . noScratchPad
                , ppHiddenNoWindows = dzenColor myPaleGrey "" . pad . noScratchPad
                , ppVisible = dzenColor "white" myDarkGrey . pad
                , ppUrgent = dzenColor "white" myOrange . pad . dzenStrip
                , ppTitle = dzenColor myGreen "" . shorten 300 . dzenEscape
                , ppWsSep = ""
                , ppSep = " "
                , ppLayout = wrap leftWrap rightWrap .
                (\x -> case x of
                    "Full" -> "[ ]"
                    "Tiled" -> dzenColor myGreen "" "|"
                    "Mirror Tiled" -> dzenColor myGreen "" "-"
                    "Tabs" -> dzenColor myGreen "" "\""
                    "ThreeCol" -> dzenColor myGreen "" "3"
                    "IRC" -> dzenColor myGreen "" "@"
                    "" -> dzenColor myGreen "" "#"
                    _ -> x)
                , ppOutput          = hPutStrLn h
                }
                where
                    noScratchPad ws = if ws == "NSP" then "" else ws
                    leftWrap = dzenColor myOrange "" "["
                    rightWrap = dzenColor myOrange "" "]"

myKey = [ ((controlMask, xK_bar), namedScratchpadAction myPad "terminal")
        , ((mod4Mask, xK_v), namedScratchpadAction myPad "mixer")
        , ((mod4Mask, xK_f), spawn "nautilus --no-desktop")
        , ((mod4Mask, xK_0), windows $ W.greedyView wS10)
        , ((mod4Mask .|. shiftMask, xK_0), windows $ W.shift wS10)
        , ((mod4Mask, xK_F12), myRestart)
        , ((mod4Mask, xK_F11), io (exitWith ExitSuccess))
        , ((mod4Mask, xK_p), shellPrompt myXPConfig)
        , ((mod1Mask, xK_F4), kill)
        , ((mod4Mask, xK_Right), nextWS)
        , ((mod4Mask, xK_Left), prevWS)
        ]
        ++
        [ ((m .|. mod4Mask, key), sc >>= screenWorkspace >>= flip whenJust (windows . f))
            | (key, sc) <- zip [xK_w, xK_e] [(screenBy (-1)),(screenBy 1)]
            , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
        ]

myPad :: [NamedScratchpad]
myPad = [ NS "mixer" spawnMixer findMixer manageMixer
        , NS "terminal" spawnTerm findTerm manageTerm
        ]
    where
        spawnMixer = "ossxmix -g 1280:140"
        findMixer = className =? "Ossxmix"
        manageMixer = defaultFloating

        spawnTerm = "systemctl --user start germinal@scratchpad"
        findTerm = resource =? "scratchpad"
        manageTerm = customFloating $ W.RationalRect l t w h

            where
                h = 0.6       -- height, 60%
                w = 0.5       -- width, 50%
                t = (1 - h)/2 -- centered top/bottom
                l = (1 - w)/2 -- centered left/right

myRestart = spawn
          $ "for pid in `pgrep conky`; do kill -9 $pid; done && " ++
            "for pid in `pgrep dzen2`; do kill -9 $pid; done && " ++
            "xmonad --recompile && xmonad --restart"

myLayouts = avoidStruts $
            onWorkspace wS3 (irc) $
            onWorkspace wS7 (simplestFloat ||| standardLayouts) $
            onWorkspaces [ wS6, wS8, wS9 ] (Full ||| tabs ||| threeCol) $
            standardLayouts

    where
        standardLayouts = (tabs ||| tiled ||| Mirror tiled ||| threeCol)
        tabs = named "Tabs" $ tabbed shrinkText myTabConfig
        tiled = named "Tiled" $ (ResizableTall 1 (2/100) (1/2) [])
        threeCol = named "ThreeCol" $ ThreeCol 2 (2/100) (1/3)
        irc = named "IRC" $ combineTwoP (Tall 1 (1/100) 0.15) (Mirror (ThreeCol 1 (2/100) (1/3))) (standardLayouts) (ClassName "Mumble" `Or` Role "buddy_list" `Or` ClassName "Skype")

myManageHook :: ManageHook
myManageHook = (composeAll . concat $
    [ [ fmap ( c `isInfixOf`) className <||> fmap ( c `isInfixOf`) title --> doFloat <+> doMaster | c <- myFloats ]
    , [ fmap ( c `isInfixOf`) className <||> fmap ( c `isInfixOf`) title --> doCenterFloat <+> doMaster | c <- myFloatC ]
    , [ fmap ( c `isInfixOf`) className <||> fmap ( c `isInfixOf`) title --> doFullFloat <+> doMaster | c <- myFloatF ]
    , [ fmap ( c `isInfixOf`) className <||> fmap ( c `isInfixOf`) title --> doShift myW | (myW, cs) <- myWSShift, c <- cs ]
    , [ fmap ( c `isInfixOf`) resource --> doIgnore | c <- myIgnores ]
    , [ isDialog <||> isMenu <||> isSplash --> doIgnore ]
    ])
    <+>
    composeOne [ isFullscreen -?> (doF W.focusDown <+> doFullFloat) ]

    where
        doMaster = doF W.shiftMaster
        myIgnores = [ "desktop_window", "desktop", "nm-applet", "NSP", "gsimplecal", "panel" ]

        isDialog = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"
        isMenu = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DIALOG"
        isSplash = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_MENU"

        myFloats = [ "Gpicview", "Vlc", "File-roller" ]
        myFloatC = [ "feh", "Xmessage", "Gmpc" ]
        myFloatF = [ "mplayer", "vdpau", "Gnome-mplayer", "operapluginwrapper-native" ]

        myWSShift = [ (wS1, [])
                    , (wS2, [ "Firefox", "Opera" ])
                    , (wS3, [ "IRC", "Pidgin", "Mangler", "Empathy", "Mumble", "Skype" ])
                    , (wS4, [ "VirtualBox" ])
                    , (wS5, [ "Spotify", "Quodlibet", "Gmpc" ])
                    , (wS6, [ "mplayer", "vdpau", "Gnome-mplayer", "plugin-container" ])
                    , (wS7, [ "Gimp", "libreoffice-startcenter", "Steam" ])
                    , (wS8, [ "Heroes of Newerth","explorer.exe" ])
                    , (wS9, [ "Wine" ]) ]

main :: IO ()
main = do
    dzen <- spawnPipe "dzen2 -p -ta l -dock -h 22 -w 1280 -e 'button3='"
    spawn $ "conky -c /home/lasseb/.xmonad/dzen_sys | dzen2 -x 1280 -p -ta r -dock -h 22 -w 640 -e 'button3='"
    spawn $ "conky -c /home/lasseb/.xmonad/dzen_mpd | dzen2 -x 1920 -p -ta l -dock -h 22 -w 1280 -e 'button3='"
    spawn $ "conky -c /home/lasseb/.xmonad/dzen_sys | dzen2 -x 3200 -p -ta r -dock -h 22 -w 487 -e 'button3='"
    xmonad $ withUrgencyHook dzenUrgencyHook $ defaultConfig
        { terminal = "/usr/bin/germinal"
        , focusFollowsMouse = True
        , borderWidth = 0
        , modMask = mod4Mask
        , workspaces = myWorkspaces
        , manageHook = manageDocks <+> myManageHook <+> namedScratchpadManageHook myPad
        , layoutHook = myLayouts
        , logHook = myLogHook dzen
        , handleEventHook = XMonad.Hooks.EwmhDesktops.fullscreenEventHook <+> docksEventHook
        }
        `additionalKeys` myKey

