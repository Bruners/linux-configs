-- {{{ Imports
import XMonad
import System.Exit
import System.IO

import Graphics.X11.Xlib
import Graphics.X11.Xinerama

import Data.List
import Data.Monoid
import qualified Data.Map as M

import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.ComboP
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Fullscreen
import XMonad.Layout.Maximize
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Named (named)

import XMonad.Util.Themes
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (hPutStrLn, spawnPipe)

import XMonad.Prompt
import XMonad.Prompt.Shell

import XMonad.Actions.DwmPromote
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleWindows

import XMonad.Hooks.DynamicLog hiding (dzen)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.Place
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName

import qualified XMonad.StackSet as W
-- }}}

myTerminal :: String
myTerminal = "urxvtc"

myBorderWidth :: Dimension
myBorderWidth = 0

myModMask :: KeyMask
myModMask = mod4Mask

myDefaultGaps ::  [(Integer, Integer, Integer, Integer)]
myDefaultGaps = [ (22,0,0,0) ]

-- {{{ keybindings - use xev to fin key codes
armorKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
armorKeys conf@(XConfig {XMonad.modMask = mM}) = M.fromList $
    [ ((mM .|. sM , xK_Return ), spawn $ XMonad.terminal conf) -- Lanch a terminal
    , ((mM        , xK_p      ), shellPrompt myXPConfig) -- Launch shellPromt
    , ((aM        , xK_F4     ), kill) -- Close focused window
    , ((mM        , xK_space  ), sendMessage NextLayout) -- Rotate layouts
    , ((mM .|. sM , xK_space  ), setLayout $ XMonad.layoutHook conf) -- Reset the layouts
    , ((mM        , xK_n      ), refresh) -- Resize windows
    , ((aM        , xK_Tab    ), cycleRecentWindows [xK_Alt_L] xK_Tab xK_Tab ) -- Move focus to next
    , ((mM        , xK_Tab    ), windows W.focusUp)
    , ((mM        , xK_j      ), windows W.focusDown) -- Move foucs to next
    , ((mM        , xK_k      ), windows W.focusUp) -- Move focus to prev
    , ((mM        , xK_m      ), windows W.focusMaster) -- Move focus to master
    , ((mM        , xK_Return ), dwmpromote) -- Swap focused with master or next in stack
    , ((mM .|. sM , xK_j      ), windows W.swapDown) -- Swap focused/next
    , ((mM .|. sM , xK_k      ), windows W.swapUp) -- Swap focused/prev
    , ((mM        , xK_h      ), sendMessage Shrink) -- Shrink area horiz
    , ((mM        , xK_l      ), sendMessage Expand) -- Expand area horiz
    , ((mM .|. cM , xK_h      ), sendMessage MirrorShrink) -- Shrink mirror vert
    , ((mM .|. cM , xK_l      ), sendMessage MirrorExpand) -- Expand mirror vert
    , ((mM        , xK_t      ), withFocused $ windows . W.sink) -- Push back into tiling
    , ((mM        , xK_F11    ), withFocused (sendMessage . maximizeRestore)) -- Push to float
    , ((mM .|. sM , xK_t      ), sendMessage ToggleStruts >> sendMessage ToggleLayout) -- Toggle Full layout
    , ((mM        , xK_comma  ), sendMessage (IncMasterN 1)) -- Increment windows
    , ((mM        , xK_period ), sendMessage (IncMasterN (-1))) -- Deincrement windows
    , ((mM .|. cM , xK_F11    ), io (exitWith ExitSuccess)) -- Quit xmonad
    , ((mM        , xK_F12    ), myRestart) -- Restart xmonad
    , ((0         , xK_Print  ), spawn "screenshot") -- Screenshot
    , ((mM        , xK_Print  ), spawn "screenshot scr") -- Screenshot screen
    , ((mM .|. sM , xK_Print  ), spawn "screenshot win") -- Screenshot window or area
    , ((mM        , xK_Left   ), prevWS) -- Cycle previous WS
    , ((mM        , xK_Right  ), nextWS) -- Cycle to next WS
    , ((mM .|. sM , xK_Left   ), shiftToPrev) -- Move WS to previous
    , ((mM .|. sM , xK_Right  ), shiftToNext) -- Move WS next WS
    , ((mM        , xK_s      ), sendMessage $ SwapWindow)
    , ((0         , 0x1008ff11), spawn "~/.bin/volume-osd -d 0.5") -- Reduce volume
    , ((0         , 0x1008ff13), spawn "~/.bin/volume-osd -i 1") -- Raise volume
    , ((0         , 0x1008ff12), spawn "~/.bin/volume-osd -t") -- Mute volume
    , ((mM .|. sM , xK_l      ), spawn "xscreensaver-command -lock") -- Lock screen
    , ((mM        , xK_b      ), sendMessage ToggleStruts) -- toggle xmobar gap
    , ((mM        , xK_f      ), spawn "nautilus --no-desktop") -- Start pcmanfm
    , ((cM        , xK_bar    ), scratchTerm) -- Spawn scratchpad terminal
    , ((mM        , xK_v      ), scratchMixer) -- Spawn scratchpad mixer
    , ((mM        , xK_0      ), windows $ W.greedyView myWS10) -- Switch to workspace 10
    , ((mM .|. sM , xK_0      ), windows $ W.shift myWS10) -- Move to workspace 10
    ]
    ++
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [ ((m .|. mM, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [ (W.greedyView, 0), (W.shift, shiftMask) ] ]
    ++
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    [ ((m .|. mM, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_e, xK_w, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask) ] ]

        where
            sM = shiftMask
            cM = controlMask
            aM = mod1Mask
            scratchTerm = namedScratchpadAction myScratchPads "terminal"
            scratchMixer = namedScratchpadAction myScratchPads "mixer"
            myRestart = spawn $ "for pid in `pgrep conky`; do kill -9 $pid; done && " ++
                                "for pid in `pgrep dzen2`; do kill -9 $pid; done && " ++
                                "xmonad --recompile && xmonad --restart"

myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w)    ) -- Float window and move with m1
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster) ) -- Raise window to the top of the stack
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w)  ) -- Float window and resize with m3
    ]

-- }}}
-- {{{ Colors used in different layouts ++
myNormalBorderColor  = "#202020"
myFocusedBorderColor = "#282828"
myGrey = "#303030"
myLightGrey = "#909090"
myLightGrey2 = "#606060"
myDarkGrey = "#202020"
myDarkGrey2 = "#262626"
myOrange = "#ee9a00"
myGreen = "green"
myGreen2 = "#99cc66"

myXPConfig :: XPConfig
myXPConfig = defaultXPConfig { XMonad.Prompt.bgColor = myDarkGrey
                             , XMonad.Prompt.fgColor = myLightGrey
                             , XMonad.Prompt.bgHLight = myGrey
                             , XMonad.Prompt.fgHLight = myOrange
                             , XMonad.Prompt.promptBorderWidth = 0
                             , XMonad.Prompt.position = Bottom
                             , XMonad.Prompt.height = 13
                             , XMonad.Prompt.historySize = 256
                             , XMonad.Prompt.showCompletionOnTab = True
                             }
myTabConfig ::  Theme
myTabConfig = defaultTheme { activeColor         = myGrey
                           , activeBorderColor   = myDarkGrey2
                           , activeTextColor     = myOrange
                           , inactiveColor       = myDarkGrey
                           , inactiveBorderColor = myDarkGrey2
                           , inactiveTextColor   = myLightGrey
                           , urgentTextColor     = myOrange
                           , decoHeight          = 15
                           }
-- }}}
-- {{{ Layouts
myLayout = avoidStruts $ toggleLayouts Full $ fullscreenFull $
           onWorkspace myWS2 (myTabbed ||| standardLayouts) $
           onWorkspace myWS3 (irc) $
           onWorkspace myWS7 (gimp ||| threeCol ||| standardLayouts) $
           onWorkspaces [ myWS6, myWS8, myWS9 ] (Full ||| simpleFloat ||| myTabbed ||| threeCol) $
           standardLayouts

    where
        standardLayouts = (tiled ||| Mirror tiled ||| tabs)
        tabs = (myTabbed ||| combineTabbed)
        tiled = (ResizableTall 1 (2/100) (1/2) [])
        irc = named "IRC" $ combineTwoP (Tall 1 (1/100) 0.15) (Mirror threeCol) (tabs ||| standardLayouts) (ClassName "Mumble" `Or` Role "buddy_list" `Or` ClassName "Skype")
        myTabbed = tabbed shrinkText myTabConfig
        combineTabbed = named "Combine Tabbed" $ combineTwoP (TwoPane 0.03 0.5) (myTabbed) (myTabbed) (ClassName "URxvt")
        threeCol = ThreeCol 1 (3/100) (1/2)
        gimp = withIM (0.11) (Role "gimp-toolbox") $
               reflectHoriz $
               withIM (0.15) (Role "gimp-dock") Full
        nmaster = 1
        ratio = 1/2
        delta = 3/100
-- }}}
-- {{{ Workspace variables for easy renaming
myWorkspaces ::  [WorkspaceId]
myWorkspaces = [ myWS1, myWS2, myWS3, myWS4, myWS5, myWS6, myWS7, myWS8, myWS9, myWS10 ]

myWS0  = "0:p2p"
myWS1  = "1:code"
myWS2  = "2:www"
myWS3  = "3:irssi"
myWS4  = "4:code"
myWS5  = "5:mp3"
myWS6  = "6:video"
myWS7  = "7:gimp"
myWS8  = "8:games"
myWS9  = "9:wine"
myWS10 = "0:p2p"
-- }}}

-- To find the property name associated with a program, use xprop | grep WM_CLASS
-- To match on the WM_NAME, you can use 'title' in the same way that 'className' and 'resource' are used below.
myManageHook :: ManageHook
myManageHook = (composeAll . concat $
    [ [ fmap ( c `isInfixOf`) className <||> fmap ( c `isInfixOf`) title --> doShift myW | (myW, cs) <- myWSShift, c <- cs ]
    --, [ isFullscreen --> (doFullFloat <+> doMaster) ]
    , [ isDialog --> (doCenterFloat <+> doMaster) ]
    , [ classNotRole (cnf) --> (doCenterFloat <+> doMaster) | (cnf) <- windowFloats ]
    , [ fmap ( c `isInfixOf`) resource --> doIgnore | c <- myIgnores ]
    , [ fmap ( c `isInfixOf`) className <||> fmap ( c `isInfixOf`) title --> doFloat <+> doMaster | c <- myAnyFloats ]
    , [ fmap ( c `isInfixOf`) className <||> fmap ( c `isInfixOf`) title --> doCenterFloat <+> doMaster | c <- myCenFloats ]
    , [ fmap ( c `isInfixOf`) className <||> fmap ( c `isInfixOf`) title --> doFullFloat <+> doMaster | c <- myFulFloats ]
    ]) where
        doMaster = doF W.shiftMaster
        myIgnores = [ "desktop_window", "idesk", "nm-applet", "NSP" ]
        myAnyFloats = [ "Google", "Gpicview", "Vlc", "File-roller", "Gsimplecal" ]
        myCenFloats = [ "feh", "Xmessage", "Gmpc" ]
        myFulFloats = [ "mplayer", "vdpau", "Gnome-mplayer" ]
        classNotRole (c,r) = className =? c <&&> (stringProperty "WM_WINDOW_ROLE") /=? r
        windowFloats = [ ("Firefox", "browser") ]
        myWSShift = [ (myWS1, [])
                    , (myWS2, [ "Firefox", "Opera" ])
                    , (myWS3, [ "IRC", "Pidgin", "Mangler", "Empathy", "Mumble", "Skype" ])
                    , (myWS4, [ "VirtualBox" ])
                    , (myWS5, [ "Spotify", "Quodlibet", "Gmpc" ])
                    , (myWS6, [ "mplayer", "vdpau", "Gnome-mplayer" ])
                    , (myWS7, [ "Gimp", "libreoffice-startcenter" ])
                    , (myWS8, [ "Heroes of Newerth","explorer.exe" ])
                    , (myWS9, [ "Wine" ]) ]

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- {{{ See the 'Dynami cLog' extension for examples.
-- To emulate dwm's status bar logHook = dynamicLogDzen
myLogHook ::  Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    { ppCurrent         = dzenColor "white" myGrey . pad
    , ppHidden          = dzenColor myLightGrey "" . pad . noScratchPad
    , ppHiddenNoWindows = dzenColor myLightGrey2 "" . pad . noScratchPad
    , ppVisible         = dzenColor "white" "" . pad
    , ppUrgent          = dzenColor "white" myOrange . pad . dzenStrip
    , ppTitle           = dzenColor myGreen "" . shorten 200 . dzenEscape
    , ppWsSep           = ""
    , ppSep             = " "
    , ppLayout          = dzenColor myLightGrey "" .
    (\x -> case x of
        "Full" -> "[ ]"
        "ResizableTall" -> dzenColor brackColor "" "[" <+> dzenColor namedColor "" "|" <+> dzenColor brackColor "" "]"
        "Mirror ResizableTall" -> dzenColor brackColor "" "[" <+> dzenColor namedColor "" "-" <+> dzenColor brackColor "" "]"
        "IM ReflectX IM Full" -> dzenColor brackColor "" "[" <+> dzenColor namedColor "" "G" <+> dzenColor brackColor "" "]"
        "Tabbed Simplest" -> dzenColor brackColor "" "[" <+> dzenColor namedColor "" "\"" <+> dzenColor brackColor "" "]"
        "Combine Tabbed" -> dzenColor brackColor "" "[" <+> dzenColor namedColor "" "\"\"" <+> dzenColor brackColor "" "]"
        "ThreeCol" -> dzenColor brackColor "" "[" <+> dzenColor namedColor "" "3" <+> dzenColor brackColor "" "]"
        "IRC" -> dzenColor brackColor "" "[" <+> dzenColor namedColor "" "@" <+> dzenColor brackColor "" "]"
        "" -> dzenColor brackColor "" "[" <+> dzenColor namedColor "" "#" <+> dzenColor brackColor "" "]"
        _ -> x)
    , ppOutput          = hPutStrLn h
    }

    where
        noScratchPad ws = if ws == "NSP" then "" else ws
        namedColor = "lightblue"
        brackColor = "orange"

-- }}}Scratchpad
myScratchPads ::  [NamedScratchpad]
myScratchPads = [ NS "mixer" spawnMixer findMixer manageMixer
                , NS "terminal" spawnTerm findTerm manageTerm
                ]
    where
        spawnMixer  = "ossxmix -g 1280:140"
        findMixer   = className =? "Ossxmix"
        manageMixer = customFloating $ W.RationalRect l t w h

            where
                h = 0.6       -- height, 60%
                w = 0.5       -- width, 60%
                t = (1 - h)/2 -- centered top/bottom
                l = (1 - w)/2 -- centered left/right

        --spawnTerm  = myTerminal ++ " -name scratchpad"
        spawnTerm  = myTerminal ++ " -name scratchpad -e tmux attach-session -t lasseb"
        findTerm   = resource =? "scratchpad"
        manageTerm = customFloating $ W.RationalRect l t w h

            where
                h = 0.6
                w = 0.5
                t = (1 - h)/2
                l = (1 - w)/2

myEwmhEvHook = XMonad.Hooks.EwmhDesktops.fullscreenEventHook
myFullEvHook = XMonad.Layout.Fullscreen.fullscreenEventHook

myPlaceHook ::  ManageHook
myPlaceHook = placeHook (withGaps (14,0,14,0) simpleSmart)

-- Perform an arbitrary action each time xmonad starts or is restarted with mod-q.
-- Used by, e.g., XMonad.Layout.PerWorkspace to initialize per-workspace layout choices.
armorStartupHook :: X ()
armorStartupHook = do
    setWMName "LG3D"
    spawn "xmodmap -e 'clear Lock'"
    spawn "xmodmap /home/lasseb/.Xmodmap"
    return ()

main :: IO ()
main = do
    dzen <- spawnPipe "dzen2 -p -ta l -dock -h 22 -w 1280 -e 'button3='"
    spawn $ "conky -c /home/lasseb/.xmonad/dzen_sys | dzen2 -x 1280 -p -ta r -dock -h 22 -w 640 -e 'button3='"
    spawn $ "conky -c /home/lasseb/.xmonad/dzen_mpd | dzen2 -x 1920 -p -ta l -dock -h 22 -w 1280 -e 'button3='"
    spawn $ "conky -c /home/lasseb/.xmonad/dzen_sys | dzen2 -x 3200 -p -ta r -dock -h 22 -w 487 -e 'button3='"
    xmonad $ withUrgencyHook dzenUrgencyHook $ defaultConfig
        { terminal           = myTerminal
        , focusFollowsMouse  = myFocusFollowsMouse
        , borderWidth        = myBorderWidth
        , modMask            = myModMask
        , workspaces         = myWorkspaces
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , keys               = armorKeys
        , mouseBindings      = myMouseBindings
        , layoutHook         = myLayout
        , manageHook         = myPlaceHook <+> fullscreenManageHook <+> manageDocks <+> myManageHook <+> namedScratchpadManageHook myScratchPads
        , handleEventHook    = myEwmhEvHook
        , startupHook        = armorStartupHook
        , logHook            = myLogHook dzen
        }
