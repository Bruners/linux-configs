
import XMonad
import System.Exit

-- For Xinerama

import Graphics.X11.Xlib
import Graphics.X11.Xinerama

-- For the gimp layout.
import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect

-- Other layouts
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile

-- Switch desktops

import XMonad.Actions.CycleWS

-- xmobar
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import Data.List

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import System.IO

myIBrowser = "Shiretoko"
myTerminal = "urxvtc"
myBorderWidth   = 1

myModMask       = mod4Mask
myNumlockMask   = mod2Mask

myWS1 = "1:main"
myWS2 = "2:www"
myWS3 = "3:irssi"
myWS4 = "4:wine"
myWS5 = "5:mp3"
myWS6 = "6:oof"
myWS7 = "7:gimp"
myWS8 = "8"
myWS9 = "9"

myWorkspaces = [ myWS1, myWS2, myWS3, myWS4, myWS5, myWS6, myWS7, myWS8, myWS9 ]
myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#000000"
myDefaultGaps = [(18,0,0,0)]

-- use xev to fin key codes
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)                            -- Lanch a terminal
    , ((modMask              , xK_p     ), spawn "exe=`dmenu_path | dmenu -fn \"terminus-8\" -nb \"#000000\" -nf \"#ffffff\" -sf orange -sb black` && eval \"exec $exe\"")  -- Launch dmenu
    , ((modMask .|. shiftMask, xK_c     ), kill)                                                    -- Close focused window
    , ((modMask              , xK_space ), sendMessage NextLayout)                                  -- Rotate through the available layout agorithms
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)                      -- Reset the layouts on the current workspace to default
    , ((modMask              , xK_n     ), refresh)                                                 -- Resize viewed windows to the correct size
    , ((modMask              , xK_Tab   ), windows W.focusDown)                                     -- Move focus to the next window
    , ((modMask              , xK_j     ), windows W.focusDown)                                     -- Move Foucs to the next window
    , ((modMask              , xK_k     ), windows W.focusUp  )                                     -- Move focus to the previous window
    , ((modMask              , xK_m     ), windows W.focusMaster  )                                 -- Move focus to the master window
    , ((modMask              , xK_Return), windows W.swapMaster)                                    -- Swap the focused window and the master window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )                                    -- Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )                                    -- Swap the focused window with the previous window
    , ((modMask              , xK_h     ), sendMessage Shrink )                                     -- Shrink the master area horiz
    , ((modMask              , xK_l     ), sendMessage Expand )                                     -- Expand the master area horiz
    , ((modMask .|. shiftMask, xK_h     ), sendMessage MirrorShrink )                               -- Shrink mirror vert
    , ((modMask .|. shiftMask, xK_l     ), sendMessage MirrorExpand )                               -- Expand mirror vert
    , ((modMask              , xK_t     ), withFocused $ windows . W.sink)                          -- Push window back into tiling
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))                              -- Increment the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))                           -- Deincrement the number of windows in the master area
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))                               -- Quit xmonad
    , ((modMask              , xK_q     ), restart "xmonad" True)                                   -- Restart xmonad
    , ((0                    , xK_Print ), spawn "`scrot -e 'mv $f ~/Pictures/scrot'`")             -- Capture screenshoot with scrot
    , ((controlMask          , xK_Print ), spawn "`sleep 0.2; scrot -s -e 'mv $f ~/Pictures/'`")      -- Capture screenshot with scrot
    , ((modMask              , xK_Left  ), prevWS )                                                 -- Cycle to previous workspace
    , ((modMask              , xK_Right ), nextWS )                                                 -- Cycle to next workspace
    , ((modMask .|. shiftMask, xK_Left  ), shiftToPrev )                                            -- Move current workspace to previous workspace
    , ((modMask .|. shiftMask, xK_Right ), shiftToNext )                                            -- Move current workspace to next workspace
    , ((0                    , 0x1008ff11 ), spawn "amixer set Master 10%- unmute")                 -- Reduce master volume by 10% and unmute
    , ((0                    , 0x1008ff13 ), spawn "amixer set Master 10%+ unmute")                 -- Raise master volume by 10% and unmute
    , ((0                    , 0x1008ff12 ), spawn "amixer set Master mute")                        -- Mute master volume
    -- , ((modMask .|. shiftMask, xK_l     ), spawn "xscreensaver-command -lock" )                     -- Lock the screen if xscreensaver is running
    ]
    ++

    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]                       -- mod-[1..9], Switch to workspace N
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]                        -- mod-shift-[1..9], Move client to workspace N
    ++

    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3

    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))                       -- mod-button1, Set the window to floating mode and move by dragging
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))                    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))                     -- mod-button3, Set the window to floating mode and resize by dragging
    ]

myLayout = onWorkspace myWS7 gimp $ standardLayouts
  where
     standardLayouts = avoidStruts $ (tiled ||| Mirror tiled ||| Grid ||| full)

     tiled = smartBorders (ResizableTall 1 (2/100) (1/2) [])
     gimp  = avoidStruts $ withIM (0.11) (Role "gimp-toolbox") $
             reflectHoriz $
             withIM (0.15) (Role "gimp-dock") Full
     full = noBorders Full
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100

-- To find the property name associated with a program, use xprop | grep WM_CLASS
-- To match on the WM_NAME, you can use 'title' in the same way that 'className' and 'resource' are used below.

myManageHook = composeAll . concat $
    [ [ fmap ( c `isInfixOf`) className --> doShift myWS1 | c <- myWS1ShiftC ]
    , [ fmap ( c `isInfixOf`) className --> doShift myWS2 | c <- myWS2ShiftC ]
    , [ fmap ( c `isInfixOf`) className --> doShift myWS3 | c <- myWS3ShiftC ]
    , [ fmap ( c `isInfixOf`) className --> doShift myWS4 | c <- myWS4ShiftC ]
    , [ fmap ( c `isInfixOf`) className --> doShift myWS5 | c <- myWS5ShiftC ]
    , [ fmap ( c `isInfixOf`) className --> doShift myWS6 | c <- myWS6ShiftC ]
    , [ fmap ( c `isInfixOf`) className --> doShift myWS7 | c <- myWS7ShiftC ]
    , [ fmap ( c `isInfixOf`) className --> doShift myWS8 | c <- myWS8ShiftC ]
    , [ fmap ( c `isInfixOf`) className --> doShift myWS9 | c <- myWS9ShiftC ]
    , [ isFullscreen  --> doFullFloat ]
    , [ (className =? myIBrowser <&&> resource =? "Dialog") --> doFloat]
	, [ (className =? myIBrowser <&&> resource =? "Extension") --> doFloat]
    , [ resource  =? "desktop_window" --> doIgnore ]
    , [ resource  =? "kdesktop"       --> doIgnore ]
    , [ fmap ( c `isInfixOf`) className --> doFloat | c <- myMatchAnywhereFloatsC ]
    , [ fmap ( c `isInfixOf`) title     --> doFloat | c <- myMatchAnywhereFloatsT ]
    , [ fmap ( c `isInfixOf`) className --> doCenterFloat | c <- myMatchCenterFloatsC ]
    ]

  where myMatchAnywhereFloatsC = ["Google", "Pidgin", "Pavucontrol", "MPlayer", "Downloads"]
        myMatchCenterFloatsC = ["feh", "Xmessage", "Squeeze", "GQview"]
        myMatchAnywhereFloatsT = ["VLC", "vlc", "Downloads"]
       
        myWS1ShiftC = []
        myWS2ShiftC = [myIBrowser]
        myWS3ShiftC = ["Pidgin"]
        myWS4ShiftC = ["Wine"]
        myWS5ShiftC = ["Spotify", "Quodlibet"]
        myWS6ShiftC = ["OpenOffice.org 3.1"]
        myWS7ShiftC = ["Gimp"]
        myWS8ShiftC = []
        myWS9ShiftC = []
        myIgnores = ["trayer"]


myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- See the 'DynamicLog' extension for examples.
-- To emulate dwm's status bar logHook = dynamicLogDzen

myLogHook xmobar = dynamicLogWithPP $ defaultPP 
    { ppOutput = hPutStrLn xmobar
    , ppCurrent = xmobarColor "orange" ""
    , ppTitle = xmobarColor "green" "" . shorten 200
    , ppUrgent = xmobarColor "white" "red"
    , ppVisible = xmobarColor "white" ""
    -- , ppHidden = wrap "(" ")"
    , ppLayout = xmobarColor "grey" ""
    , ppSep = " - "
    }

-- Perform an arbitrary action each time xmonad starts or is restarted with mod-q.  
-- Used by, e.g., XMonad.Layout.PerWorkspace to initialize per-workspace layout choices.

myStartupHook = return ()

main = do xmobar <- spawnPipe "xmobar"
          xmonad $ defaultConfig {
                       terminal           = myTerminal,
                       focusFollowsMouse  = myFocusFollowsMouse,
                       borderWidth        = myBorderWidth,
                       modMask            = myModMask,
                       numlockMask        = myNumlockMask,
                       workspaces         = myWorkspaces,
                       normalBorderColor  = myNormalBorderColor,
                       focusedBorderColor = myFocusedBorderColor,
                       keys               = myKeys,
                       mouseBindings      = myMouseBindings,
                       layoutHook         = myLayout,
                       manageHook         = manageDocks <+> myManageHook,
                       startupHook        = myStartupHook,
                       logHook            = myLogHook xmobar
                     }
