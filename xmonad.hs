
import XMonad
import System.Exit

-- For the gimp layout.
import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect

import XMonad.Layout.Grid

-- xmobar
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import Data.List

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import System.IO

myIBrowser = "Shiretoko"
myTerminal = "terminal"
myBorderWidth   = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--
-- Set numlockMask = 0 if you don't have a numlock key, or want to treat
-- numlock status separately.
--
myNumlockMask   = mod2Mask

myWorkspaces    = ["1-main","2-www","3-irc","4-im","5-oof","6-pal","7-win","8","9"]
myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#000000"
myDefaultGaps = [(18,0,0,0)]

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)                            -- Lanch a terminal
    , ((modMask              , xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")  -- Launch dmenu
    , ((modMask .|. shiftMask, xK_p     ), spawn "gmrun")                                           -- Launch gmrun
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
    , ((modMask              , xK_h     ), sendMessage Shrink)                                      -- Shrink the master area
    , ((modMask              , xK_l     ), sendMessage Expand)                                      -- Expand the master area
    , ((modMask              , xK_t     ), withFocused $ windows . W.sink)                          -- Push window back into tiling
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))                              -- Increment the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))                           -- Deincrement the number of windows in the master area
    -- TODO, update this binding with avoidStruts , ((modMask              , xK_b     ),            -- Toggle the status bar gap
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))                               -- Quit xmonad
    , ((modMask              , xK_q     ), restart "xmonad" True)                                   -- Restart xmonad
    , ((0                    , xK_Print ), spawn "`scrot -e 'mv $f ~/Pictures/scrot'`")
    , ((controlMask          , xK_Print ), spawn "sleep 0.2; scrot -s -e `mv $f ~/Pictures/`")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

myLayout = tiled ||| Mirror tiled ||| Grid ||| Full ||| gimp
  where
     tiled = Tall nmaster delta ratio
     golden = toRational (2/(1 + sqrt 5 :: Double))
     gimp  = withIM (0.11) (Role "gimp-toolbox") $
             reflectHoriz $
             withIM (0.15) (Role "gimp-dock") Full

     nmaster = 1
     ratio   = 1/2
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--

myManageHook = composeAll . concat $
    [ [ className =? "MPlayer"   --> doFloat ]
    , [ className =? "Gimp"      --> doFloat ]
    , [ className =? myIBrowser --> doShift "2-www" ]
    , [ className =? "Pidgin"  --> doShift "4-im" ]
    , [ (className =? myIBrowser <&&> resource =? "Dialog") --> doFloat]
    , [ resource  =? "desktop_window" --> doIgnore ]
    , [ resource  =? "kdesktop"       --> doIgnore ]
    , [ fmap ( c `isInfixOf`) className --> doFloat | c <- myMatchAnywhereFloatsC ]
    , [ fmap ( c `isInfixOf`) title     --> doFloat | c <- myMatchAnywhereFloatsT ]
    ]
	
  where myMatchAnywhereFloatsC = ["Google", "Pidgin"]
        myMatchAnywhereFloatsT = ["VLC"]


myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--
myLogHook xmobar = dynamicLogWithPP $ defaultPP { 
    ppOutput = hPutStrLn xmobar, 
    ppTitle = xmobarColor "green" "" . shorten 50
    }

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.

myStartupHook = return ()

------------------------------------------------------------------------

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
                       layoutHook         = avoidStruts $ myLayout,
                       manageHook         = manageDocks <+> myManageHook,
                       startupHook        = myStartupHook,
                       logHook            = myLogHook xmobar
                     }
