
import XMonad
import System.Exit
-- module to get host information
import System.Posix.Unistd
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
import XMonad.Util.Run (hPutStrLn, spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import Data.List

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import System.IO

myTerminal = "urxvtc"
myBorderWidth   = 1

myModMask       = mod4Mask
myNumlockMask   = mod2Mask

myWorkspaces = [ myWS1, myWS2, myWS3, myWS4, myWS5, myWS6, myWS7, myWS8, myWS9 ]
myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#000000"
myDefaultGaps = [(18,0,0,0)]
myDmenu = "exe=`dmenu_path | dmenu -nb \"#000000\" -nf \"#ffffff\" -fn \"terminus-8\"` && eval \"exec $exe\""
-- use xev to fin key codes
armorKeys conf@(XConfig {XMonad.modMask = mM}) = M.fromList $
    [ ((mM .|. sM , xK_Return ), spawn $ XMonad.terminal conf      ) -- Lanch a terminal
    , ((mM        , xK_p      ), spawn myDmenu                     )  -- Launch dmenu
   -- , ((mM .|. sM , xK_p      ), spawn "PieDock"                   ) -- Spawn PieDock
    , ((mM .|. sM , xK_c      ), kill                              ) -- Close focused window
    , ((mM        , xK_space  ), sendMessage NextLayout            ) -- Rotate layouts
    , ((mM .|. sM , xK_space  ), setLayout $ XMonad.layoutHook conf) -- Reset the layouts
    , ((mM        , xK_n      ), refresh                           ) -- Resize windows
    , ((mM        , xK_Tab    ), windows W.focusDown               ) -- Move focus to next
    , ((mM        , xK_j      ), windows W.focusDown               ) -- Move foucs to next
    , ((mM        , xK_k      ), windows W.focusUp                 ) -- Move focus to prev
    , ((mM        , xK_m      ), windows W.focusMaster             ) -- Move focus to master
    , ((mM        , xK_Return ), windows W.swapMaster              ) -- Swap focused/master
    , ((mM .|. sM , xK_j      ), windows W.swapDown                ) -- Swap focused/next
    , ((mM .|. sM , xK_k      ), windows W.swapUp                  ) -- Swap focused/prev
    , ((mM        , xK_h      ), sendMessage Shrink                ) -- Shrink area horiz
    , ((mM        , xK_l      ), sendMessage Expand                ) -- Expand area horiz
    , ((mM .|. cM , xK_h      ), sendMessage MirrorShrink          ) -- Shrink mirror vert
    , ((mM .|. cM , xK_l      ), sendMessage MirrorExpand          ) -- Expand mirror vert
    , ((mM        , xK_t      ), withFocused $ windows . W.sink    ) -- Push back into tiling
    , ((mM        , xK_comma  ), sendMessage (IncMasterN 1)        ) -- Increment windows
    , ((mM        , xK_period ), sendMessage (IncMasterN (-1))     ) -- Deincrement windows
    , ((mM .|. sM , xK_q      ), io (exitWith ExitSuccess)         ) -- Quit xmonad
    , ((mM        , xK_q      ), restart "xmonad" True             ) -- Restart xmonad
    , ((mM        , xK_Print  ), spawn "run_scrot scr"             ) -- Screenshot screen
    , ((mM .|. sM , xK_Print  ), spawn "run_scrot win"             ) -- Screenshot window or area
    , ((mM        , xK_Left   ), prevWS                            ) -- Cycle previous WS
    , ((mM        , xK_Right  ), nextWS                            ) -- Cycle to next WS
    , ((mM .|. sM , xK_Left   ), shiftToPrev                       ) -- Move WS to previous
    , ((mM .|. sM , xK_Right  ), shiftToNext                       ) -- Move WS next WS
    , ((0         , 0x1008ff11), spawn "~/.bin/vol-"               ) -- Reduce volume
    , ((0         , 0x1008ff13), spawn "~/.bin/vol+"               ) -- Raise volume
    , ((0         , 0x1008ff12), spawn "~/.bin/mute"               ) -- Mute volume
    , ((mM .|. sM , xK_l      ), spawn "xscreensaver-command -lock") -- Lock screen
    , ((mM        , xK_b      ), sendMessage ToggleStruts          ) -- toggle xmobar gap
    ]
    ++
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. mM, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    [((m .|. mM, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

	where 
	   sM = shiftMask
	   cM = controlMask

mushimKeys conf@(XConfig {XMonad.modMask = mM}) = M.fromList $
    [ ((mM .|. sM , xK_Return ), spawn $ XMonad.terminal conf      ) -- Lanch a terminal
    , ((mM        , xK_p      ), spawn myDmenu                     )  -- Launch dmenu
    , ((mM .|. sM , xK_p      ), spawn "gmrun"                     ) -- Launch gmrun
    , ((mM .|. sM , xK_c      ), kill                              ) -- Close focused window
    , ((mM        , xK_space  ), sendMessage NextLayout            ) -- Rotate through the available layout agorithms
    , ((mM .|. sM , xK_space  ), setLayout $ XMonad.layoutHook conf) -- Reset the layouts on the current workspace to default
    , ((mM        , xK_n      ), refresh                           ) -- Resize viewed windows to the correct size
    , ((mM        , xK_Tab    ), windows W.focusDown               ) -- Move focus to the next window
    , ((mM        , xK_j      ), windows W.focusDown               ) -- Move Foucs to the next window
    , ((mM        , xK_k      ), windows W.focusUp                 ) -- Move focus to the previous window
    , ((mM        , xK_m      ), windows W.focusMaster             ) -- Move focus to the master window
    , ((mM        , xK_Return ), windows W.swapMaster              ) -- Swap the focused window and the master window
    , ((mM .|. sM , xK_j      ), windows W.swapDown                ) -- Swap the focused window with the next window
    , ((mM .|. sM , xK_k      ), windows W.swapUp                  ) -- Swap the focused window with the previous window
    , ((mM        , xK_h      ), sendMessage Shrink                ) -- Shrink the master area
    , ((mM        , xK_l      ), sendMessage Expand                ) -- Expand the master area
    , ((mM        , xK_t      ), withFocused $ windows . W.sink    ) -- Push window back into tiling
    , ((mM        , xK_comma  ), sendMessage (IncMasterN 1)        ) -- Increment the number of windows in the master area
    , ((mM        , xK_period ), sendMessage (IncMasterN (-1))     ) -- Deincrement the number of windows in the master area
    , ((mM        , xK_b      ), sendMessage ToggleStruts          ) -- Toggle xmobar gap
    , ((mM .|. sM , xK_q      ), io (exitWith ExitSuccess)         ) -- Quit xmonad
    , ((mM        , xK_q      ), restart "xmonad" True             ) -- Restart xmonad
    , ((mM        , xK_Print  ), spawn "run_scrot scr"             ) -- Screenshot screen
    , ((mM .|. sM , xK_Print  ), spawn "run_scrot win"             ) -- Screenshot window or area
    , ((mM        , xK_Left   ), prevWS                            ) -- Cycle previous WS
    , ((mM        , xK_Right  ), nextWS                            ) -- Cycle next WS
    , ((mM .|. sM , xK_Left   ), shiftToPrev                       ) -- Move WS to previous
    , ((mM .|. sM , xK_Right  ), shiftToNext                       ) -- Move WS next WS
    , ((mM .|. sM , xK_l      ), spawn "xscreensaver-command -lock") -- Lock screen
    , ((0         , 0x1008ff13), spawn "~/bin/ossvol -i 1"         ) -- Raise volume
    , ((0         , 0x1008ff11), spawn "~/bin/ossvol -d 1"         ) -- Reduce volume
    , ((0         , 0x1008ff12), spawn "~/bin/ossvol -t"           ) -- Mute volume
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. mM, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. mM, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

        where sM = shiftMask
              cM = controlMask

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w)    ) -- Float window and move with m1
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster) ) -- Raise window to the top of the stack
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w)  ) -- Float window and resize with m3
--    , ((modMask, button4), (\_ -> prevWS)) -- switch to previous workspace
--    , ((modMask, button5), (\_ -> nextWS)) -- switch to next workspace
    ]

myLayout = onWorkspace myWS5 gimp $ standardLayouts
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

myWS1 = "1:main"
myWS2 = "2:www"
myWS3 = "3:irssi"
myWS4 = "4:mp3"
myWS5 = "5:gimp"
myWS6 = "6:oof"
myWS7 = "7:games"
myWS8 = "8:wine"
myWS9 = "9"

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
--    , [ isFullscreen --> doFullFloat ]
    , [ isFullscreen --> (doF W.focusDown <+> doFullFloat) ]
    , [ className =? myIBrowser <&&> fmap ( c `isInfixOf`) resource --> doFloat | c <- myIBrowserFloat ]
    , [ resource  =? "desktop_window" --> doIgnore ]
    , [ resource  =? "kdesktop"       --> doIgnore ]
    , [ fmap ( c `isInfixOf`) className --> doFloat | c <- myMatchAnywhereFloatsC ]
    , [ fmap ( c `isInfixOf`) title     --> doFloat | c <- myMatchAnywhereFloatsT ]
    , [ fmap ( c `isInfixOf`) className --> doCenterFloat | c <- myMatchCenterFloatsC ]
    ]

  where myMatchAnywhereFloatsC = ["Google", "Pidgin", "Pavucontrol", "MPlayer", "Gpicview"]
        myMatchCenterFloatsC = ["feh", "Xmessage", "Squeeze", "GQview"]
        myMatchAnywhereFloatsT = ["VLC", "vlc"]

        myIBrowserFloat = ["Dialog", "Extension", "Browser", "Downloads"]
        myIBrowser = "Firefox"
        myWS1ShiftC = []
        myWS2ShiftC = ["Firefox", "Namoroka", "Chrome"]
        myWS3ShiftC = ["Pidgin", "Mangler"]
        myWS4ShiftC = ["Spotify", "Quodlibet"]
        myWS5ShiftC = ["Gimp"]
        myWS6ShiftC = ["OpenOffice.org 3.1"]
        myWS7ShiftC = ["Heroes of Newerth"]
        myWS8ShiftC = ["Wine"]
        myWS9ShiftC = []
        myIgnores = ["trayer"]


myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- See the 'DynamicLog' extension for examples.
-- To emulate dwm's status bar logHook = dynamicLogDzen

myLogHook xmobar1 = dynamicLogWithPP $ defaultPP
                     { ppOutput = hPutStrLn xmobar1
                     , ppCurrent = xmobarColor "orange" ""
                     , ppTitle = xmobarColor "green" "" . shorten 200
                     , ppUrgent = xmobarColor "white" "red"
                     , ppVisible = xmobarColor "white" ""
                  -- , ppHidden = wrap "(" ")"
                     , ppLayout = xmobarColor "grey" "" .
                     (\x -> case x of
                       "Full" -> "[ ]"
                       "ResizableTall" -> "[|]"
                       "Mirror ResizableTall" -> "[-]"
                       "Grid" -> "[+]"
                       "IM ReflectX IM Full" -> "[G]"
                       _ -> x
                     )
                     , ppSep = " - "
                     }

-- Perform an arbitrary action each time xmonad starts or is restarted with mod-q.  
-- Used by, e.g., XMonad.Layout.PerWorkspace to initialize per-workspace layout choices.

mushimStartupHook = do
                     return ()
armorStartupHook =  do
                     spawnOnce "xmobar -x 1 ~/.xmobarrc2" -- Spawn our second xmobar on monitor 1
		     return ()

main = do
          host <- fmap nodeName getSystemID
          xmobar1 <- spawnPipe "xmobar -x 0 ~/.xmobarrc"
          xmonad $ defaultConfig 
                      { terminal           = myTerminal
                      , focusFollowsMouse  = myFocusFollowsMouse
                      , borderWidth        = myBorderWidth
                      , modMask            = myModMask
                      , workspaces         = myWorkspaces
                      , normalBorderColor  = myNormalBorderColor
                      , focusedBorderColor = myFocusedBorderColor
                      , keys               = (if host == "mushim" then
                                               mushimKeys
                                             else
                                               armorKeys)
                      , mouseBindings      = myMouseBindings
                      , layoutHook         = myLayout
                      , manageHook         = myManageHook <+> manageDocks
                      , startupHook        = (if host == "mushim" then
                                               mushimStartupHook
                                             else
                                               armorStartupHook)
                      , logHook            = myLogHook xmobar1
                     }
