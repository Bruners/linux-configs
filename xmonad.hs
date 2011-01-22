
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
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Tabbed
import XMonad.Util.Themes

-- Prompt.Shell replacement for dmenu
import XMonad.Prompt
import XMonad.Prompt.Shell

-- Switch desktops
import XMonad.Actions.CycleWS

-- Scratchpad
import XMonad.Util.Scratchpad

-- Dzen
import Dzen
import XMonad.Util.Run (hPutStrLn, spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Hooks.DynamicLog hiding (dzen)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.Place
import Data.List

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import System.IO

myTerminal = "urxvtc"
myBorderWidth   = 1

myModMask       = mod4Mask
myNumlockMask   = mod2Mask
myWorkspaces = [ myWS1, myWS2, myWS3, myWS4, myWS5, myWS6, myWS7, myWS8, myWS9, myWS10 ]

myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#000000"
myDefaultGaps = [(18,0,0,0)]
myDmenu = "exe=`dmenu_path | dmenu -nb \"#000000\" -nf \"#ffffff\" -fn \"terminus-8\"` && eval \"exec $exe\""
-- use xev to fin key codes
armorKeys conf@(XConfig {XMonad.modMask = mM}) = M.fromList $
    [ ((mM .|. sM , xK_Return ), spawn $ XMonad.terminal conf      ) -- Lanch a terminal
    , ((mM        , xK_p      ), shellPrompt defaultXPConfig       )  -- Launch shellPromt
    , ((mM .|. sM , xK_p      ), spawn myDmenu                     )  -- Launch dmenu
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
    , ((mM        , xK_q      ), myRestart                         ) -- Restart xmonad
    , ((mM        , xK_Print  ), spawn "run_scrot scr"             ) -- Screenshot screen
    , ((mM .|. sM , xK_Print  ), spawn "run_scrot win"             ) -- Screenshot window or area
    , ((mM        , xK_Left   ), prevWS                            ) -- Cycle previous WS
    , ((mM        , xK_Right  ), nextWS                            ) -- Cycle to next WS
    , ((mM .|. sM , xK_Left   ), shiftToPrev                       ) -- Move WS to previous
    , ((mM .|. sM , xK_Right  ), shiftToNext                       ) -- Move WS next WS
    , ((0         , 0x1008ff11), spawn "~/.bin/oss4_hd_ctl -d 2"   ) -- Reduce volume
    , ((0         , 0x1008ff13), spawn "~/.bin/oss4_hd_ctl -i 2"   ) -- Raise volume
    , ((0         , 0x1008ff12), spawn "~/.bin/oss4_hd_ctl -t"     ) -- Mute volume
    , ((mM        , 0x1008ff11), spawn "~/.bin/oss4_sb_ctl -d 2"   ) -- Reduce volume
    , ((mM        , 0x1008ff13), spawn "~/.bin/oss4_sb_ctl -i 2"   ) -- Raise volume
    , ((mM        , 0x1008ff12), spawn "~/.bin/oss4_sb_ctl -t"     ) -- Mute volume
    , ((mM .|. sM , xK_l      ), spawn "xscreensaver-command -lock") -- Lock screen
    , ((mM        , xK_b      ), sendMessage ToggleStruts          ) -- toggle xmobar gap
    , ((mM        , xK_f      ), spawn "pcmanfm"                   ) -- Start pcmanfm
    , ((cM        , xK_Tab    ), sP                                ) -- Spawn a scratchpad terminal
    , ((mM        , xK_0      ), windows $ W.view myWS10           ) -- Move to workspace 10
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
           sP = scratchpadSpawnActionTerminal "urxvtc -background '#303030'"
           myRestart = spawn $ "for pid in `pgrep conky`; do kill -9 $pid; done && " ++
                               "for pid in `pgrep dzen2`; do kill -9 $pid; done && " ++
                               "xmonad --recompile && xmonad --restart"

mushimKeys conf@(XConfig {XMonad.modMask = mM}) = M.fromList $
    [ ((mM .|. sM , xK_Return ), spawn $ XMonad.terminal conf      ) -- Lanch a terminal
    , ((mM        , xK_p      ), spawn myDmenu                     ) -- Launch dmenu
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
        | (i, k) <- zip (XMonad.workspaces conf) [xK_0 .. xK_9]
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

--
-- Color config for the tabbed layout
--

myTabConfig = defaultTheme { activeColor         = _fg_color
                           , activeBorderColor   = _fg_color
                           , activeTextColor     = _bg_color
                           , inactiveColor       = _bg_color
                           , inactiveBorderColor = _bg_color
                           , inactiveTextColor   = "#ee9a00"
                           , decoHeight          = 13
                           }

myLayout = avoidStruts $ onWorkspace myWS3 irc $ 
                         onWorkspace myWS6 (gimp ||| standardLayouts) $ 
                         onWorkspace myWS7 full $ 
                         onWorkspace myWS2 (tabbed shrinkText myTabConfig ||| standardLayouts) $
                         standardLayouts
  where
     standardLayouts = avoidStruts $ (tiled ||| Mirror tiled ||| threeCol ||| tabbed shrinkText myTabConfig ||| Grid ||| full)

     tiled = smartBorders (ResizableTall 1 (2/100) (1/2) [])
     irc   = reflectHoriz $ withIM (0.15) (ClassName "Pidgin") $ reflectHoriz $ standardLayouts
     threeCol = ThreeCol 1 (3/100) (1/2) ||| ThreeColMid 1 (3/100) (1/2)
     gimp  = withIM (0.11) (Role "gimp-toolbox") $
             reflectHoriz $
             withIM (0.15) (Role "gimp-dock") Full
     full = noBorders Full
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100

-- 
-- Workspace variables for easy renaming
--

myWS0  = "0:p2p"
myWS1  = "1:main"
myWS2  = "2:www"
myWS3  = "3:irssi"
myWS4  = "4:code"
myWS5  = "5:mp3"
myWS6  = "6:gimp"
myWS7  = "7:oof"
myWS8  = "8:games"
myWS9  = "9:wine"
myWS10 = "0:p2p"


-- To find the property name associated with a program, use xprop | grep WM_CLASS
-- To match on the WM_NAME, you can use 'title' in the same way that 'className' and 'resource' are used below.

myManageHook = composeAll . concat $
    [ [ fmap ( c `isInfixOf`) className <||> fmap ( c `isInfixOf`) title --> doShift myW | (myW,  cs) <- myWSShift, c <- cs ]
    , [ isFullscreen --> (doF W.focusDown <+> doFullFloat) ]
    , [ classNotRole (cnf) --> doCenterFloat | (cnf) <- windowFloats ]
    , [ resource  =? "desktop_window" --> doIgnore ]
    , [ resource  =? "idesk" --> doIgnore ]
    , [ fmap ( c `isInfixOf`) className <||> fmap ( c `isInfixOf`) title --> doFloat | c <- myMatchAnywhereFloats ]
    , [ fmap ( c `isInfixOf`) className <||> fmap ( c `isInfixOf`) title --> doCenterFloat | c <- myMatchCenterFloats ]
    ]

  where myMatchAnywhereFloats = ["Google", "Pavucontrol", "MPlayer", "Gpicview", "Vlc", "File-roller", "Brasero", "Gnomebaker"]
        myMatchCenterFloats = ["feh", "Xmessage", "Squeeze", "GQview", "Thunar", "Pcmanfm"]
        classNotRole (c,r) = className =? c <&&> (stringProperty "WM_WINDOW_ROLE") /=? r
        windowFloats = [ ("Firefox", "browser")
                       , ("Pidgin", "buddy_list")
                       ]
        myWSShift = [ (myWS1, [])
                     , (myWS2, ["Firefox", "Namoroka", "Chrome"])
                     , (myWS3, ["IRC", "Pidgin", "Mangler"])
                     , (myWS4, [])
		     , (myWS5, ["Spotify", "Quodlibet"])
                     , (myWS6, ["Gimp"])
                     , (myWS7, ["OpenOffice.org 3.2"])
                     , (myWS8, ["Heroes of Newerth"])
                     , (myWS9, ["Wine"])
                     ]

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- See the 'DynamicLog' extension for examples.
-- To emulate dwm's status bar logHook = dynamicLogDzen

_fg_color = "#909090"
_bg_color = "#303030"
_hd_color = "#606060"
_or_color = "#ee9a00"

myLogHook dzen1 = dynamicLogWithPP $ defaultPP
                     { ppCurrent         = dzenColor "white"   "brown" . pad
                     , ppHidden          = dzenColor _fg_color "" . pad . noScratchPad
                     , ppHiddenNoWindows = dzenColor _hd_color "" . pad . noScratchPad
                     , ppVisible         = dzenColor "white"   "" . pad
                     , ppUrgent          = dzenColor "white"   _or_color . pad . dzenStrip
                     , ppTitle           = dzenColor "green"   "" . shorten 100 . pad
                     , ppWsSep           = ""
                     , ppSep             = " "
                     , ppLayout          = dzenColor _fg_color "" .
                     (\x -> case x of
                       "Full" -> "[ ]"
                       "ResizableTall" -> "[|]"
                       "Mirror ResizableTall" -> "[-]"
                       "Grid" -> "[+]"
                       "IM ReflectX IM Full" -> "[G]"
                       "ReflectX IM ReflectX ResizableTall" -> "[@]"
                       "Tabbed Simplest" -> "[\"]"
                       "ThreeCol" -> "[3]"
                       _ -> x
                     )
                     , ppOutput            = hPutStrLn dzen1
                     }
                     where
                       noScratchPad ws = if ws == "NSP" then "" else ws

-- StatusBars

myLeftBar1 :: DzenConf
myLeftBar1 = defaultDzen
    -- use the default as a base and override width and colors
    { width       = 960
    , fg_color    = _fg_color
    , bg_color    = _bg_color
    }

myLeftBar2 :: DzenConf
myLeftBar2 = myLeftBar1
    { x_position  = 960
    , width       = 960
    , alignment   = RightAlign
    }

myRightBar1 :: DzenConf
myRightBar1 = myLeftBar2
    -- use the left one as a base and override just the x position and width
    { x_position = 1920
    , width      = 1769
    , alignment  = RightAlign
    }


-- Scratchpad

manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)

  where

    h = 0.1     -- terminal height, 10%
    w = 1       -- terminal width, 100%
    t = 200     -- distance from top edge, 90%
    l = 200     -- distance from left edge, 0%

-- Perform an arbitrary action each time xmonad starts or is restarted with mod-q.  
-- Used by, e.g., XMonad.Layout.PerWorkspace to initialize per-workspace layout choices.

mushimStartupHook = do
                     return ()
armorStartupHook =  do
		     return ()
main :: IO ()
main = do
          host <- fmap nodeName getSystemID
          dzen1 <- spawnDzen myLeftBar1
          spawn $ "conky -c /home/lasseb/.xmonad/dzen_left2 | " ++ dzen myLeftBar2
          spawn $ "conky -c /home/lasseb/.xmonad/dzen_right1 | " ++ dzen myRightBar1
          xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig 
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
                      , manageHook         = placeHook simpleSmart <+> myManageHook <+> manageDocks <+> manageScratchPad
                      , startupHook        = (if host == "mushim" then
                                               mushimStartupHook
                                             else
                                               armorStartupHook)
                      , logHook            = myLogHook dzen1
                     }
