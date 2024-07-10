-----------------------------------------------------------------
--------------------------------------------------------------------------------
--- Imports
--------------------------------------------------------------------------------
-----------------------------------------------------------------


import XMonad
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Util.NamedScratchpad
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.ToggleLayouts
-- import XMonad.Layout.Fullscreen
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName

import qualified XMonad.Actions.Search as S
import XMonad.Hooks.OnPropertyChange
import XMonad.Actions.RotSlaves
import XMonad.Actions.CycleWS
import XMonad.Actions.SpawnOn
import XMonad.Actions.Submap
-- import XMonad.Hooks.ManageHelpers
-- import XMonad.Hooks.EwmhDesktops
import XMonad.ManageHook
import XMonad.Prompt

import Control.Arrow (first)

import Data.Monoid
import Data.Maybe
import System.Exit
import Data.List

import Graphics.X11.ExtraTypes.XF86

import qualified XMonad.Layout.IndependentScreens as LIS
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified XMonad.Util.Hacks as Hacks

-----------------------------------------------------------------
--------------------------------------------------------------------------------
--- Variables
--------------------------------------------------------------------------------
-----------------------------------------------------------------


myTerminal      = "alacritty"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
myBorderWidth :: Dimension
myBorderWidth   = 2

-- action key
myModMask       = mod4Mask

-- workspaces list
myWorkspaces    = [" \58968 "," \58930 "," \57879  ","4","5","6","7","8","9"]

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "Catia (L)" spawnCatiaL findCatiaL manageCatiaL,
                  NS "Cadence (R)" spawnCadenceR findCadenceR manageCadenceR,
                  NS "Terminal" spawnTerm findTerm manageTerm]
  where
    spawnCatiaL = "catia"
    findCatiaL = title =? "Catia"
    manageCatiaL = customFloating $ W.RationalRect 0.02 0.09 0.47 0.87
    spawnCadenceR = "cadence"
    findCadenceR = title =? "Cadence"
    manageCadenceR = customFloating $ W.RationalRect 0.51 0.09 0.47 0.87
    spawnTerm = "alacritty -t scratchpad"
    findTerm = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect 0.02 0.09 0.96 0.87

myFont :: String
myFont = "xft:SauceCodePro Nerd Font Mono:regular:size=17:antialias=true:hinting=true"

myXPKeymap :: M.Map (KeyMask,KeySym) (XP ())
myXPKeymap = M.fromList $
     map (first $ (,) controlMask)      -- control + <key>
     [ (xK_p, pasteString)
     , (xK_h, moveCursor Prev)
     , (xK_l, moveCursor Next)
     , (xK_k, moveHistory W.focusDown')
     , (xK_j, moveHistory W.focusUp')
     , (xK_BackSpace, killWord Prev)
     ]
     ++
     map (first $ (,) 0) -- <key>
     [ (xK_Return, setSuccess True >> setDone True)
     , (xK_KP_Enter, setSuccess True >> setDone True)
     , (xK_BackSpace, deleteString Prev)
     , (xK_Delete, deleteString Next)
     , (xK_Left, moveCursor Prev)
     , (xK_Right, moveCursor Next)
     , (xK_Home, startOfLine)
     , (xK_End, endOfLine)
     , (xK_Down, moveHistory W.focusUp')
     , (xK_Up, moveHistory W.focusDown')
     , (xK_Escape, quit)
     ]


myXPConfig :: XPConfig
myXPConfig = def
      { font                = myFont
      , bgColor             = "#282c34"
      , fgColor             = "#bbc2cf"
      , bgHLight            = "#c792ea"
      , fgHLight            = "#000000"
      , borderColor         = "#535974"
      , promptBorderWidth   = 0
      , promptKeymap        = myXPKeymap
      -- , position            = Top
      , position            = CenteredAt { xpCenterY = 0.3, xpWidth = 0.3 }
      , height              = 40
      , historySize         = 256
      , historyFilter       = id
      , defaultText         = []
      , autoComplete        = Nothing -- Just 100000  -- set Just 100000 for .1 sec
      , showCompletionOnTab = False
      -- , searchPredicate     = isPrefixOf
      -- , searchPredicate     = fuzzyMatch
      -- , defaultPrompter     = id $ map toUpper  -- change prompt to UPPER
      -- , defaultPrompter     = unwords . map reverse . words  -- reverse the prompt
      -- , defaultPrompter     = drop 5 .id (++ "XXXX: ")  -- drop first 5 chars of prompt and add XXXX:
      , alwaysHighlight     = True
      , maxComplRows        = Just 0      -- set to 'Just 5' for 5 rows
      }


-----------------------------------------------------------------
--------------------------------------------------------------------------------
--- KeyBindings
--------------------------------------------------------------------------------
-----------------------------------------------------------------


myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm,               xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_d     ), spawn "rofi -show drun -show-icons -kb-accept-entry 'Return' -kb-row-down 'Control+j' -kb-remove-to-eol '' -kb-row-up 'Control+k'") -- "dmenu_run")

    -- browse query in firefox
    , ((modm,               xK_b     ), S.promptSearch myXPConfig (S.searchEngine "google" "https://google.com/search?q="))

    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")

    -- close focused window
    , ((modm .|. shiftMask, xK_q     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    -- , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- fullscreen
    , ((modm              , xK_f), sendMessage (Toggle "Full"))

    -- sleep
    , ((modm              , xK_s), spawn "systemctl suspend")

    -- rotate all
    , ((modm              , xK_c), rotAllDown )
    , ((modm .|. shiftMask, xK_c), rotAllUp )

    -- next screen
    , ((modm              , xK_Tab), nextScreen )

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_e    ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm .|. shiftMask, xK_r    ), spawn "xmonad --recompile && xmonad --restart")

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_z, xK_x] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++
    -- addidional Keys
    --
    [ ((0                     , 0x1008ff11), spawn "pactl set-sink-volume 3 -5%"), -- amixer -q sset Master 2%-
      ((0                     , 0x1008ff13), spawn "pactl set-sink-volume 3 +5%"), -- amixer -q sset Master 2%+
      ((0                     , 0x1008ff12), spawn "pactl set-sink-mute 3 toggle"),
      ((0		      , xF86XK_MonBrightnessUp), spawn "xbacklight -inc 10"),
      ((0		      , xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10"),
      ((0                     , xK_Print), spawn "flameshot gui")
    ]
    ++
    -- fast open
    [((modm                     , xK_o), submap . M.fromList $
     [
       ((0,              xK_e), spawn "emacsclient -r -n -a 'emacs' "),
       ((0,              xK_f), spawn "firefox"),
       ((0,              xK_n), spawn "nautilus"),
       ((0,              xK_a), namedScratchpadAction myScratchPads "Catia (L)" <+> namedScratchpadAction myScratchPads "Cadence (R)"),
       ((0,              xK_t), namedScratchpadAction myScratchPads "Terminal")
     ])]
-----------------------------------------------------------------
--------------------------------------------------------------------------------
--- MouseBindings
--------------------------------------------------------------------------------
-----------------------------------------------------------------


myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]


-----------------------------------------------------------------
--------------------------------------------------------------------------------
--- Layouts
--------------------------------------------------------------------------------
-----------------------------------------------------------------


-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.

myLayout = toggleLayouts
        (noBorders Full)
        ( gaps [(U, 50)] $ smartBorders . spacingWithEdge 10 $ tiled ||| Mirror tiled)


  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100


-----------------------------------------------------------------
--------------------------------------------------------------------------------
--- WindowRules
--------------------------------------------------------------------------------
-----------------------------------------------------------------


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
myManageHook = manageSpawn <+> composeAll
    [ manageDocks
    , manageHook def
    , className =? "Emacs" --> doShift ( myWorkspaces !! 1 )
    , className =? "firefox" --> doShift ( myWorkspaces !! 0 )
    , className =? "Telegram" --> doShift ( myWorkspaces !! 2 )
    , className =? "Qalculate-gtk" --> doSideFloat C
    -- , isFullscreen --> doFullFloat
    ] <+> namedScratchpadManageHook myScratchPads


-----------------------------------------------------------------
--------------------------------------------------------------------------------
--- Event handling
--------------------------------------------------------------------------------
-----------------------------------------------------------------


-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.

myEventHook = Hacks.trayerAboveXmobarEventHook
  --mempty
  --XMonad.Hooks.EwmhDesktops.fullscreenEventHook


-----------------------------------------------------------------
--------------------------------------------------------------------------------
--- Status bars and logging
--------------------------------------------------------------------------------
-----------------------------------------------------------------


-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.

-- myLogHook =

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..]
clickable :: [Char] -> [Char]
clickable ws = "<action=xdotool key super+"++ show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices


-----------------------------------------------------------------
--------------------------------------------------------------------------------
--- Startup hook
----------------------------------------------------------------------
-----------------------------------------------------------------


-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.

myStartupHookOneScreen :: X()
myStartupHookOneScreen = do
            spawnOnce "nitrogen --restore"
            spawn "sleep 2 && trayer -l --edge top --align right --margin 22 --distance 15 --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --transparent true --alpha 0 --tint 0x292929 --heighttype pixel --height 30"

myStartupHookTwoScreens :: X()
myStartupHookTwoScreens = do
            spawnOnce "nitrogen --restore"
            spawn "xrandr --output HDMI-1-0 --mode 1920x1080"
            spawn "xrandr --output eDP-1 --primary"
            spawn "xrandr --output HDMI-1-0 --above eDP-1"
            -- spawn "xrandr --output HDMI-1-0 --scale 0.66x0.66"
            -- spawn "sleep 2 && trayer -l --edge bottom --align right --widthtype request --monitor 1 --padding 6 --SetDockType true --SetPartialStrut true --expand true --transparent true --alpha 0 --tint 0x000000 --heighttype pixel --height 30"
            spawn "sleep 2 && trayer -l --edge top --align right --margin 22 --distance 15  --widthtype request --monitor 1 --padding 6 --SetDockType true --SetPartialStrut true --expand true --transparent true --alpha 0 --tint 0x000000 --heighttype pixel --height 30"

myStartupHook :: X()
myStartupHook = do
            setWMName "LG3D"
            screencount <- LIS.countScreens
            if screencount > 1
                then myStartupHookTwoScreens
                else myStartupHookOneScreen
            -- Common stuff
            spawn "killall trayer"
            spawn "picom &"
            spawn "flameshot &"
            -- spawn "setxkbmap -model pc104 -layout us,ru -option grp:caps_toggle"
            spawn "setxkbmap -option ctrl:nocaps"
            -- spawn "killall fcitx5"
            spawnOnce "fcitx5 -d"
            spawnOnce "xset s off"
            spawnOnce "xset -dpms"
            spawnOnce "nm-applet"
            spawn "syndaemon -i 1 -K -R -d"
            spawnOnce "pulseaudio --start"
            spawn "/usr/bin/emacs --daemon"
            -- spawnOnce ""

----------------------------------------------------------------------
--------------------------------------------------------------------------------
--- MainSection (do not modify)
--------------------------------------------------------------------------------
----------------------------------------------------------------------


-- Now run xmonad with all the defaults we set up.
-- Run xmonad with the settings you specify. No need to modify this.

main :: IO()
main = do
  -- before startup hook to proper bottom-bar rendering
  spawn "killall xmobar"
  spawn "xrandr --auto"
  screencount <- LIS.countScreens
  xmprocs <- if screencount == 1
    then mapM (\i -> spawnPipe ("sleep 2 && xmobar -x " ++ show i ++ " /home/contramund/.config/xmobar/xmobar.config")) [0 .. screencount-1]
    else mapM (\i -> spawnPipe ("sleep 2 && xmobar -x " ++ show i ++ " /home/contramund/.config/xmobar/xmobar" ++ show i ++ ".config")) [0 .. screencount-1]
  xmonad $ ewmhFullscreen . ewmh $ docks def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        startupHook        = myStartupHook,
        logHook            =
            dynamicLogWithPP $ filterOutWsPP [scratchpadWorkspaceTag] $ xmobarPP {
                ppOutput = \x -> foldl (\a b -> a >> hPutStrLn b x) mempty xmprocs
                              -- >> hPutStrLn xmproc1 x
                , ppCurrent = xmobarColor "#77ff00" "" . wrap "[" "]"
                -- Visible but not current workspace
                , ppVisible = xmobarColor "#77ff00" "" . wrap "<" ">" . clickable
                -- Hidden workspace
                , ppHidden = xmobarColor "#ff00ff" "" . clickable
                --             ("<box type=Top width=2 mt=2 color=" ++ "#ff00ff" ++ ">") "</box>" . clickable
                -- Hidden workspaces (no windows)
                , ppHiddenNoWindows = xmobarColor "#770077" "" . clickable
                -- Title of active window
                , ppTitle = xmobarColor "#ffff00" "" . shorten 30
                -- Separator character
                , ppSep =  "<fc=" ++ "#ffffff" ++ "> <fn=0>|</fn> </fc>"
                -- Urgent workspace
                -- , ppUrgent = xmobarColor "#00ff00" "" . wrap "!" "!"
                -- Adding # of windows on current workspace to the bar
                -- , ppExtras  = [windowCount]
                -- order of things in xmobar
                , ppOrder  = \(ws:_:t:_) -> [ws]++[t]
            }

    }

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
