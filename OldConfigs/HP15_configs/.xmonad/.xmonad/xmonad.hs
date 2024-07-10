-----------------------------------------------------------------
--------------------------------------------------------------------------------
--- Imports
--------------------------------------------------------------------------------
-----------------------------------------------------------------


import XMonad
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.ToggleLayouts
-- import XMonad.Layout.Fullscreen
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Actions.RotSlaves
import XMonad.Actions.CycleWS
import XMonad.Actions.SpawnOn
-- import XMonad.Hooks.ManageHelpers
-- import XMonad.Hooks.EwmhDesktops
import Data.Monoid
import Data.Maybe
import System.Exit
import Data.List

import qualified XMonad.Layout.IndependentScreens as LIS
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-----------------------------------------------------------------
--------------------------------------------------------------------------------
--- Variables
--------------------------------------------------------------------------------
-----------------------------------------------------------------


myTerminal      = "alacritty"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
myBorderWidth :: Dimension
myBorderWidth   = 2

-- action key
myModMask       = mod4Mask

-- workspaces list
myWorkspaces    = ["web","emacs","3","4","5","6","7","8","9"]

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"


-----------------------------------------------------------------
--------------------------------------------------------------------------------
--- KeyBindings
--------------------------------------------------------------------------------
-----------------------------------------------------------------


myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm,               xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_d     ), spawn "dmenu_run")

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
    , ((modm              , xK_n), nextScreen )

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
    --addidional Keys
    --
    [ ((0                     , 0x1008ff11), spawn "amixer -q sset Master 2%-"),
      ((0                     , 0x1008ff13), spawn "amixer -q sset Master 2%+"),
      ((0                     , 0x1008ff12), spawn "amixer -D pulse set Master toggle"),
      ((0                     , xK_Print), spawn "flameshot gui")
    ]

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
        ( avoidStruts $ spacingWithEdge 10 $ tiled ||| Mirror tiled)


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
    , manageHook defaultConfig
    , className =? "Emacs" --> doShift ( myWorkspaces !! 1 )
    , className =? "firefox" --> doShift ( myWorkspaces !! 0 )
    -- , isFullscreen --> doFullFloat
    ]


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

myEventHook = mempty
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
            spawn "sleep 2 && trayer --edge bottom --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --transparent true --alpha 0 --tint 0x000000 --heighttype pixel --height 30"

myStartupHookTwoScreens :: X()
myStartupHookTwoScreens = do
            spawnOnce "nitrogen --restore"
            spawn "xrandr --output HDMI-1 --mode 1280x1024"
            spawn "xrandr --output eDP-1 --primary"
            spawn "xrandr --output HDMI-1 --left-of eDP-1"
            spawn "sleep 2 && trayer --edge bottom --align right --widthtype request --monitor 1 --padding 6 --SetDockType true --SetPartialStrut true --expand true --transparent true --alpha 0 --tint 0x000000 --heighttype pixel --height 30"

myStartupHook :: X()
myStartupHook = do
            screencount <- LIS.countScreens
            if screencount > 1
                then myStartupHookTwoScreens
                else myStartupHookOneScreen
            -- Common stuff
            spawn "killall trayer"
            spawn "picom &"
            spawn "flameshot &"
            spawn "setxkbmap -model pc104 -layout us,ru -option grp:caps_toggle"
            spawnOnce "xset s off"
            spawnOnce "xset -dpms"
            spawnOnce "nm-applet"
            spawnOnce "syndaemon -i 1 -K -d"
            spawnOnce "pulseaudio --start"
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
  xmprocs <-  sequence $ fmap (\i -> spawnPipe ("sleep 2 && xmobar -x " ++ show i ++ " /home/contramund/.config/xmobar/xmobar.config")) [0 .. screencount-1]
  xmonad $ docks def {
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
            dynamicLogWithPP xmobarPP {
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
