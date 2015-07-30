{-
  This is my xmonad configuration file.
  There are many like it, but this one is mine.

  If you want to customize this file, the easiest workflow goes
  something like this:
    1. Make a small change.
    2. Hit "super-q", which recompiles and restarts xmonad
    3. If there is an error, undo your change and hit "super-q" again to
       get to a stable place again.
    4. Repeat

  Author:     David Brewer
  Repository: https://github.com/davidbrewer/xmonad-ubuntu-conf
-}

import XMonad
import XMonad.Hooks.SetWMName
import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile
import XMonad.Layout.IM
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.Circle
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Fullscreen
import XMonad.Layout.Tabbed
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare
import XMonad.Hooks.DynamicLog
import XMonad.Actions.Plane
import XMonad.Actions.CycleWS
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ICCCMFocus
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.Ratio ((%))
import Data.List

{-
  Xmonad configuration variables. These settings control some of the
  simpler parts of xmonad's behavior and are straightforward to tweak.
-}

------------------------------------------------------------------
------
-- Terminal
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal = "terminal"


------------------------------------------------------------------
------
-- Workspaces
-- The default number of workspaces (virtual screens) and their names.
--
myWorkspaces = ["1:web","2:code","3:term","4:run","5:vm"] ++ map show [6..9]
myws n = myWorkspaces !! (n-1)

startupWorkspace = "5:Dev"  -- which workspace do you want to be on after launch?


------------------------------------------------------------------
--------
---- Colors and borders
--

myFocusedBorderColor = "#ff0000"      -- color of focused border
myNormalBorderColor  = "#cccccc"      -- color of inactive border
myBorderWidth        = 1              -- width of border around windows
myIMRosterTitle      = "Buddy List"   -- title of roster on IM workspace
                                      -- use "Buddy List" for Pidgin, but
                                      -- "Contact List" for Empathy


-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
tabConfig = defaultTheme {
    activeBorderColor = "#7C7C7C",
    activeTextColor = "#CEFFAC",
    activeColor = "#000000",
    inactiveBorderColor = "#7C7C7C",
    inactiveTextColor = "#EEEEEE",
    inactiveColor = "#000000"
}

{-
  Xmobar configuration variables. These settings control the appearance
  of text which xmonad is sending to xmobar via the DynamicLog hook.
-}

myTitleColor     = "#8888ee"  -- color of window title
myTitleLength    = 80         -- truncate window title to this length
myCurrentWSColor = "#f6242c"  -- color of active workspace
myVisibleWSColor = "#41b522"  -- color of inactive workspace
myUrgentWSColor  = "#cccc00"  -- color of workspace with 'urgent' window
myHiddenWSColor  = "#555555"  -- color of hidden workspaces
myCurrentWSLeft  = "["        -- wrap active workspace with these
myCurrentWSRight = "]"
myVisibleWSLeft  = "("        -- wrap inactive workspace with these
myVisibleWSRight = ")"
myUrgentWSLeft  = "{"         -- wrap urgent workspace with these
myUrgentWSRight = "}"



{-
  Layout configuration. In this section we identify which xmonad
  layouts we want to use. I have defined a list of default
  layouts which are applied on every workspace, as well as
  special layouts which get applied to specific workspaces.

  Note that all layouts are wrapped within "avoidStruts". What this does
  is make the layouts avoid the status bar area at the top of the screen.
  Without this, they would overlap the bar. You can toggle this behavior
  by hitting "super-b" (bound to ToggleStruts in the keyboard bindings
  in the next section).
-}

-- Define group of default layouts used on most screens, in the
-- order they will appear.
-- "smartBorders" modifier makes it so the borders on windows only
-- appear if there is more than one visible window.
-- "avoidStruts" modifier makes it so that the layout provides
-- space for the status bar at the top of the screen.
defaultLayouts = smartBorders(avoidStruts(
  -- ResizableTall layout has a large master window on the left,
  -- and remaining windows tile on the right. By default each area
  -- takes up half the screen, but you can resize using "super-h" and
  -- "super-l".
  ResizableTall 1 (3/100) (1/2) []

  -- Mirrored variation of ResizableTall. In this layout, the large
  -- master window is at the top, and remaining windows tile at the
  -- bottom of the screen. Can be resized as described above.
  ||| Mirror (ResizableTall 1 (3/100) (1/2) [])

  -- ThreeColMid layout puts the large master window in the center
  -- of the screen. As configured below, by default it takes of 3/4 of
  -- the available space. Remaining windows tile to both the left and
  -- right of the master window. You can resize using "super-h" and
  -- "super-l".
  -- ||| ThreeColMid 1 (3/100) (3/4)

  -- Circle layout places the master window in the center of the screen.
  -- Remaining windows appear in a circle around it
  -- ||| Circle

  -- Grid layout tries to equally distribute windows in the available
  -- space, increasing the number of columns and rows as necessary.
  -- Master window is at top left.
  ||| Grid

  -- Tabbed
  ||| tabbed shrinkText tabConfig
  
  -- Full layout makes every window full screen. When you toggle the
  -- active window, it will bring the active window to the front.
  -- ||| noBorders Full
  ||| Full

  ))


-- Here we define some layouts which will be assigned to specific
-- workspaces based on the functionality of that workspace.

-- The chat layout uses the "IM" layout. We have a roster which takes
-- up 1/8 of the screen vertically, and the remaining space contains
-- chat windows which are tiled using the grid layout. The roster is
-- identified using the myIMRosterTitle variable, and by default is
-- configured for Pidgin, so if you're using something else you
-- will want to modify that variable.
-- chatLayout = avoidStruts(withIM (1%7) (Title myIMRosterTitle) Grid)

-- The GIMP layout uses the ThreeColMid layout. The traditional GIMP
-- floating panels approach is a bit of a challenge to handle with xmonad;
-- I find the best solution is to make the image you are working on the
-- master area, and then use this ThreeColMid layout to make the panels
-- tile to the left and right of the image. If you use GIMP 2.8, you
-- can use single-window mode and avoid this issue.
-- gimpLayout = smartBorders(avoidStruts(ThreeColMid 1 (3/100) (3/4)))

-- Here we combine our default layouts with our specific, workspace-locked
-- layouts.
myLayouts = defaultLayouts
  -- onWorkspace "7:Chat" chatLayout
  -- $ onWorkspace "9:Pix" gimpLayout
  -- $ defaultLayouts




{-
  Management hooks. You can use management hooks to enforce certain
  behaviors when specific programs or windows are launched. This is
  useful if you want certain windows to not be managed by xmonad,
  or sent to a specific workspace, or otherwise handled in a special
  way.

  Each entry within the list of hooks defines a way to identify a
  window (before the arrow), and then how that window should be treated
  (after the arrow).

  To figure out to identify your window, you will need to use a
  command-line tool called "xprop". When you run xprop, your cursor
  will temporarily change to crosshairs; click on the window you
  want to identify. In the output that is printed in your terminal,
  look for a couple of things:
    - WM_CLASS(STRING): values in this list of strings can be compared
      to "className" to match windows.
    - WM_NAME(STRING): this value can be compared to "resource" to match
      windows.

  The className values tend to be generic, and might match any window or
  dialog owned by a particular program. The resource values tend to be
  more specific, and will be different for every dialog. Sometimes you
  might want to compare both className and resource, to make sure you
  are matching only a particular window which belongs to a specific
  program.

  Once you've pinpointed the window you want to manipulate, here are
  a few examples of things you might do with that window:
    - doIgnore: this tells xmonad to completely ignore the window. It will
      not be tiled or floated. Useful for things like launchers and
      trays.
    - doFloat: this tells xmonad to float the window rather than tiling
      it. Handy for things that pop up, take some input, and then go away,
      such as dialogs, calculators, and so on.
    - doF (W.shift "Workspace"): this tells xmonad that when this program
      is launched it should be sent to a specific workspace. Useful
      for keeping specific tasks on specific workspaces. In the example
      below I have specific workspaces for chat, development, and
      editing images.
-}

myManagementHooks :: [ManageHook]
myManagementHooks = [
    -- [ className =? "Chromium"       --> doShift "1:web"
    -- , className =? "Google-chrome"  --> doShift "1:web"
      resource  =? "desktop_window" --> doIgnore
    , className =? "Galculator"     --> doFloat
    , className =? "Steam"          --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "gpicview"       --> doFloat
    , className =? "MPlayer"        --> doFloat
    , className =? "VirtualBox"     --> doShift (myws 5)
    , fmap ("VirtualBox : 1" `isInfixOf`) resource --> doShift "8"
    , fmap ("VirtualBox : 2" `isInfixOf`) resource --> doShift "9"
    -- , resource  =? "TV [Running] - Oracle VM VirtualBox : 1" --> doShift "8"
    -- , resource  =? "TV [Running] - Oracle VM VirtualBox : 2" --> doShift "9"
    , className =? "Xchat"          --> doShift "6"
    , className =? "stalonetray"    --> doIgnore
    -- , isFullscreen --> (doF W.focusDown <+> doFullFloat)
    , resource  =? "synapse"        --> doIgnore
    , className =? "rdesktop"       --> doFloat
    ]

  -- , (className =? "Komodo IDE") --> doF (W.shift "5:Dev")
  -- , (className =? "Komodo IDE" <&&> resource =? "Komodo_find2") --> doFloat
  -- , (className =? "Komodo IDE" <&&> resource =? "Komodo_gotofile") --> doFloat
  -- , (className =? "Komodo IDE" <&&> resource =? "Toplevel") --> doFloat
  -- , (className =? "Empathy") --> doF (W.shift "7:Chat")
  -- , (className =? "Pidgin") --> doF (W.shift "7:Chat")
  -- , (className =? "Gimp-2.8") --> doF (W.shift "9:Pix")


{-
  Custom keybindings. In this section we define a list of relatively
  straightforward keybindings. This would be the clearest place to
  add your own keybindings, or change the keys we have defined
  for certain functions.

  It can be difficult to find a good list of keycodes for use
  in xmonad. I have found this page useful -- just look
  for entries beginning with "xK":

  http://xmonad.org/xmonad-docs/xmonad/doc-index-X.html

  Note that in the example below, the last three entries refer
  to nonstandard keys which do not have names assigned by
  xmonad. That's because they are the volume and mute keys
  on my laptop, a Lenovo W520.

  If you have special keys on your keyboard which you
  want to bind to specific actions, you can use the "xev"
  command-line tool to determine the code for a specific key.
  Launch the command, then type the key in question and watch
  the output.
-}

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.

myModMask = mod4Mask

setWS w e = do
    (Just wOn0) <- screenWorkspace 0
    windows $ W.view wOn0
    windows $ W.greedyView w
    (Just wOn1) <- screenWorkspace 1
    windows $ W.view wOn1
    windows $ W.greedyView e

myKeys =
  ----------------------------------------------------------------------
  -- Custom key bindings
  --

  -- Lock the screen using xscreensaver.
  [
    ((mod1Mask .|. controlMask, xK_l),
     spawn "xscreensaver-command -lock")
  -- , ((mod4Mask, xK_l),
  --    spawn "xscreensaver-command -lock")

  , ((controlMask .|. mod4Mask, xK_BackSpace),
     setWS (myws 1) (myws 2))

  , ((controlMask .|. mod4Mask, xK_Escape),
     setWS (myws 1) (myws 2))

  , ((controlMask .|. mod4Mask, xK_equal),
     setWS (myws 8) (myws 9))

  -- Launch dmenu via yeganesh.
  -- Use this to launch programs without a key binding.
  -- , ((myModMask, xK_p),
  --    spawn "~/.xmonad/dmenu-with-yeganesh")

  -- Focus on urgent window
  , ((myModMask, xK_u),
     focusUrgent)

  -- Take a screenshot in select mode.
  -- After pressing this key binding, click a window, or draw a rectangle with
  -- the mouse.
  , ((myModMask .|. shiftMask, xK_p),
     spawn "select-screenshot")

  -- Take full screenshot in multi-head mode.
  -- That is, take a screenshot of everything you see.
  , ((myModMask .|. controlMask .|. shiftMask, xK_p),
     spawn "screenshot")

  -- Fetch a single use password.
  , ((myModMask .|. shiftMask, xK_o),
     spawn "fetchotp -x")


  -- Mute volume.
  , ((myModMask .|. controlMask, xK_m),
     spawn "amixer -q set Master toggle")
  , ((0, 0x1008FF12),
     spawn "amixer -q set Master toggle")

  -- Decrease volume.
  , ((myModMask .|. controlMask, xK_j),
     spawn "amixer -q set Master 10%-")
  , ((0, 0x1008FF11),
     spawn "amixer -q set Master 10%-")

  -- Increase volume.
  , ((myModMask .|. controlMask, xK_k),
     spawn "amixer -q set Master 10%+")
  , ((0, 0x1008FF13),
     spawn "amixer -q set Master 10%+")

  -- Audio previous.
  , ((0, 0x1008FF16),
     spawn "")

  -- Play/pause.
  , ((0, 0x1008FF14),
     spawn "")

  -- Audio next.
  , ((0, 0x1008FF17),
     spawn "")

  -- Eject CD tray.
  , ((0, 0x1008FF2C),
     spawn "eject -T")

  --------------------------------------------------------------------
  -- "Standard" xmonad key bindings
  --

  -- Toggle the status bar gap.
  , ((myModMask, xK_b),
     sendMessage ToggleStruts)

  -- Shrink the master area vertically.
  , ((myModMask, xK_a),
     sendMessage MirrorShrink)

  -- Expand the master area vertically.
  , ((myModMask, xK_z),
     sendMessage MirrorExpand)

  -- Shrink the master area.
  , ((shiftMask .|. myModMask, xK_h),
     sendMessage Shrink)

  -- Expand the master area.
  , ((shiftMask .|. myModMask, xK_l),
     sendMessage Expand)

  -- Next non-empty non-visible WS
  , ((myModMask, xK_Right),
     moveTo Next HiddenNonEmptyWS)

  -- Prev non-empty non-visible WS
  , ((myModMask, xK_Left),
     moveTo Prev HiddenNonEmptyWS)
  ]
  ++
  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. myModMask, k), f i)
      | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
      , (f, m) <- [(toggleOrView, 0), (windows . W.shift, shiftMask)]]
  ++
  -- mod-{i,o}, Switch to physical/Xinerama screens 1, 2
  -- mod-shift-{i,o}, Move client to screen 1, 2
  [((m .|. myModMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_i, xK_o] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

------------------------------------------------------------------
------
-- Mouse bindings
--
-- Focus rules
-- True if your focus should follow your mouse cursor.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouse conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1),
     (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2),
       (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3),
       (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]


{-
  Here we actually stitch together all the configuration settings
  and run xmonad. We also spawn an instance of xmobar and pipe
  content into it via the logHook.
-}

main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
    focusedBorderColor = myFocusedBorderColor
  , normalBorderColor = myNormalBorderColor
  , terminal = myTerminal
  , focusFollowsMouse = myFocusFollowsMouse
  , borderWidth = myBorderWidth
  , layoutHook = myLayouts
  , workspaces = myWorkspaces
  , modMask = myModMask
  , handleEventHook = fullscreenEventHook
  , startupHook = do
      setWMName "LG3D"
      -- windows $ W.greedyView startupWorkspace
      spawn "~/.xmonad/startup-hook"
  , manageHook = manageHook defaultConfig
      <+> composeAll myManagementHooks
      <+> fullscreenManageHook
      <+> manageDocks
  , logHook = takeTopFocus <+> dynamicLogWithPP xmobarPP {
      ppOutput = hPutStrLn xmproc
      -- , ppSep = "┃"
      , ppSep = "    "
      , ppOrder = (\(ws:lo:ti:ex) -> [ti,ws,lo])
      , ppSort = getSortByXineramaRule
      , ppTitle = xmobarColor myTitleColor "" . wrap "<" ">" . shorten myTitleLength
      -- , ppTitle = xmobarColor "black" "#777777" . shorten myTitleLength
      , ppCurrent = xmobarColor myCurrentWSColor ""
        . wrap myCurrentWSLeft myCurrentWSRight
      , ppVisible = xmobarColor myVisibleWSColor ""
        . wrap myVisibleWSLeft myVisibleWSRight
      , ppUrgent = xmobarColor myUrgentWSColor ""
        . wrap myUrgentWSLeft myUrgentWSRight
      , ppHidden = xmobarColor myHiddenWSColor ""
      , ppLayout = xmobarColor "white" "#222222" .
              (\ x -> pad $ case x of
                        "ResizableTall"        -> " ┃┇"
                        "Mirror ResizableTall" -> " 𝌃 "
                        "Full"                 -> " □ "
                        "Grid"                 -> " 𐌎 "
                        "Tabbed Simplest"      -> " ▣ "
                        _                      -> x
              )
    }
  }
    `additionalKeys` myKeys

    -- `additionalMouseBindings` myMouse
