import XMonad hiding (whenJust)
import XMonad.Layout.Spacing
import qualified XMonad.Layout.BinarySpacePartition as BSP
import XMonad.Layout.WindowNavigation hiding (Direction2D(..))
import qualified XMonad.Layout.WindowNavigation as Nav (Direction2D(..))
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.StackSet
import qualified XMonad.Layout.Gaps as Gaps
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.StackSet (peek)
import qualified XMonad.Operations as O
import Control.Monad.Extra (whenJust)

myTerminal = "/usr/bin/konsole"
modm = mod5Mask
altMask = mod1Mask

main = xmonad $ docks $ ewmh def
  { terminal = myTerminal
  , modMask = modm
  , borderWidth = 1
  , focusedBorderColor = "#e69de8"
  , handleEventHook = handleEventHook def <+> fullscreenEventHook
  -- , logHook = fadeInactiveLogHook 0.8
    , layoutHook = (windowNavigation . addGaps 10 . toggleableFullscreen) $
                 BSP.emptyBSP
  , startupHook = spawn "polybar -r cherry"
  , focusFollowsMouse = False
  }
  `additionalKeys`
  [ ((modm .|. shiftMask, xK_q), O.kill)
  , ((modm, xK_period), withWindowSet $ \x -> whenJust (peek x) O.float)
  , ((modm, xK_comma), withFocused $ windows . sink)
  , ((modm, xK_Return), spawn myTerminal)
  , ((modm, xK_d), spawn "/usr/bin/dmenu_run")
  , ((modm, xK_o), sendMessage $ Toggle NBFULL)
  , ((modm, xK_h), sendMessage $ Go Nav.L)
  , ((modm, xK_j), sendMessage $ Go Nav.D)
  , ((modm, xK_k), sendMessage $ Go Nav.U)
  , ((modm, xK_l), sendMessage $ Go Nav.R)
  , ((modm .|. shiftMask, xK_h), sendMessage $ Swap BSP.L)
  , ((modm .|. shiftMask, xK_j), sendMessage $ Swap BSP.D)
  , ((modm .|. shiftMask, xK_k), sendMessage $ Swap BSP.U)
  , ((modm .|. shiftMask, xK_l), sendMessage $ Swap BSP.R)
  , ((modm .|. altMask, xK_h), sendMessage $ BSP.ShrinkFrom BSP.R)
  , ((modm .|. altMask, xK_j), sendMessage $ BSP.ExpandTowards BSP.D)
  , ((modm .|. altMask, xK_k), sendMessage $ BSP.ShrinkFrom BSP.D)
  , ((modm .|. altMask, xK_l), sendMessage $ BSP.ExpandTowards BSP.R)
  , ((modm, xK_r), sendMessage BSP.Rotate)
  ]

addGaps gapSize = let innerGaps = gapSize `div` 2
                      outerGaps = map (\x -> (x, gapSize)) [Gaps.U, Gaps.R, Gaps.D, Gaps.L]
                   in (avoidStruts . Gaps.gaps outerGaps . spacing innerGaps)
toggleableFullscreen = mkToggle $ single NBFULL
