import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Spacing
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

myModMask = mod4Mask

myTerminal = "urxvt"

myLayout = tiled ||| Mirror tiled ||| Full  
  where  
      -- default tiling algorithm partitions the screen into two panes  
      tiled = spacing 10 $ Tall nmaster delta ratio  
 
      -- The default number of windows in the master pane  
      nmaster = 1  
 
      -- Default proportion of screen occupied by master pane  
      ratio = 1/2  
 
      -- Percent of screen to increment by when resizing panes  
      delta = 3/100
        

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts myLayout
        , logHook = dynamicLogWithPP xmobarPP
                        {  ppOutput = hPutStrLn xmproc
                        ,  ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , modMask = mod4Mask
        , terminal = myTerminal
        }
