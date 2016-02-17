import XMonad

import Data.Char (isSpace, toLower)
import System.Exit

import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.Search
import XMonad.Actions.Submap

import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.Circle
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.OneBig
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.ShowWName
import XMonad.Layout.Spacing

import XMonad.Prompt
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Input
import XMonad.Prompt.RunOrRaise

import XMonad.Util.Run (runProcessWithInput)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

notes = "/home/ben/Notes.txt"

startupCommands = ["skype","dunst","spotify","spotify-notify","compton"
                  ,"urxvt -rv -T ViM -e vim","redshift"
                  ,"xrandr --output VGA-1 --right-of LVDS-1"
                  ]

--
--
--  WORKSPACES
--  Using XMonad.Actions.DynamicWorkspaces, all workspaces have a name that I can set on
--  the fly. Some workspaces are also considered Fixed and/or Named, which come with
--  special properties. 
--
--  Fixed workspaces are static. These are the only workspaces that are guaranteed to be
--  up at all times, because non-fixed workspaces are removed as soon as they are both
--  empty and unviewed. All fixed workspaces are named, but not vice versa.
--
--  Named workspaces are workspaces with predefined settings. For named workspaces that
--  are also fixed, this means those workspaces simply always have those properties. For
--  those that aren't fixed, it means I have a way of creating a workspace with special
--  properties by simply creating one with a matching name. The setting which all named
--  workspaces share is a shortcut that corresponds with one of the number keys, which
--  can be used to travel to that workspace quickly (without going into visual mode).
--
--

fixedWorkspaces = ["main","vim","chat","media"]

namedWs = ["main","vim","chat","writing","gimp","work","reference","compile","steam"
          ,"media","view"]

wsKeys = [xK_1,xK_2,xK_3,xK_4,xK_5,xK_6,xK_7,xK_8,xK_9,xK_0]

addHiddenWorkspacePrompt :: X ()
addHiddenWorkspacePrompt = inputPrompt myXPConfig "Workspace Name" ?+ addHiddenWorkspace

clearWorkspace = removeEmptyWorkspaceAfterExcept fixedWorkspaces

--
--
--  PROMPTS
--  A sizable part of this setup is going to be devoted to prompts from XMonad.Prompt,
--  which I use for writing to files, opening man pages, replacing dmenu, etc.
--
--

myXPConfig = defaultXPConfig
  { position = Top, promptBorderWidth = 0, height = 14
  , font = "xft:Hack:Regular:size=8", bgColor = "#222222"
  , fgColor = "#87afdf", bgHLight = "#87afdf"
  }

makeNote :: X ()
makeNote =  appendFilePrompt myXPConfig notes
         >> spawn "echo \"%\" >> Notes.txt"
         >> spawn "sudo strfile /usr/share/fortune/myNotes"

calcPrompt :: XPConfig -> String -> X ()
calcPrompt c ans = 
  inputPrompt c (trim ans) ?+ \input ->
    liftIO (runProcessWithInput "qalc" [input] "") >>= calcPrompt c
  where trim = let f = reverse . dropWhile isSpace in f . f

infoPrompt :: XPConfig -> String -> X ()
infoPrompt c str =
  inputPromptWithCompl c (trim str) (mkComplFunFromList cs) ?+ \input ->
    liftIO (findCommand input) >>= infoPrompt c
  where findCommand input = case input of
          "battery"     -> runProcessWithInput "acpi" ["-b"] ""
          "date"        -> runProcessWithInput "date" ["+%a, %B %d"] ""
          "time"        -> runProcessWithInput "date" ["+%H:%M:%S"] ""
          _             -> runProcessWithInput "echo" ["No such command..."] ""
        cs = ["battery","date","time"]
        trim = let f = reverse . dropWhile isSpace in f . f

killPrompt :: XPConfig -> X ()
killPrompt c = 
  inputPrompt c "kill" ?+ \str -> spawn $ concat ["kill -9 `pgrep ",str,"`"]

searchPrompt :: XPConfig -> X ()
searchPrompt c = inputPromptWithCompl c "Search" (mkComplFunFromList ss) ?+ search'
  where ss = ["google","dictionary","wolfram","alpha","wikipedia","youtube","hackage"
             ,"hoogle","imdb","archwiki","aur","gmail","sareth"]

search' :: String -> X ()
search' s = search "vivaldi-snapshot" (use engine) query
  where (engine,query) = parseSearch s

parseSearch :: String -> (SearchEngine,String)
parseSearch s = (engine,query)
  where engine' = takeWhile (/=' ') s
        engine = case map toLower engine' of
          "google"      -> google
          "dictionary"  -> dictionary
          "wolfram"     -> alpha
          "alpha"       -> alpha
          "wiki"        -> wikipedia
          "wikipedia"   -> wikipedia
          "youtube"     -> youtube
          "hackage"     -> hackage
          "hoogle"      -> hoogle
          "imdb"        -> imdb
          "archwiki"    -> searchEngine "archwiki" archwiki
          "aur"         -> searchEngine "aur" aur
          "gmail"       -> searchEngine "gmail" gmail
          --Sareth goes to the Sareth Wiki, a personal website
          "sareth"      -> searchEngine "sareth" sareth
          _             -> google
        query = drop 1 $ dropWhile (/=' ') s
        sareth = "http://www.bkugler.com/wiki/doku.php?do=search&id="
        archwiki = "https://wiki.archlinux.org/index.php?title=Special%3ASearch&search="
        aur = "https://aur.archlinux.org/packages/?O=0&K="
        gmail = "https://inbox.google.com/u/0/search/"

--
--
--  KEYBINDINGS
--  The basic idea here is a modal shortcut system. There are three modes that XMonad
--  can be in: default mode, visual mode, or command mode. The modes are made with
--  submaps of keybindings, most of which call the submap again.
--
--
--
--
--

keyboard conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
  --  Default mode has a few convenience functions (ones that I use too often to always 
  --  have to switch modes) and then shortcuts for going into command or visual.
  [ ((modm              , xK_Return), spawn $ XMonad.terminal conf)

  , ((modm              , xK_v), submap . M.fromList $ visualMode)
  , ((modm              , xK_semicolon), submap . M.fromList $ commandMode)
  , ((modm              , xK_m), submap . M.fromList $ audioMode)

  --Everything below this point (including the three mapped lists) is here because
  --I use these commands too much for me to have to enter a specific mode every
  --time I use one
  , ((modm              , xK_j), windows W.focusDown)
  , ((modm              , xK_k), windows W.focusUp)
  , ((modm              , xK_minus), clearWorkspace $ toggleWS)

  --And a safety function
  , ((modm .|. shiftMask, xK_q), io (exitWith ExitSuccess))
  ] ++ navKeys
  where 
    --  Command mode is a submap for spawning new processes, calling in XMonad prompts, and
    --  generally executing actions I find myself doing a lot.
    commandMode =
      --Command mode is mainly going to be used for prompts...
      [ ((0, xK_o), runOrRaisePrompt myXPConfig)
      , ((0, xK_n), makeNote)
      , ((0, xK_c), calcPrompt myXPConfig "qalc")
      , ((0, xK_i), infoPrompt myXPConfig "info")
      , ((0, xK_k), killPrompt myXPConfig)
      , ((0, xK_s), searchPrompt myXPConfig)
  
      -- ...but occasionally I'll use it to spawn something else
      , ((0, xK_Return), spawn $ XMonad.terminal conf)
      , ((0, xK_x), spawn "xmonad --recompile; xmonad --restart")
      , ((0, xK_e), spawn "urxvt -rv -T ViM -e vim")
      , ((0, xK_t), spawn "urxvt -T ScratchPad -rv")
      , ((0, xK_r), spawn "urxvt -rv -T Ranger -e ranger")

      , ((0, xK_m), submap . M.fromList $ audioMode)
      , ((0, xK_v), submap . M.fromList $ visualMode)
      ]
    --  Visual mode is for manipulating the windows on the screen. This means changing 
    --  focus, rearranging tiles, etc. are handled almost exclusively in visual mode
    visualMode =
      --The easiest use of visual mode is for basic navigation and tile arrangement
      [ ((0, xK_j), windows W.focusDown >> visual)
      , ((0, xK_k), windows W.focusUp >> visual)
      , ((shiftMask, xK_j), windows W.swapDown >> visual)
      , ((shiftMask, xK_k), windows W.swapUp >> visual)
      , ((0, xK_minus), clearWorkspace $ toggleWS)

      --{modifier}+{h,l} controls sizing of tiles
      , ((shiftMask, xK_h), sendMessage Shrink >> visual)
      , ((shiftMask, xK_l), sendMessage Expand >> visual)
      , ((controlMask, xK_h), sendMessage MirrorExpand >> visual)
      , ((controlMask, xK_l), sendMessage MirrorShrink >> visual)
    
      , ((0, xK_d), kill1)
      , ((0, xK_space), sendMessage NextLayout >> visual)
      , ((0, xK_s), (withFocused $ windows . W.sink) >> visual)
      , ((0, xK_z), (sendMessage $ Toggle FULL) >> visual)
    
      -- <C-a> and <C-x> increment/decrement the number of tiles in the master area
      , ((controlMask, xK_a), sendMessage (IncMasterN 1) >> visual)
      , ((controlMask, xK_z), sendMessage (IncMasterN (-1)) >> visual)
    
      --Visual mode is also where you go to make new workspaces, with o, t, or c.
      --Shift+O, t, and c all drag (or copy, for c) the currently focused window
      --into the new workspace.
      , ((0, xK_o), clearWorkspace $ selectWorkspace myXPConfig)
      , ((0, xK_t), withWorkspace myXPConfig (windows . W.shift))
      , ((shiftMask, xK_o), clearWorkspace $ withWorkspace myXPConfig
                              (\w -> windows $ (W.greedyView w) . (W.shift w)))
      , ((0, xK_c), withWorkspace myXPConfig (windows . copy))

      , ((0, xK_semicolon), submap . M.fromList $ commandMode)
      , ((0, xK_m), submap . M.fromList $ audioMode)
      ] ++ navKeys
    visual = submap . M.fromList $ visualMode
    --Audio Mode deals with volume and spotify
    audioMode = 
      --Volume controls first
      [ ((0, xK_m), spawn "amixer set Master toggle" >> audio)

      , ((0, xK_j),           spawn "amixer set Master 10%-" >> audio)
      , ((shiftMask, xK_j),   spawn "amixer set Master 20%-" >> audio)
      , ((controlMask, xK_j), spawn "amixer set Master 5%-" >> audio)
      , ((0, xK_k),           spawn "amixer set Master 10%+" >> audio)
      , ((shiftMask, xK_k),   spawn "amixer set Master 20%+" >> audio)
      , ((controlMask, xK_k), spawn "amixer set Master 5%+" >> audio)

      , ((0, xK_1), spawn "amixer set Master 10%" >> audio)
      , ((0, xK_2), spawn "amixer set Master 20%" >> audio)
      , ((0, xK_3), spawn "amixer set Master 30%" >> audio)
      , ((0, xK_4), spawn "amixer set Master 40%" >> audio)
      , ((0, xK_5), spawn "amixer set Master 50%" >> audio)
      , ((0, xK_6), spawn "amixer set Master 60%" >> audio)
      , ((0, xK_7), spawn "amixer set Master 70%" >> audio)
      , ((0, xK_8), spawn "amixer set Master 80%" >> audio)
      , ((0, xK_9), spawn "amixer set Master 90%" >> audio)
      , ((0, xK_0), spawn "amixer set Master 100%" >> audio)

      --Now for Spotify
      , ((0, xK_l), spawn "spotify-notify -a next" >> audio)
      , ((0, xK_h), spawn "spotify-notify -a previous" >> audio)
      , ((0, xK_space), spawn "spotify-notify -a playPause" >> audio)

      , ((0, xK_semicolon), submap . M.fromList $ commandMode)
      , ((0, xK_v), submap . M.fromList $ visualMode)
      ]
    audio = submap . M.fromList $ audioMode
    --Navigation keys take care of movement between workspaces and screens
    navKeys =
      [((modm .|. m, key), (screenWorkspace sc >>= flip whenJust (windows . f)))
        | (key, sc) <- zip [xK_w, xK_e] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
      ++
      [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip namedWs wsKeys
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

mouse (XConfig {XMonad.modMask = modm}) = M.fromList $
  [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)) ]

--
--
--  LAYOUT
--  Basic layout uses ResizableTall (in two orientations) and Full. Any number of tiles
--  can be merged into a Tabbed tile in visual mode. 
--
--  There are also custom layouts for particular named workspaces. Each of these layouts
--  is given its own line below the "where" and then strung together at the end with the
--  perWS function.
--
--  mkToggle allows for zooming in on windows. This zoom (Z in visual mode) replaces
--  the use of the Full layout.
--
--

layout = showWName $ mkToggle (single FULL) $ perWS $ smartBorders $ defaultLayouts
  where tall = smartSpacing 5 $ ResizableTall 1 (1/100) (1/2) []
        one = smartSpacing 5 $ OneBig (3/4) (3/4)

        defaultLayouts = tall ||| Mirror tall

        chat =    onWorkspace "chat"    $ reflectHoriz Circle
        steam =   onWorkspace "steam"   $ one
        compile = onWorkspace "compile" $ one
        media =   onWorkspace "media"   $ one ||| reflectHoriz one
        gimp =    onWorkspace "gimp"    $   
                    withIM (0.15) (Role "gimp-toolbox") $ reflectHoriz $
                      withIM (0.20) (Role "gimp-dock") (Mirror tall ||| Full)
        writing = onWorkspace "writing" $ one ||| Grid
        vim =     onWorkspace "vim"     $ one ||| tall
        file =    onWorkspace "file"    $ Grid ||| one
        view =    onWorkspace "view"    $ one ||| Grid

        grid =    onWorkspace "grid"    $ Grid ||| reflectHoriz Circle

        perWS = chat . steam . compile . media . gimp . writing . vim . file . view . grid

--
--
--  MANAGE HOOK
--  Fairly standard manage hook. Some windows, when spawned, are moved to one of the
--  named workspaces designed for that type of program.
--
--

myManageHook = composeAll
  [ className =? "MPlayer"                            --> doShift "media"
  , className =? "Gimp"                               --> doShift "gimp"
  , className =? "feh"						                    --> doFloat
  , className =? "Firefox" <&&> resource =? "Dialog"  --> doFloat
  , className =? "Skype"					                    --> doShift "chat"
  , className =? "Mumble"                             --> doShift "chat"
  , className =? "Steam"                              --> doShift "steam"
  , className =? "Spotify"                            --> doShift "media"
  , className =? "stalonetry"                         --> doSideFloat NC
  , title =? "ViM"                                    --> doShift "vim"
  , title =? "ScratchPad"                             --> doSideFloat CE
  , isFullscreen                                      --> doFullFloat
  , isDialog                                          --> doFloat
  ]

--
--
--  Log Hook
--  This is used to handle the FadeWindows hook. The fadeHook works similarly to other
--  hooks, but because it hits all windows, it has to account for all possibilities. The
--  first line ("opaque") is the default; the isUnfocused line is the thing I want it
--  to handle (most unfocused windows are transparent).
--  Everything after that line are exceptions to the rule; they will reset to opaque.
--
--

myLogHook = fadeWindowsLogHook fadeHook
  where fadeHook :: FadeHook
        fadeHook = composeAll [ opaque 
                              , isUnfocused             --> transparency 0.3
                              , isFloating              --> opaque
                              , className =? "Netflix"  --> opaque
                              , className =? "MPlayer"  --> opaque
                              ]

main :: IO ()
main = xmonad $ defaultConfig
  { terminal            = "urxvt -rv"
  , normalBorderColor   = "#495157"
  , focusedBorderColor  = "#61849B"
  , focusFollowsMouse   = True
  , clickJustFocuses    = False
  , borderWidth         = 2
  , modMask             = mod1Mask
  , workspaces          = fixedWorkspaces
  , keys                = keyboard
  , mouseBindings       = mouse
  , layoutHook          = showWName layout
  , manageHook          = myManageHook
  , startupHook         = mapM_ spawn startupCommands
  , logHook             = myLogHook
  }
