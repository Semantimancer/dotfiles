import XMonad

import Data.Char (isSpace, toLower)
import System.Exit

import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.TopicSpace
import XMonad.Actions.Search
import XMonad.Actions.Submap

import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Circle
import XMonad.Layout.IM
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.OneBig
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
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

startupCommands = ["skype","rawdog -Wuw"]

--
--
--  TOPICS
--  Topic spaces let me execute commands upon going to a workspace. Most
--  workspaces will also have a special directory that terminals will
--  automatically cd to when called.
--  
--
--

wsKeys = [xK_BackSpace,xK_1,xK_2,xK_3,xK_4,xK_5,xK_6,xK_7,xK_8,xK_9,xK_0]

clearWorkspace :: X () -> X ()
clearWorkspace = removeEmptyWorkspaceAfterExcept stickies
  where stickies = take 11 $ map topicName myTopics

data TopicItem = TI { topicName :: String
                    , topicPath :: FilePath
                    , topicAction :: X ()
                    }

myTopics :: [TopicItem]
myTopics = [TI "dashboard"  homedir                       $ spawn' "Ranger" "ranger"
           ,TI "web"        homedir                       $ spawn "firefox"
           ,TI "vim"        homedir                       $ spawn' "ViM" "vim"
           ,TI "chat"       homedir                       $ spawn "skype"
           ,TI "writing"    (homedir++"/Documents")       $ writerPrompt
           ,TI "gimp"       (homedir++"/Pictures")        $ spawn "gimp"
           ,TI "work"       homedir                       $ spawnShell
           ,TI "reference"  homedir                       $ spawnShell
           ,TI "compile"    (homedir++"/Computer")        $ spawnShell
           ,TI "game"       (homedir++"/Games")           $ spawnShell
           ,TI "music"      (homedir++"/Music")           $ spawn "spotify"
           ,TI "movie"      (homedir++"/Videos")          $ spawnShell
           ,TI "view"       (homedir++"/Pictures")        $ spawnShell
           ,TI "upload"     (homedir++"/Computer/Web")    $ spawn "filezilla"
           ,TI "pdf"        (homedir++"/Documents")       $ spawnShell
           ,TI "steam"      (homedir++"/home/ben/Games")  $ spawn "steam.sh"
           ]
  where homedir = "/home/ben"
        spawn' x y = spawn $ "urxvt -T "++x++" -e "++y

myTopicConfig :: TopicConfig
myTopicConfig = defaultTopicConfig
  { topicDirs = M.fromList $ map (\(TI n p _) -> (n,p)) myTopics
  , defaultTopicAction = const spawnShell
  , defaultTopic = "dashboard"
  , topicActions = M.fromList $ map (\(TI n _ a) -> (n,a)) myTopics
  , maxTopicHistory = 1
  }

spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawn $ "urxvt -cd \""++dir++"\""

goto :: WorkspaceId -> X ()
goto = switchTopic myTopicConfig

promptedGoto :: XPConfig -> X ()
promptedGoto c = promptedAction c "goto" (clearWorkspace . createOrGoto)

promptedShift :: XPConfig -> X ()
promptedShift c = promptedAction c "shift" (windows . W.shift)

promptedCopy :: XPConfig -> X ()
promptedCopy c = promptedAction c "copy" createAndCopy
  where createAndCopy w = do newWorkspace w
                             windows $ copy w

promptedAction :: XPConfig -> String -> (WorkspaceId -> X ()) -> X ()
promptedAction c str f = do
  ws <- getPossibleWorkspaces
  inputPromptWithCompl c str (mkComplFunFromList ws) ?+ f

createGoto :: WorkspaceId -> X ()
createGoto w = newWorkspace w >> switchTopic myTopicConfig w

createOrGoto :: WorkspaceId -> X ()
createOrGoto w = do
  exists <- workspaceExist w
  if exists then goto w else createGoto w

newWorkspace :: WorkspaceId -> X ()
newWorkspace w = do
  exists <- workspaceExist w
  if exists then return () else addHiddenWorkspace w

workspaceExist :: WorkspaceId -> X Bool
workspaceExist w = do
  xs <- get
  return $ workspaceExists w (windowset xs)

workspaceExists :: WorkspaceId -> W.StackSet WorkspaceId l a s sd -> Bool
workspaceExists w ws = w `elem` map W.tag (W.workspaces ws)

getPossibleWorkspaces :: X [WorkspaceId]
getPossibleWorkspaces = do
  ws <- get
  return $ ts++(filter (`notElem` ts) (existingWorkspaces (windowset ws)))
  where ts = map topicName myTopics

existingWorkspaces :: W.StackSet WorkspaceId l a s sd -> [String]
existingWorkspaces ws = map W.tag $ W.workspaces ws

--
--
--  PROMPTS
--  A sizable part of this setup is going to be devoted to prompts from XMonad.
--  Prompt, which I use for writing to files, opening man pages, replacing 
--  dmenu, etc.
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
searchPrompt c = inputPromptWithCompl c "search" (mkComplFunFromList ss) ?+ search'
  where ss = ["google","dictionary","wolfram","alpha","wikipedia","youtube","hackage"
             ,"hoogle","imdb","archwiki","aur","gmail","sareth"]

search' :: String -> X ()
search' s = search "firefox" (use engine) query
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

writerPrompt :: X ()
writerPrompt = inputPromptWithCompl myXPConfig "writer" (mkComplFunFromList xs) ?+ f
  where f x = if x `elem` xs then spawn x else return ()
        xs = ["focuswriter","turtl"]

spotifyCommand :: String -> X ()
spotifyCommand str = spawn $ "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player."++str

wallpaperCommand :: String -> X ()
wallpaperCommand str = case str of
  "toggle"  -> spawn ("/home/ben/.wallpaper/toggle.sh")
  "bg"      -> spawn ("echo 'BG' > /home/ben/.wallpaper/toggle && "++wallpapers)
  _         -> spawn wallpapers
  where wallpapers = "/home/ben/.wallpaper/wallpapers.sh"

--
--
--  KEYBINDINGS
--  The basic idea here is a modal shortcut system. There are four modes that 
--  XMonad can be in: default mode, visual mode, command mode, and audio mode. 
--  The modes are made with submaps of keybindings, most of which call the 
--  submap again.
--
--
--
--

keyboard conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
  --  Default mode has a few convenience functions (ones that I use too often to 
  --  always have to switch modes) and then shortcuts for going into command,
  --  visual, and audio modes.
  [ ((modm              , xK_Return), spawnShell)
  , ((modm .|. shiftMask, xK_Return), currentTopicAction myTopicConfig)

  , ((modm              , xK_v), submap . M.fromList $ visualMode)
  , ((modm              , xK_semicolon), submap . M.fromList $ commandMode)
  , ((modm              , xK_m), submap . M.fromList $ audioMode)

  --  Everything below this point (including the three mapped lists) is here 
  --  because I use these commands too much for me to have to enter a specific 
  --  mode every time I use one
  , ((modm              , xK_j), windows W.focusDown)
  , ((modm              , xK_k), windows W.focusUp)

  --And a safety function
  , ((modm .|. shiftMask, xK_q), io (exitWith ExitSuccess))
  ] ++ navKeys
  where 
    --  Command mode is a submap for spawning new processes, calling in XMonad 
    --  prompts, and generally executing actions I find myself doing a lot.
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
      , ((0, xK_e), spawn "urxvt -T ViM -e vim")
      , ((0, xK_t), spawn "urxvt -T ScratchPad")
      , ((0, xK_r), spawn "urxvt -T Ranger -e ranger")

      , ((0, xK_m), submap . M.fromList $ audioMode)
      , ((0, xK_v), submap . M.fromList $ visualMode)
      ] ++ navKeys
    --  Visual mode is for manipulating the windows on the screen. This means 
    --  changing focus, rearranging tiles, etc. are handled almost exclusively 
    --  in visual mode
    visualMode =
      -- Basic navigation 
      [ ((0, xK_j), windows W.focusDown >> visual)
      , ((0, xK_k), windows W.focusUp >> visual)
      , ((controlMask, xK_u), sendMessage FocusParent >> visual)

      -- Moving tiles around
      , ((shiftMask, xK_j), windows W.swapDown >> visual)
      , ((shiftMask, xK_k), windows W.swapUp >> visual)
      , ((0, xK_y), sendMessage SelectNode >> visual)
      , ((controlMask, xK_a), sendMessage FocusParent >> visual)
      , ((0, xK_p), sendMessage MoveNode)
      , ((0, xK_Return), sendMessage Swap)
      , ((shiftMask, xK_Return), sendMessage Rotate)

      , ((0, xK_d), kill1)
      , ((0, xK_space), sendMessage NextLayout >> visual)
      , ((0, xK_s), (withFocused $ windows . W.sink) >> visual)
      , ((0, xK_z), (sendMessage $ Toggle FULL) >> visual)
    
      --  control+{h,j,k,l controls sizing of tiles
      , ((controlMask, xK_l), sendMessage (MoveSplit R) >> visual)
      , ((controlMask, xK_h), sendMessage (MoveSplit L) >> visual)
      , ((controlMask, xK_j), sendMessage (MoveSplit D) >> visual)
      , ((controlMask, xK_k), sendMessage (MoveSplit U) >> visual)

      -- Playing with wallpapers
      , ((0, xK_w), wallpaperCommand "bg" >> visual)
      , ((shiftMask, xK_w), wallpaperCommand "toggle" >> visual)
      , ((controlMask, xK_w), wallpaperCommand "" >> visual)
    
      --  Visual mode is also where you go to switch topics with o or O, or copy
      --  windows to other workspaces with c. 
      --  o goes to a topic, shift+O drags the focused window with you.
      , ((0, xK_o), promptedGoto myXPConfig)
      , ((shiftMask, xK_o), promptedShift myXPConfig)
      , ((0, xK_c), promptedCopy myXPConfig)

      -- Switching to other modes
      , ((0, xK_semicolon), submap . M.fromList $ commandMode)
      , ((0, xK_m), submap . M.fromList $ audioMode)
      ] ++ navKeys
    visual = submap . M.fromList $ visualMode
    --Audio Mode deals with volume and spotify
    audioMode = 
      --Volume controls first
      [ ((0, xK_m), spawn "pamixer -t" >> audio)

      , ((0, xK_j),           spawn "pamixer --decrease 10")
      , ((shiftMask, xK_j),   spawn "pamixer --decrease 20")
      , ((controlMask, xK_j), spawn "pamixer --decrease 5")
      , ((0, xK_k),           spawn "pamixer --increase 10")
      , ((shiftMask, xK_k),   spawn "pamixer --increase 20")
      , ((controlMask, xK_k), spawn "pamixer --increase 5")

      , ((0, xK_1), spawn "pamixer --set-volume 10" >> audio)
      , ((0, xK_2), spawn "pamixer --set-volume 20" >> audio)
      , ((0, xK_3), spawn "pamixer --set-volume 30" >> audio)
      , ((0, xK_4), spawn "pamixer --set-volume 40" >> audio)
      , ((0, xK_5), spawn "pamixer --set-volume 50" >> audio)
      , ((0, xK_6), spawn "pamixer --set-volume 60" >> audio)
      , ((0, xK_7), spawn "pamixer --set-volume 70" >> audio)
      , ((0, xK_8), spawn "pamixer --set-volume 80" >> audio)
      , ((0, xK_9), spawn "pamixer --set-volume 90" >> audio)
      , ((0, xK_0), spawn "pamixer --set-volume 100" >> audio)

      --Now for Spotify
      , ((0, xK_l), spotifyCommand "Next" >> audio)
      , ((0, xK_h), spotifyCommand "Previous" >> audio)
      , ((0, xK_space), spotifyCommand "PlayPause" >> audio)

      , ((0, xK_semicolon), submap . M.fromList $ commandMode)
      , ((0, xK_v), submap . M.fromList $ visualMode)
      ] ++ navKeys
    audio = submap . M.fromList $ audioMode
    --Navigation keys take care of movement between screens and topic spaces
    navKeys =
      [((modm .|. m, key), (screenWorkspace sc >>= flip whenJust (windows . f)))
        | (key, sc) <- zip [xK_w, xK_e] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
      ++
      [((m, i), f j)
        | (i,j) <- zip wsKeys (map topicName myTopics)
        , (m,f) <- [(modm,clearWorkspace . switchTopic myTopicConfig)
                   ,(modm .|. shiftMask,windows . W.shift)]
        ]

mouse (XConfig {XMonad.modMask = modm}) = M.fromList $
  [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)) ]

--
--
--  LAYOUT
--  Basic layout uses BinarySpacePartition, and all layouts can "zoom in" to only display
--  the currently-focused window (full).
--
--  There are also custom layouts for particular named workspaces. Each of these layouts
--  is given its own line below the "where" and then strung together at the end with the
--  perWS function.
--
--  mkToggle allows for zooming in on windows. This zoom (Z in visual mode) replaces
--  the use of the Full layout.
--
--

layout = showWName $ mkToggle (single FULL) $ perWS $ smartBorders $ emptyBSP
  where one = smartSpacing 5 $ OneBig (3/4) (3/4)

        chat =    onWorkspace "chat"    $ reflectHoriz Circle
        steam =   onWorkspace "steam"   $ one
        compile = onWorkspace "compile" $ one
        music =   onWorkspace "media"   $ one ||| reflectHoriz one
        gimp =    onWorkspace "gimp"    $   
                    withIM (0.15) (Role "gimp-toolbox") $ reflectHoriz $
                      withIM (0.20) (Role "gimp-dock") (emptyBSP ||| Full)
        writing = onWorkspace "writing" $ one ||| emptyBSP
        vim =     onWorkspace "vim"     $ one ||| emptyBSP
        file =    onWorkspace "file"    $ emptyBSP ||| one
        view =    onWorkspace "view"    $ one ||| emptyBSP
        movie =   onWorkspace "movie"   $ one ||| emptyBSP

        perWS = chat . steam . compile . music . gimp . writing . vim . file . view
              . movie

--
--
--  MANAGE HOOK
--  Fairly standard manage hook. Some windows, when spawned, are moved to one 
--  of the named workspaces designed for that type of program.
--
--

myManageHook = composeAll
  [ className =? "MPlayer"                            --> doShift "movie"
  , className =? "mpv"                                --> doShift "movie"
  , className =? "Gimp"                               --> doShift "gimp"
  , className =? "feh"                                --> doFloat
  , className =? "Firefox" <&&> resource =? "Dialog"  --> doFloat
  , className =? "Skype"                              --> doShift "chat"
  , className =? "Mumble"                             --> doShift "chat"
  , className =? "Steam"                              --> doShift "steam"
  , className =? "Spotify"                            --> doShift "music"
  , className =? "stalonetry"                         --> doSideFloat NC
  , title =? "ViM"                                    --> doShift "vim"
  , title =? "Ranger"                                 --> doShift "dashboard"
  , title =? "News"                                   --> doShift "reference"
  , title =? "ScratchPad"                             --> doSideFloat CE
  , isFullscreen                                      --> doFullFloat
  , isDialog                                          --> doFloat
  ]
  where unfloat = ask >>= doF . W.sink

--
--
--  Log Hook
--  This is used to handle the FadeWindows hook. The fadeHook works similarly 
--  to other hooks, but because it hits all windows, it has to account for all 
--  possibilities. The first line ("opaque") is the default; the isUnfocused 
--  line is the thing I want it to handle (most unfocused windows are 
--  transparent). Everything after that line are exceptions to the rule; they 
--  will reset to opaque.
--
--

myLogHook = fadeWindowsLogHook fadeHook
  where fadeHook :: FadeHook
        fadeHook = composeAll [ opaque 
                              , isUnfocused             --> transparency 0.3
                              , isFloating              --> opaque
                              , className =? "chromium" --> opaque
                              , className =? "MPlayer"  --> opaque
                              , className =? "mpv"      --> opaque
                              , iconName =? gimpName    --> opaque
                              ]
        gimpName = "GNU Image Manipulation Program"
        iconName = stringProperty "WM_ICON_NAME"

main :: IO ()
main = xmonad $ defaultConfig
  { terminal            = "urxvt"
  , normalBorderColor   = "#39474A"
  , focusedBorderColor  = "#567A6E"
  , focusFollowsMouse   = False
  , clickJustFocuses    = False
  , borderWidth         = 2
  , modMask             = mod1Mask
  , workspaces          = map topicName myTopics
  , keys                = keyboard
  , mouseBindings       = mouse
  , layoutHook          = layout
  , manageHook          = myManageHook
  , startupHook         = mapM_ spawn startupCommands
  , logHook             = myLogHook
  }
