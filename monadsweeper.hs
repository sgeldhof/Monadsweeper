{-# LANGUAGE TemplateHaskell #-}

import Control.Monad (void)
import qualified Data.Set as S
import qualified Data.Vector as V
import System.Random

import Brick
import qualified Graphics.Vty as Vty
import Lens.Micro.Platform

data Cell = Unrevealed | Flagged | Bomb | Zero | One | Two
          | Three | Four | Five | Six | Seven | Eight deriving (Eq)

instance Show Cell where
  show Unrevealed = "◼"
  show Flagged    = "▶"
  show Bomb       = "*"
  show Zero       = " "
  show One        = "1"
  show Two        = "2"
  show Three      = "3"
  show Four       = "4"
  show Five       = "5"
  show Six        = "6"
  show Seven      = "7"
  show Eight      = "8"

fromInt :: Int -> Cell
fromInt 0 = Zero
fromInt 1 = One
fromInt 2 = Two
fromInt 3 = Three
fromInt 4 = Four
fromInt 5 = Five
fromInt 6 = Six
fromInt 7 = Seven
fromInt 8 = Eight
fromInt _ = Unrevealed

type Grid = V.Vector (V.Vector Cell)

data State = State
  { _grid        :: Grid
  , _bombs       :: S.Set (Location)
  , _cursor      :: Location
  , _show_cursor :: Bool
  , _revealed    :: Int
  , _flagged     :: Int
  , _rands       :: [Int]
  , _won         :: Bool
  , _lost        :: Bool
  }

makeLenses ''State

type Event = ()
data Name = GridBox | Status deriving (Eq, Ord, Show)

rows       = 16 :: Int
cols       = 30 :: Int
totalCells = rows*cols :: Int
numBombs  = 100 :: Int
lostStr = " Game Over. You Lose." :: String
wonStr = " Game Over. You Won!" :: String
restartStr = "\nPress r to restart" :: String
emptyGrid = V.replicate rows (V.replicate cols Unrevealed) :: Grid
emptyState = State
  { _grid     = emptyGrid
  , _bombs    = S.empty
  , _cursor   = Location (0,0)
  , _show_cursor = True
  , _revealed = 0
  , _flagged  = 0
  , _rands    = []
  , _won      = False
  , _lost     = False
  }

app :: App State Event Name
app = App
  { appDraw = drawUI
  , appChooseCursor = if True then showFirstCursor else neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = startEvent
  , appAttrMap = const theMap
  }

neighbors :: Location -> [Location]
neighbors l = 
  [ Location (cs, rs)
  | cs <- [cMin..cMax]
  , rs <- [rMin..rMax]
  , cs /= c || rs /= r
  ]
  where
    Location (c,r) = l
    cMin = max 0 $ c-1
    cMax = min (cols-1) $ c+1
    rMin = max 0 $ r-1
    rMax = min (rows-1) $ r+1

spawnBombs :: Location -> State -> State
spawnBombs l s
  | (S.size $ s^.bombs) < numBombs &&
    (bomb /= l) &&
    bomb `notElem` (neighbors l)
  = spawnBombs l $ s & bombs %~ S.insert bomb
                     & rands %~ drop 2
  | (S.size $ s^.bombs) < numBombs = spawnBombs l $ s & rands %~ drop 2
  | otherwise = s
    where
      r1 = (s^.rands)!!0
      r2 = (s^.rands)!!1
      bomb = Location (succ r1 `mod` cols-1, succ r2 `mod` rows-1)

-- The 1 is subtracted as revealed is being incremented at the same time.
hasWon :: State -> Bool
hasWon s = s^.revealed == totalCells - numBombs - 1

-- I should probably get rid of the nested ifs
reveal :: Location -> State -> State
reveal l s = 
  let Location (c,r) = l
      newCell = fromInt . S.size $ S.intersection
        (s^.bombs) (S.fromList . neighbors $ l) in
  case s^.grid^?ix r^._Just^?ix c of
    Just Unrevealed
      -> if l `elem` s^.bombs 
         then s & grid.ix r.ix c .~ Bomb
                & lost .~ True
         else
           (if newCell == Zero
            then revealNeighbors l
            else id)
              $ s & grid.ix r.ix c .~ newCell
                  & won .~ (hasWon s)
                  & revealed %~ succ
    _ -> s

revealNeighbors :: Location -> State -> State
revealNeighbors l s = foldl (flip reveal) s $ neighbors l

toggleFlag :: Location -> State -> State
toggleFlag l s =
  let Location (c,r) = l in
  case s^.grid^?ix r^._Just^?ix c of
    Just Unrevealed -> s & grid.ix r.ix c .~ Flagged
                         & flagged %~ succ
    Just Flagged    -> s & grid.ix r.ix c .~ Unrevealed
                         & flagged %~ pred
    _ -> s

reset :: State -> State
reset s = emptyState & rands .~ s^.rands

toggle_cursor :: State -> State
toggle_cursor s = s & show_cursor %~ not

main :: IO ()
main = do
  gen <- getStdGen
  let initState = emptyState & rands .~ randoms gen
      buildVty = do
        v <- Vty.mkVty =<< Vty.standardIOConfig
        Vty.setMode (Vty.outputIface v) Vty.Mouse True
        return v
  void $ customMain buildVty Nothing app initState

moveUpOne :: State -> State 
moveUpOne = over (cursor .locL. _2) ((`mod` rows) . pred)

moveDownOne :: State -> State
moveDownOne = over (cursor .locL. _2) ((`mod` rows) . succ)

moveLeftOne :: State -> State
moveLeftOne = over (cursor .locL. _1) ((`mod` cols) . pred)

moveRightOne :: State -> State
moveRightOne = over (cursor .locL. _1) ((`mod` cols) . succ)

moveLeft :: State -> State
moveLeft = set (cursor .locL. _1) 0

moveRight :: State -> State
moveRight = set (cursor .locL. _1) (cols-1)

moveTop :: State -> State
moveTop = set (cursor .locL. _2) 0

moveBottom :: State -> State
moveBottom = set (cursor .locL. _2) (rows-1)

startEvent :: State -> EventM Name State
startEvent s = do
  return s

handleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent s e
  | s^.lost    = handleEventAny s e
  | s^.won     = handleEventAny s e
  | otherwise  = handleEventPlay s e

handleEventPlay :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEventPlay s e =
  case e of
    (MouseDown GridBox Vty.BLeft [] l)
      -> continue . reveal l .
         (if s^.revealed == 0 then spawnBombs l else id) $ s
    (MouseDown GridBox Vty.BRight [] l)
      -> continue $ toggleFlag l s
    _ -> handleEventAny s e

handleEventAny :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEventAny s e =
  case e of
    VtyEvent (Vty.EvKey (Vty.KChar 'c') [])
      -> continue . toggle_cursor $ s
    VtyEvent (Vty.EvKey (Vty.KEnter) [])
      -> continue . reveal (s^.cursor) .
         (if s^.revealed == 0 then spawnBombs (s^.cursor) else id) $ s
    VtyEvent (Vty.EvKey (Vty.KChar ' ') [])
      -> continue $ toggleFlag (s^.cursor) s
    VtyEvent (Vty.EvKey (Vty.KChar 'r') [])
      -> continue . reset $ s
    VtyEvent (Vty.EvKey Vty.KEsc [])
      -> halt s
    VtyEvent (Vty.EvKey (Vty.KChar 'k') [])
      -> continue $ moveUpOne s
    VtyEvent (Vty.EvKey (Vty.KChar 'j') [])
      -> continue $ moveDownOne s
    VtyEvent (Vty.EvKey (Vty.KChar 'h') [])
      -> continue $ moveLeftOne s
    VtyEvent (Vty.EvKey (Vty.KChar 'l') [])
      -> continue $ moveRightOne s
    VtyEvent (Vty.EvKey (Vty.KChar 'K') [])
      -> continue $ moveTop s
    VtyEvent (Vty.EvKey (Vty.KChar 'J') [])
      -> continue $ moveBottom s
    VtyEvent (Vty.EvKey (Vty.KChar 'H') [])
      -> continue $ moveLeft s
    VtyEvent (Vty.EvKey (Vty.KChar 'L') [])
      -> continue $ moveRight s
    VtyEvent (Vty.EvKey Vty.KUp [])
      -> continue $ moveUpOne s
    VtyEvent (Vty.EvKey Vty.KDown [])
      -> continue $ moveDownOne s
    VtyEvent (Vty.EvKey Vty.KLeft [])
      -> continue $ moveLeftOne s
    VtyEvent (Vty.EvKey Vty.KRight [])
      -> continue $ moveRightOne s
    VtyEvent (Vty.EvKey Vty.KUp [Vty.MShift])
      -> continue $ moveTop s
    VtyEvent (Vty.EvKey Vty.KDown [Vty.MShift])
      -> continue $ moveBottom s
    VtyEvent (Vty.EvKey Vty.KLeft [Vty.MShift])
      -> continue $ moveLeft s
    VtyEvent (Vty.EvKey Vty.KRight [Vty.MShift])
      -> continue $ moveRight s
    _ -> continue s

drawUI ::  State -> [Widget Name]
drawUI s = [drawGrid s <=> drawStatus s]

drawGrid :: State -> Widget Name
drawGrid s = 
  clickable GridBox
  $ (if s^.show_cursor then showCursor GridBox (s^.cursor) else id)
  $ (V.foldl1 (<=>)) . V.map ((V.foldl1 (<+>)) . V.map (drawCell)) $ s^.grid

drawCell :: Cell -> Widget Name
drawCell c = withAttr (color c) . str . show $ c

drawStatus :: State -> Widget Name
drawStatus s = str $ concat
  [show $ numBombs - s^.flagged
  , if s^.lost then lostStr    else ""
  , if s^.lost then restartStr else ""
  , if s^.won  then wonStr     else ""
  , if s^.won  then restartStr else ""
  ]

theMap :: AttrMap
theMap = attrMap Vty.defAttr
  [ (blue, Vty.blue `on` Vty.brightWhite)
  , (cyan, Vty.cyan `on` Vty.brightWhite)
  , (red, Vty.red `on` Vty.brightWhite)
  , (magenta, Vty.magenta `on` Vty.brightWhite)
  , (green, Vty.green `on` Vty.brightWhite)
  , (yellow, Vty.yellow `on` Vty.brightWhite)
  , (black, Vty.brightBlack `on` Vty.brightWhite)
  , (white, Vty.white `on` Vty.brightWhite)
  ]

black   :: AttrName
black    = attrName "black"
blue    :: AttrName
blue     = attrName "blue"
cyan    :: AttrName
cyan     = attrName "cyan"
green   :: AttrName
green    = attrName "green"
magenta :: AttrName
magenta  = attrName "magenta"
red     :: AttrName
red      = attrName "red"
white   :: AttrName
white    = attrName "white"
yellow  :: AttrName
yellow   = attrName "yellow"

color :: Cell -> AttrName
color Zero       = white
color One        = blue
color Two        = green
color Three      = red
color Four       = cyan
color Five       = magenta
color Six        = yellow
color Seven      = blue
color Eight      = blue
color Unrevealed = black
color Flagged    = red
color Bomb       = black

