{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import Data.List
import Data.Text.Internal
import Data.String


-- polymorphic functions -------------------------------------------------------

elemList :: Eq a => a -> [a] -> Bool
elemList value [] = False
elemList value (x:xs) = value == x || elemList value xs


appendList :: [a] -> [a] -> [a]
appendList list1 list2 = list1 ++ list2

listLength :: [a] -> Integer
listLength [] = 0
listLength (x:xs) = 1 + listLength xs

filterList :: (a -> Bool) -> [a] -> [a]
filterList func [] = []
filterList func (x:xs)
  | func x = x:(filterList func xs)
  | otherwise = filterList func xs

nth :: [a] -> Integer -> a
nth (x:xs) 0 = x
nth (x:xs) n = nth xs (n-1)

mapList :: (a -> b) -> [a] -> [b]
mapList func [] = []
mapList func (x:xs) = func x:mapList func xs

andList :: [Bool] -> Bool
andList [] = True
andList (x:xs) = x && andList xs

allList :: (a -> Bool) -> [a] -> Bool
allList func [] = True
allList func (x:xs)
  | func x = allList func xs
  | otherwise = False

foldList :: (a -> b -> b) -> b -> [a] -> b
foldList function acc []     = acc
foldList function acc (x:xs) = function x (foldList function acc xs)

-- graph functions -------------------------------------------------------------

-- allVisitedOk helper function
allListVisitedOk :: Eq a => [a] -> (a -> [a]) -> [a] -> (a -> Bool) -> (Bool, [a])
allListVisitedOk [] neighbours visited isOk = (True, visited)
allListVisitedOk (x:xs) neighbours visited isOk
  = (xOk && xsOk, xsVisited)
  where
    xOkVisited = allVisitedOk x neighbours visited isOk
    xOk = fst xOkVisited
    xsOkVisited = allListVisitedOk xs neighbours (snd xOkVisited) isOk
    xsOk = fst xsOkVisited
    xsVisited = (snd xsOkVisited)


-- returns (whether all in x are ok, x = list of all vertices reachable from current)
allVisitedOk :: Eq a => a -> (a -> [a]) -> [a] -> (a -> Bool) -> (Bool, [a])
allVisitedOk current neighbours visited isOk
  | visited == [] && currentUnvisitedNeighbours == [] = (isOk current, [current])
  | currentUnvisitedNeighbours == [] = (isOk current, visited)
  | otherwise = (allList isOk currentUnvisitedNeighbours
         && fst neighboursVisitedOk, snd neighboursVisitedOk)
  where
    currentNeighbours = neighbours current
    currentUnvisitedNeighbours = currentNeighbours \\ visited
    newVisited = visited ++ currentUnvisitedNeighbours
    neighboursVisitedOk = allListVisitedOk currentUnvisitedNeighbours neighbours newVisited isOk


isGraphClosed :: Eq a => a -> (a -> [a]) -> (a -> Bool) -> Bool
isGraphClosed initial neighbours isOk = fst $ allVisitedOk initial neighbours [] isOk



-- reachableInUnvisited helper function
reachableInUnvisitedList :: Eq a => a -> [a] -> (a -> [a]) -> [a] -> (Bool, [a])
reachableInUnvisitedList v [] neighbours visited = (False, visited)
reachableInUnvisitedList v (x:xs) neighbours visited
  = (xReachable || xsReachable, xsVisited)
  where
    xReachableVisited = reachableInUnvisited v x neighbours visited
    xReachable = fst xReachableVisited
    xsReachableVisited = reachableInUnvisitedList v xs neighbours (snd xReachableVisited)
    xsReachable = fst xReachableVisited
    xsVisited = (snd xsReachableVisited)


-- returns (whether v is in x, x = list of all vertices reachable from current)
reachableInUnvisited :: Eq a => a -> a -> (a -> [a]) -> [a] -> (Bool, [a])
reachableInUnvisited v current neighbours visited
  | visited == [] && currentUnvisitedNeighbours == [] = (v == current, [current])
  | currentUnvisitedNeighbours == [] = (v == current, visited)
  | otherwise = (elemList v currentUnvisitedNeighbours
         || fst neighboursReachableVisited, snd neighboursReachableVisited)
  where
    currentNeighbours = neighbours current
    currentUnvisitedNeighbours = currentNeighbours \\ visited
    newVisited = visited ++ currentUnvisitedNeighbours
    neighboursReachableVisited = reachableInUnvisitedList v currentUnvisitedNeighbours neighbours newVisited



reachable :: Eq a => a -> a -> (a -> [a]) -> Bool
reachable v initial neighbours = fst $ reachableInUnvisited v initial neighbours []


allReachable :: Eq a => [a] -> a -> (a -> [a]) -> Bool
allReachable vs initial neighbours = allList reachable2 vs
  where reachable2 v = reachable v initial neighbours


getReachableList :: Eq a => a -> (a -> [a]) -> [a]
getReachableList initial neighbours = snd $ reachableInUnvisited initial initial neighbours []


-- tiles -----------------------------------------------------------------------

wall, ground, storage, box :: Picture

horizontalLine = colored black $ translated 0 (-0.525) $ solidRectangle 1 0.05
wallHorizontalLines = pictures([translated 0 y horizontalLine
  | y <- [0.25, 0.5 .. 1]])

verticalLine = colored black $ translated (-0.475) (-0.375)
  $ solidRectangle 0.05 0.25
wallVerticalLines1 = pictures([ translated x y verticalLine
  | x <- [0.25, 0.75], y <- [0, 0.5]])
wallVerticalLines2 = pictures([ translated x y verticalLine
  | x <- [0, 0.5], y <- [0.25, 0.75]])

wallBase = colored red $ solidRectangle 1 1

wall  = wallHorizontalLines & wallVerticalLines1 & wallVerticalLines2 & wallBase

ground  = colored green $ solidRectangle 1 1

storage  = colored blue $ solidRectangle 1 1

boxBorder = colored (dark brown) $ thickRectangle 0.14 0.85 0.85
boxPlank = colored (dark brown) $ solidRectangle 1.2 0.2
boxCross =  rotated (0.25 * pi) boxPlank & rotated (0.75 * pi) boxPlank
boxBase = colored brown $ solidRectangle 1 1
box  = boxBorder & boxCross & boxBase


data Tile = Wall | Ground | Storage | Box | Blank deriving Eq

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

-- maze ------------------------------------------------------------------------

data Coord = Coord {coordX :: Integer, coordY :: Integer} deriving Eq

type GameMap = Coord -> Tile

data Maze = Maze {initPos :: Coord, mazeTiles :: GameMap}

goodMaze1 :: Maze
goodMaze1 = Maze start map where
  start = Coord 0 1
  map (Coord x y)
    | abs x > 4  || abs y > 4  = Blank
    | abs x == 4 || abs y == 4 = Wall
    | x ==  2 && y <= 0        = Wall
    | x ==  3 && y <= 0        = Storage
    | x >= -2 && y == 0        = Box
    | otherwise                = Ground

-- Wariant domyślnego, gdzie jest więcej Storage niz Boxów, a plansza jest lekko większa.
goodMaze2 :: Maze
goodMaze2 = Maze start map where
  start = Coord 0 1
  map (Coord x y)
   | abs x > 5  || abs y > 5      = Blank
   | abs x == 5 || abs y == 5     = Wall
   | x ==  2 && y <= 0            = Wall
   | x ==  3 && y <= 0            = Storage
   | x >= -3 && x <= 0 && y == 0  = Box
   | otherwise                    = Ground

goodMaze3 :: Maze
goodMaze3 = Maze start map where
  start = Coord 0 0
  map (Coord x y)
   | x < (-1) || x > 20  || abs y > 1 = Blank
   | (x >= (-1) && x <= 20) && abs y == 1 = Wall
   | (x == (-1) && y == 0) || (x == 20 && y == 0) = Wall
   | x == 1 && y == 0 = Box
   | x == 19 && y == 0 = Storage  
   | otherwise = Ground

goodMaze4 :: Maze
goodMaze4 = Maze start map where
  start = Coord 0 (-2)
  map (Coord x y)
    | abs x > 3 || y > 4 || y < (-5) = Blank
    | x == (-3) && (y <= 4 && y >= (-5)) = Wall
    | x == 3 && (y <= 4 && y >= (-5)) = Wall
    | x == 2 && ((y <= 4 && y >= 0) || (y <= (-4) && y >= (-5))) = Wall    
    | y == 4 && x >= (-3) && x <= 3 = Wall
    | y == 3 && x >= 0 && x <= 3 = Wall
    | y == 0 && ((x > (-4) && x < 0) || (x > 0 && x <= 3)) = Wall
    | y == (-4) && ((x > (-4) && x < 0) || (x >= 2 && x <= 3)) = Wall
    | y == (-5) && x >= (-3) && x <= 3 = Wall
    | (x == (-1) && y == 1) || (x == (-1) && y == (-1)) || (x == 1 && y == (-1))
      || (x == 0 && y == (-3)) = Box
    | y == (-2) && ((x > (-3) && x < 0) || (x > 0 && x <= 2)) = Storage
    | otherwise = Ground


-- Brak większości ścian.
badMaze1 :: Maze
badMaze1 = Maze start map where
  start = Coord 0 1
  map (Coord x y)
   | abs x > 4  || abs y > 4  = Blank  -- blank
   | x ==  2 && y <= 0        = Wall  -- wall
   | x ==  3 && y <= 0        = Storage  -- storage
   | x >= -2 && y == 0        = Box  -- box
   | otherwise                = Ground  -- ground

-- Za dużo boxów.
badMaze2 :: Maze
badMaze2 = Maze start map where
  start = Coord 0 1
  map (Coord x y)
   | abs x > 4  || abs y > 4  = Blank  -- blank
   | abs x == 4 || abs y == 4 = Wall  -- wall
   | x ==  3 && y <= 0        = Storage  -- storage
   | x >= -2 && y == 0        = Box  -- box
   | otherwise                = Ground  -- ground

-- Za dużo boxów.
badMaze3 :: Maze
badMaze3 = Maze start map where
  start = Coord 0 1
  map (Coord x y)
   | abs x > 4  || abs y > 4  = Blank  -- blank
   | abs x == 4 || abs y == 4 = Wall  -- wall
   | x ==  2 && y <= 0        = Wall  -- wall
   | x ==  3 && y <= (-1)     = Storage  -- storage
   | x >= -2 && y == 0        = Box  -- box
   | otherwise                = Ground  -- ground

badMaze4 :: Maze
badMaze4 = Maze (Coord 0 0) (\c -> Blank)

badMaze5 :: Maze
badMaze5 = Maze (Coord 0 0) (\c -> Wall)

mazes :: [Maze]
mazes = [goodMaze1, goodMaze2, goodMaze3, goodMaze4]
badMazes :: [Maze]
badMazes = [badMaze1, badMaze2, badMaze3, badMaze4, badMaze5]

-- maze functions --------------------------------------------------------------

removeBoxes :: GameMap -> GameMap
removeBoxes gameMap = f . gameMap
  where f tile = if tile == Box
                 then Ground
                 else tile

addBoxes :: [Coord] -> GameMap -> GameMap
addBoxes [] gameMap = gameMap
addBoxes (boxCoord:rest) gameMap = addBoxes rest newGameMap
  where newGameMap coord = if coord == boxCoord
                           then Box
                           else gameMap coord

-- maze graph functions --------------------------------------------------------

mapNeighbours :: GameMap -> Coord -> [Coord]
mapNeighbours gameMap coord 
  = if gameMap coord == Ground || gameMap coord == Storage
      || gameMap coord == Box
    then allNeighbours
    else if gameMap coord == Wall
    then [neighbour | neighbour <- allNeighbours, gameMap neighbour == Wall]
    else []
    where 
      allNeighbours = [adjacentCoord R coord, adjacentCoord U coord, 
        adjacentCoord L coord, adjacentCoord D coord]

notBlank :: GameMap -> Coord -> Bool
notBlank gameMap coord = gameMap coord /= Blank


isClosed :: Maze -> Bool
isClosed maze = (gameMap initialPos == Ground || gameMap initialPos == Storage)
  && isGraphClosed initialPos neighbours2 notBlank2
  where
    initialPos = initPos maze
    gameMap = mazeTiles maze
    notBlank2 = notBlank gameMap
    neighbours2 = mapNeighbours gameMap


numberOfReachable :: GameMap -> Tile -> Coord -> (Coord -> [Coord]) -> Integer
numberOfReachable gameMap tile initial neighbours = listLength listOfReachableTile
  where 
    allReachable = getReachableList initial neighbours
    isTile coord = gameMap coord == tile
    listOfReachableTile = filterList isTile allReachable


isSane :: Maze -> Bool
isSane maze = numberOfReachable gameMap Storage initial neighbours 
  >= numberOfReachable gameMap Box initial neighbours
  where
    gameMap = mazeTiles maze
    initial = initPos maze
    neighbours = mapNeighbours gameMap


pictureOfBools :: [Bool] -> Picture
pictureOfBools xs = translated (-fromIntegral k / 2) (fromIntegral k) (go 0 xs)
  where n = length xs
        k = findK 0 -- k is the integer square of n
        findK i | i * i >= n = i
                | otherwise  = findK (i+1)
        go _ [] = blank
        go i (b:bs) =
          translated (fromIntegral (i `mod` k))
                     (-fromIntegral (i `div` k))
                     (pictureOfBool b)
          & go (i+1) bs

        pictureOfBool True =  colored green (solidCircle 0.4)
        pictureOfBool False = colored red   (solidCircle 0.4)

allMazes :: [Maze]  
allMazes = mazes ++ badMazes

areClosedAndSane :: [Maze] -> [Bool]
areClosedAndSane [] = []
areClosedAndSane (x:xs) = (isClosed x && isSane x):areClosedAndSane xs

etap4 :: Picture
etap4 = pictureOfBools $ areClosedAndSane allMazes


initialBoxes :: Integer -> [Coord]
initialBoxes lvl = listOfReachableBox
  where
    initMaze = nth mazes lvl
    initMap = mazeTiles initMaze
    initial = initPos initMaze
    allReachable = getReachableList initial (mapNeighbours initMap)
    isBox coord = initMap coord == Box
    listOfReachableBox = filterList isBox allReachable


-- player ----------------------------------------------------------------------

player1 :: Picture
player1 = colored white $ solidPolygon([a, b, c])
  where a = (-0.35, -0.4)
        b = (0.35, -0.4)
        c = (0, 0.4)

-- direction -------------------------------------------------------------------

data Direction = R | U | L | D deriving Eq

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord dir coord
  | dir == R = coord {coordX = coordX coord + 1}
  | dir == U = coord {coordY = coordY coord + 1}
  | dir == L = coord {coordX = coordX coord - 1}
  | dir == D = coord {coordY = coordY coord - 1}

-- state -----------------------------------------------------------------------

data State = State {gameFinished :: Bool, lvl :: Integer, playerCoord :: Coord, 
  playerDir :: Direction, boxesCoords :: [Coord], moveNum :: Integer} deriving Eq

initialCoord :: Integer -> Coord
initialCoord lvl = initPos $ nth mazes lvl

initialState :: State
initialState = State {gameFinished = False, lvl = initLvl, playerCoord = initialCoord 
  initLvl, playerDir = U, boxesCoords = initialBoxes initLvl, moveNum = 0}
  where initLvl = 0

nextLvl :: State -> State
nextLvl state
  | lvl state == listLength mazes - 1 = state {gameFinished = True}
  | otherwise = state {lvl = newLvl, playerCoord = initialCoord newLvl, 
    playerDir = U, boxesCoords = initialBoxes newLvl, moveNum = 0}
  where newLvl = (lvl state) + 1

-- check moves -----------------------------------------------------------------

-- Check tile at coord is free. 
tileFree :: State -> Coord -> Bool
tileFree state coord
  | (emptyMap coord == Ground || emptyMap coord == Storage) &&
    not(coord `elem` (boxesCoords state)) = True
  | otherwise = False
  where
    emptyMap = removeBoxes $ mazeTiles $ nth mazes (lvl state)

-- Check if adjacent coord to playerCoord in given direction is free.
nextTileFree :: State -> Direction -> Bool
nextTileFree state direction = tileFree state nextCoord
  where nextCoord = adjacentCoord direction (playerCoord state)

-- Check if adjacent coord to playerCoord in given direction is a movable box.
nextTileMovableBox :: State -> Direction -> Bool
nextTileMovableBox state direction
  | nextCoord `elem` (boxesCoords state) && tileFree state nextNextCoord = True
  | otherwise = False
  where nextCoord = adjacentCoord direction (playerCoord state)
        nextNextCoord = adjacentCoord direction nextCoord

allStorage :: GameMap -> [Coord] -> Bool
allStorage gameMap [] = True
allStorage gameMap (coord:rest) = gameMap coord == Storage && allStorage gameMap rest

isWinning :: State -> Bool
isWinning state = allStorage (mazeTiles $ nth mazes (lvl state)) (boxesCoords state)

-- move ------------------------------------------------------------------------

movePlayer :: State -> Direction -> State
movePlayer state dir = state {playerCoord = adjacentCoord dir (playerCoord state), 
  playerDir = dir, moveNum = moveNum state + 1}

moveBox :: State -> Direction -> State
moveBox state dir = state {boxesCoords = nextBoxCoord:(delete boxCoord (boxesCoords state))}
  where boxCoord = adjacentCoord dir (playerCoord state)
        nextBoxCoord = adjacentCoord dir boxCoord

go :: State -> Direction -> State
go state dir
  | nextTileFree state dir = movePlayer state dir
  | nextTileMovableBox state dir = movePlayer (moveBox state dir) dir
  | otherwise = state

textToDir :: Data.Text.Internal.Text -> (Bool, Direction)
textToDir "Right" = (True, R)
textToDir "Up" = (True, U)
textToDir "Left" = (True, L)
textToDir "Down" = (True, D)
textToDir _ = (False, D)

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) state
  | gameFinished state = state
  | key == "N" = nextLvl state
  | isWinning state && key == " " = nextLvl state
  | not $ isWinning state 
    = if fst (textToDir key) == True
      then go state $ snd (textToDir key)
      else state
handleEvent _ state = state

-- draw game -------------------------------------------------------------------

lvlEndScreen :: State -> Picture
lvlEndScreen state = lettering $ fromString $ "Poziom ukończony, liczba ruchów: " ++ moves
  where moves = show $ moveNum state

gameEndScreen :: Picture
gameEndScreen = scaled 3 3 (lettering "Game Finished!")

atCoord :: Coord -> Picture -> Picture
atCoord coord pic = translated (fromIntegral $ coordX coord)
  (fromIntegral $ coordY coord) pic

getRotation :: Direction -> Double
getRotation dir
  | dir == R = (-0.5) * pi
  | dir == U = 0 * pi 
  | dir == L = 0.5 * pi
  | otherwise = pi

drawPlayer :: State -> Picture
drawPlayer state = (atCoord (playerCoord state) $ rotated rotation player1)
  where rotation = getRotation (playerDir state)

maxCoord :: [Coord] -> Integer
maxCoord [] = 1
maxCoord (x:xs) = max (max (abs $ coordX x) (abs $ coordY x)) (maxCoord xs)

draw :: State -> Picture
draw state
  | gameFinished state = gameEndScreen
  | isWinning state = lvlEndScreen state
  | otherwise = scaled scale scale (drawPlayer state & mapPicture)
  where
    emptyMap = removeBoxes $ mazeTiles $ nth mazes (lvl state)
    mapWithBoxes = addBoxes (boxesCoords state) emptyMap
    allReachable = getReachableList (playerCoord state) (mapNeighbours mapWithBoxes)

    mapPicture = pictures[ translated (fromIntegral $ coordX coord) (fromIntegral $ coordY coord)
      $ drawTile $ addBoxes (boxesCoords state) emptyMap coord | coord <- allReachable ]

    scale = 8 / (fromIntegral $ maxCoord allReachable)


-- Activity --------------------------------------------------------------------

data Activity world = Activity {
  actState  :: world,
  actHandle :: (Event -> world -> world),
  actDraw   ::(world -> Picture)
}

resettable :: Activity s -> Activity s
resettable (Activity state0 handle draw)
  = Activity state0 handle' draw
  where handle' (KeyPress key) _ | key == "Esc" = state0
        handle' e s = handle e s

startScreen :: Picture
startScreen = etap4

data SSState world = StartScreen | Running world

withStartScreen :: Activity s -> Activity (SSState s)
withStartScreen (Activity state0 handle draw)
  = Activity state0' handle' draw'
  where
    state0' = StartScreen

    handle' (KeyPress key) StartScreen
      | key == " " = Running state0
    handle' _ StartScreen = StartScreen
    handle' e (Running s) = Running (handle e s)

    draw' StartScreen = startScreen
    draw' (Running s) = draw s


data WithUndo a = WithUndo a [a]

withUndo :: Eq a => Activity a -> Activity (WithUndo a)
withUndo (Activity state0 handle draw) = Activity state0' handle' draw' where
    state0' = WithUndo state0 []
    handle' (KeyPress key) (WithUndo s stack) | key == "U"
      = case stack of s':stack' -> WithUndo s' stack'
                      []        -> WithUndo s []
    handle' e              (WithUndo s stack)
       | s' == s = WithUndo s stack
       | otherwise = WithUndo (handle e s) (s:stack)
      where s' = handle e s
    draw' (WithUndo s _) = draw s


runActivity :: Activity s -> IO ()
runActivity (Activity state0 handle draw)
  = activityOf state0 handle draw

etap5 :: IO()
etap5 = runActivity $ resettable $
  withStartScreen $ withUndo (Activity initialState handleEvent draw)

main :: IO ()
main = etap5



