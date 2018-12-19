{-# LANGUAGE RecordWildCards  #-}

import qualified Common as C
import qualified Data.Map as M
import Data.List
import Data.Monoid
import Data.Maybe
import Debug.Trace

main = answer "day13.txt"

data Tile = Vert | Hori| TopLeft| TopRight| BotLeft| BotRight| Intersec
  deriving (Eq, Show)

data Turn = TLeft| TStraight| TRight
  deriving (Eq, Show)

data Dir = Up | Down | DLeft | DRight
  deriving (Show, Eq)

type State = (M.Map (Int,Int) Tile, [Car], Int)

data Car = Car {x :: Int,
                y :: Int,
                dir :: Dir,
                nextTurn :: Turn
               }
  deriving (Show, Eq)


updateNxtTurn :: Car -> Car
updateNxtTurn c@Car{nextTurn= TLeft} =c {nextTurn = TStraight}
updateNxtTurn c@Car{nextTurn= TStraight} =c {nextTurn = TRight}
updateNxtTurn c@Car{nextTurn= TRight} =c {nextTurn = TLeft}

-- have to read from top and from left
readMapChar :: (Char, Int, Int) -> State -> State
readMapChar (' ', _,_) s = s
readMapChar (ch, xIdx, yIdx) (m,cars,_) = (nM,nCars,0)
  where nM = M.insert (xIdx,yIdx) tile m
        isTop = yIdx ==0 || M.notMember (xIdx, yIdx-1) m || not (m M.! (xIdx, yIdx-1) `elem` [Vert, Intersec])
        nCars = case ch of
          '<' -> Car {x = xIdx, y=yIdx, nextTurn = TLeft, dir = DLeft} : cars
          '>' -> Car {x = xIdx, y=yIdx, nextTurn = TLeft, dir = DRight} : cars
          'v' -> Car {x = xIdx, y=yIdx, nextTurn = TLeft, dir = Down} : cars
          '^' -> Car {x = xIdx, y=yIdx, nextTurn = TLeft, dir = Up} : cars
          _ -> cars
        tile = case ch of
          '>' -> Hori
          '<' -> Hori
          '-' -> Hori
          '|' -> Vert
          'v' -> Vert
          '^' -> Vert
          '+' -> Intersec
          '\\' -> case isTop of
            True -> TopRight
            False -> BotLeft
          '/' -> case isTop of
            True -> TopLeft
            False -> BotRight

readMap :: [String] -> State -> State
readMap strs s = foldr (\ch ms -> readMapChar ch ms) s chars
  where linesWithIdx = zip strs [0..]
        chars = sortBy (\(_,_,cy) (_,_,ay) -> compare ay cy) charsRaw
        charsRaw = concat $ fmap (\(str, sy) -> [(ch, sx, sy) | (ch, sx) <- zip str [0..]] ) linesWithIdx

tick :: State -> State
tick (m, cs, _) = singleCarTick (m,sortedCars, 0)
  where sortedCars = sortBy (\a b -> (compare (y a) (y b)) <> (compare (x a) (x b))) cs

singleCarTick :: State -> State
singleCarTick (m, cs, curCar) = case curCar == length cs of
  False -> singleCarTick (m, removeCrashed, nxtCar)
  True -> (m,cs,curCar)
  where tile = m M.! (x c, y c)
        c = cs !! curCar
        nc = moveCar c tile
        newCs = C.replaceAt cs curCar nc
        haveCrash = Nothing /= crash
        crash = getCrash newCs
        (removeCrashed, nxtCar) = case haveCrash of
          True -> (C.removeAt (C.removeAt newCs curCar) crashCarIdx, case crashCarIdx >= curCar of
                      True -> curCar
                      False -> curCar -1 )
          False -> (newCs, curCar+1)
        crashCarIdx = fst $ head $ filter (\(_,cr) -> (x cr, y cr) == fromJust crash) $ zip [0..] $ C.removeAt newCs curCar

moveUp :: Car -> Car
moveUp c@Car{..} = c{y = y-1, dir = Up}

moveDown :: Car -> Car
moveDown c@Car{..} = c{y = y+1, dir = Down}

moveLeft :: Car -> Car
moveLeft c@Car{..} = c{x = x-1, dir = DLeft}

moveRight :: Car -> Car
moveRight c@Car{..} = c{x = x+1, dir = DRight}

moveCar :: Car -> Tile -> Car
moveCar c@Car{dir = Up, nextTurn = TLeft} Intersec = updateNxtTurn $ moveLeft c
moveCar c@Car{dir = Up, nextTurn = TRight} Intersec = updateNxtTurn $ moveRight c
moveCar c@Car{dir = Up, nextTurn = TStraight} Intersec =updateNxtTurn $ moveUp c

moveCar c@Car{dir = Down, nextTurn = TLeft} Intersec =updateNxtTurn $ moveRight c
moveCar c@Car{dir = Down, nextTurn = TRight} Intersec =updateNxtTurn $ moveLeft c
moveCar c@Car{dir = Down, nextTurn = TStraight} Intersec =updateNxtTurn $ moveDown c

moveCar c@Car{dir = DLeft, nextTurn = TLeft} Intersec =updateNxtTurn $ moveDown c
moveCar c@Car{dir = DLeft, nextTurn = TRight} Intersec =updateNxtTurn $ moveUp c
moveCar c@Car{dir = DLeft, nextTurn = TStraight} Intersec =updateNxtTurn $ moveLeft c

moveCar c@Car{dir = DRight, nextTurn = TLeft} Intersec =updateNxtTurn $ moveUp c
moveCar c@Car{dir = DRight, nextTurn = TRight} Intersec =updateNxtTurn $ moveDown c
moveCar c@Car{dir = DRight, nextTurn = TStraight} Intersec =updateNxtTurn $ moveRight c


moveCar c@Car{dir = Up} Vert = moveUp c
moveCar c@Car{dir = Up} TopLeft = moveRight c
moveCar c@Car{dir = Up} TopRight = moveLeft c

moveCar c@Car{dir = Down} Vert = moveDown c
moveCar c@Car{dir = Down} BotLeft = moveRight c
moveCar c@Car{dir = Down} BotRight = moveLeft c

moveCar c@Car{dir = DLeft} Hori = moveLeft c
moveCar c@Car{dir = DLeft} BotLeft = moveUp c
moveCar c@Car{dir = DLeft} TopLeft= moveDown c

moveCar c@Car{dir = DRight} Hori = moveRight c
moveCar c@Car{dir = DRight} BotRight = moveUp c
moveCar c@Car{dir = DRight} TopRight = moveDown c

getCrash :: [Car] -> Maybe (Int,Int)
getCrash cs = case (length $ nub xys) < length xys of
  True -> Just $ head $ xys \\ (nub xys)
  False -> Nothing
  where xys = fmap (\c -> (x c,y c)) cs

answer :: String -> IO ()
answer fileName = do
  ls <- C.getLines fileName
  let m = M.empty
      stt = readMap ls (m, [], 0)
      allTicked = iterate tick stt
      (_,r,_)= head $ dropWhile (\(mp, cars, crash) -> 1 < length cars) allTicked

  putStrLn $ show $ r
  putStrLn ""

