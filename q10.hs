import Data.List
import qualified Common as C
type Point = (Int,Int,Int,Int)

separator :: [Char]
separator = ['a'..'z'] ++ "= <>,"

getInts :: String -> [String]
getInts str = groupBy (\a b -> elem a separator == elem b separator ) str

readPoints :: [String] -> Point
readPoints str = (x,y,vx,vy)
  where (x:y:vx:vy:[]) = fmap read ints
        ints = fmap snd $ filter (\(b, _) -> even b ) $ zip [1,2 ..] $ str

move :: Int -> Point -> Point
move t (x,y,vx,vy) = (x+ t *vx,y+ t*vy,vx,vy)

possible :: [Point] -> Bool
possible ns = not $ any (>100) allDist
  where allDist = [dist a b | a <- ns , b <- ns]

dist :: Point -> Point -> Int
dist (x,y,_,_) (xa,ya,_,_) = abs (x - xa) + abs (y - ya)

numOfTrue :: [Bool] -> Int
numOfTrue = length . filter id

firstTrue :: [Bool] -> Int -> Int
firstTrue (True:ns) n = n
firstTrue (False:ns) n = firstTrue ns (n+1)

getInfo :: [[Bool]] -> [[Bool]]
getInfo pic = fmap (take 100 . drop minx) rows
  where rows = filter (\row -> numOfTrue row > 2) pic
        minx = minimum $ fmap (\r -> firstTrue r 0) rows


toBools :: [Point] -> [[Bool]]
toBools points_ = bools
  where xs = fmap (\(x,_,_,_) -> x) points_
        ys = fmap (\(_,y,_,_) -> y) points_
        minx = minimum xs
        miny = minimum ys
        points = fmap (\(x,y,vx,vy) -> (x - minx,y - miny,vx,vy)) points_

        (mX,_,_,_) = maximumBy (\(x,_,_,_) (ax,_,_,_) -> compare x ax) points
        (mY,_,_,_) = maximumBy (\(_,y,_,_) (_,ay,_,_) -> compare y ay) points

        xys = fmap (\(x,y,_,_) -> (x,y)) points
        withX = [take mX $ repeat (x,0)| x <- [0..mX]]
        withXY = fmap (\row -> zipWith putY [0..mY] row ) withX
        putY y (x,_) = (x,y)
        bools = (fmap . fmap)  (\c -> elem c xys ) withXY

draw :: [[Bool]] -> String
draw pixs = "---\n" ++ (concat $ fmap drawLine pixs)
  where drawLine n = fmap drawPixal n ++ "\n"
        drawPixal True = '#'
        drawPixal False = '.'

answer :: String -> IO ()
answer fn = do
  strs <- C.getLines fn
  let pointsRaw = fmap readPoints $ fmap getInts strs
      xs = fmap (\(x,_,_,_) -> x) pointsRaw
      ys = fmap (\(_,y,_,_) -> y) pointsRaw
      minx = minimum xs
      miny = minimum ys
      maxX = maximum xs + abs minx
      maxY = maximum ys + abs miny
      points = fmap (\(x,y,vx,vy) -> (x + abs minx,y + abs miny,vx,vy)) pointsRaw

      minxAndVx = minimumBy (\(x,_,_,_) (ax,_,_,_) -> compare x ax) points
      maxxAndVx = maximumBy (\(x,_,_,_) (ax,_,_,_) -> compare x ax) points
      timePass = (getTimePass minxAndVx maxxAndVx) - 20

      start = fmap (move timePass) points
      iter = fmap transpose $ fmap toBools $ iterate (fmap (move 1)) start
      toSee = zipWith (\t c -> (show t) ++ c)[1..] $ fmap draw $ filter (\a -> (length a) > 3) $ fmap getInfo $ iter
  putStrLn $ show timePass
  putStrLn $ concat toSee
  return ()

getTimePass :: Point -> Point -> Int
getTimePass a@(x,_,vx,_) b@(ax,_,avx,_) = (max 0 ((abs (x-ax)) - 100)) `div` (abs vx + abs avx)
  -- dist - n * v < 100


main = do
  putStrLn ""
