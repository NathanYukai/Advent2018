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

toBools :: Int -> Int -> [Point] -> [[Bool]]
toBools mX mY points = bools
  where xys = fmap (\(x,y,_,_) -> (x,y)) points
        withX = [take mX $ repeat (x,0)| x <- [0..mX]]
        withXY = fmap (\row -> zipWith putY [0..mY] row ) withX
        putY y (x,_) = (x,y)
        bools = (fmap . fmap)  (\c -> elem c xys ) withXY

draw :: [[Bool]] -> String
draw pixs = concat $ fmap drawLine pixs
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

      pictureStr = draw $ transpose $ toBools maxX maxY points
  putStrLn $ show [minx, maxX]
  putStrLn $ show points
  putStrLn $ pictureStr
  return ()


main = do

  putStrLn ""
