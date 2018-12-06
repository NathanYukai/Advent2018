import qualified Common as C
import Data.List
import qualified Data.Map as M

separator :: [Char]
separator = ", "

getInts :: String -> [String]
getInts str = groupBy (\a b -> elem a separator == elem b separator ) str

readCoord :: String -> (Int, Int)
readCoord str = (l,r)
  where (l:r:[]) = fmap (read) $ fmap snd $ filter (\(b, _) -> odd b ) $ zip [1,2 ..] $ getInts str

dist :: (Int, Int) -> (Int, Int) -> Int
dist (ax,ay) (bx,by) = abs (ax-bx) + abs (ay - by)

calcScore :: [(Int, Int)] -> (Int, Int) -> Int -> Int -> Int
calcScore allCoord (x, y) xMax yMax = length $
  [(ax,ay) | ax<- [0..xMax], ay<-[0..yMax],
   (dist (x,y) (ax,ay)) < (minimum $ fmap (dist (ax,ay)) allOtherCoord)]
  where allOtherCoord = allCoord \\ [(x,y)]


calcScoreExpanded :: [(Int, Int)] -> (Int, Int) -> Int -> Int -> Int
calcScoreExpanded allCoord (x, y) xMax yMax = length $
  [(ax,ay) | ax<- [-1..xMax+1], ay<-[-1..yMax+1],
   (dist (x,y) (ax,ay)) < (minimum $ fmap (dist (ax,ay)) allOtherCoord)]
  where allOtherCoord = allCoord \\ [(x,y)]



removeOutside :: [(Int,Int)] -> [(Int, Int)]
removeOutside allCoord = filter (removeFunc [xMax,xMin] [yMax,yMin]) allCoord
  where xMax = maximum xx
        xMin = minimum xx
        yMax = maximum yy
        yMin = minimum yy
        xx = fmap fst allCoord
        yy = fmap snd allCoord
        removeFunc xs ys (x,y) =
          not $ (elem x xs) || (elem y ys)

process :: String -> IO [(Int, Int)]
process fn = do
  strs <- C.getLines fn
  let allCoord = fmap readCoord strs
  return allCoord

answer :: IO [(Int,Int)] -> IO Int
answer io= do
  allCoord <- io
  let
      xMax = maximum $ fmap fst allCoord
      yMax = maximum $ fmap snd allCoord
      allScore = fmap (\c -> calcScore allCoord c xMax yMax) allCoord
      allScoreExpanded = fmap
        (\c -> calcScoreExpanded allCoord c xMax yMax) allCoord
  return $ maximum $ filter (\e -> elem e allScore) allScoreExpanded

answerTwo :: IO [(Int,Int)] -> IO Int
answerTwo io = do
  allCoord <- io
  let res = length $ filter (<10000) $
        fmap (\c -> calcDistSum allCoord c) everyCoord
      everyCoord = [(x,y) | x <- [0..xMax], y <- [0..yMax]]
      xMax = maximum $ fmap fst allCoord
      yMax = maximum $ fmap snd allCoord
  return res

calcDistSum :: [(Int, Int)] -> (Int, Int) -> Int
calcDistSum allCoord (x, y) = sum
  $ fmap (dist (x,y)) allCoord



main = putStrLn ""
