import Common(getLines)
import qualified Common as C
import qualified Data.Map as Map
import Data.List(groupBy, filter)

-- #21 @ 243,96: 14x11
readSquares :: String -> [Int]
readSquares str = fmap (read) $ fmap snd $ filter (\(b, _) -> even b ) $ zip [1,2 ..] $ getInts str

separator :: [Char]
separator = "@,:x #"

getInts :: String -> [String]
getInts str = groupBy (\a b -> elem a separator == elem b separator ) str


replace :: [a] -> Int -> Int -> [a] -> [a]
replace thing dist len rep = take (dist) thing
                             ++ rep
                             ++ drop (len + dist) thing

takeMiddle :: [a] -> Int -> Int -> [a]
takeMiddle thing dist len = drop (dist) $ take (dist+len) thing

fillMap :: [Int] -> [[Int]] -> [[Int]]
fillMap (id:top:left:tall:wide:[]) curMap = replace curMap top tall middle
  where middle = fmap (fillRow left wide id) originMiddle
        originMiddle = takeMiddle curMap top tall

fillRow :: Int -> Int -> Int -> [Int] -> [Int]
fillRow left wide num row = replace row left wide middle
  where middle = fmap (fillFunc num) originMiddle
        originMiddle = takeMiddle row left wide

fillFunc :: Int -> Int -> Int
fillFunc num (-1) = num
fillFunc num _ = duplicatedSqaure

process :: [String] -> [[Int]] -> [[Int]]
process line curMap = foldr (\l -> fillMap (readSquares l))  curMap line

countDupSquare :: [[Int]] -> Int
countDupSquare curMap = sum $ fmap (length . filter ( == duplicatedSqaure)) curMap

answer :: String -> IO Int
answer fn = do
  strs <- getLines fn
  let initMap = take 1000 $ repeat $ take 1000 $ repeat emptySquare

  -- putStrLn $ show $ process strs initMap
  return $ countDupSquare $ process strs initMap

main = putStrLn ""

--part 2
countAppearance :: [[Int]] -> [(Int, Int)]
countAppearance nums = zip (Map.keys appMap) $ fmap length $ Map.elems appMap
  where appMap = C.groupBy id $ concat nums

answerTwo :: String -> IO String
answerTwo fn = do
  strs <- getLines fn
  let initMap = take 1000 $ repeat $ take 1000 $ repeat emptySquare
      res = process strs initMap

      fullapp = foldr (\l accu -> getFullAppearance l : accu) [] strs
      appearance = countAppearance res
      ans = filter (\e -> elem e fullapp ) appearance

  return $ show ans

getFullAppearance :: String -> (Int, Int)
getFullAppearance line = (id, w*h)
  where (id:_:_:w:h:[]) = readSquares line

duplicatedSqaure :: Int
duplicatedSqaure = 0

emptySquare :: Int
emptySquare = -1


isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral
