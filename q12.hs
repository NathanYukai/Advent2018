import qualified Common as C
import Data.Maybe

main = answer "day12.txt"

getPlants :: String -> [Bool]
getPlants = fmap getPlant

getPlant :: Char -> Bool
getPlant '.' = False
getPlant '#' = True

getPattern :: String -> ([Bool], Bool)
getPattern str = ((getPlants $ take 5 str), getPlant $ last str)

findAllChange :: [Bool] -> ([Bool],Bool) -> [(Int,Bool)]
findAllChange cur patn = catMaybes $ fmap (findChange cur patn) pos
  where pos = [fstT-4..lstT+4]
        fstT = fst $ head $ filter (snd) $ zip [0..] cur
        lstT = fst $ last $ filter (snd) $ zip [0..] cur

findChange :: [Bool] -> ([Bool], Bool) -> Int -> Maybe (Int,Bool)
findChange cur (patn, v) centre =
  case vals == patn of
    True -> Just $ (centre, v)
    _ -> Nothing
  where pos = [centre-2..centre+2]
        vals = fmap (cur !!) pos

process :: [Bool] -> [([Bool], Bool)] -> [Bool]
process cur patns = foldr (\(idx, v) c
                           -> C.replaceAt c idx v) cur allChanges
  where allChanges = concat $ fmap (findAllChange cur) patns

initialState :: String
initialState = "#.####...##..#....#####.##.......##.#..###.#####.###.##.###.###.#...#...##.#.##.#...#..#.##..##.#.##"

prettyPrint :: [Bool] -> String
prettyPrint = fmap pretty
  where pretty True = '#'
        pretty _ = '.'

countNum :: [Bool] -> Int
countNum bs = sum $ fmap ct $ zip [-400..] bs
  where ct (idx, True) = idx
        ct (_, False) = 0

calcDiff :: [Int] -> [Int]
calcDiff [] = [0]
calcDiff [a] = [a]
calcDiff (h:a:res) = (h - a) : (calcDiff (a:res))

answer :: String -> IO ()
answer fn = do
  rawInputs <- C.getLines fn
  let patterns_all = fmap getPattern $ drop 2 rawInputs
      patterns = filter (\(p, v) -> not $ p !! 2 == v) patterns_all
      extra = take 400 $ repeat False
      initState = extra ++ getPlants initialState ++ extra
      p1 = take 201 $ iterate (\st -> process st patterns) initState
  putStrLn $ show $ calcDiff $ fmap countNum p1
