import qualified Data.Map as Map
import Data.List (minimumBy, filter)
import Data.Ord (compare)
import Common

countDup :: String -> Int -> Bool
countDup s n = elem n dups
  where grouped = groupBy id s
        dups = fmap length $ Map.elems grouped

process :: [String] -> (Int, Int)
process ss= res
  where res = foldr reduceFunc (0,0) ss

reduceFunc :: String -> (Int, Int) -> (Int, Int)
reduceFunc s (tos, threes) = (n_to, n_three)
  where n_to = case countDup s 2 of
          True -> tos+1
          False -> tos
        n_three = case countDup s 3 of
          True -> threes +1
          False -> threes

answer :: String -> IO (Int,Int)
answer fN = do
  strs <- getLines fN
  return $ process strs

main = putStrLn ""


groupBy :: Ord k => (v -> k) -> [v] -> Map.Map k [v]
groupBy key as = Map.fromListWith (++)  as' where
            as' = map ((,) <$> key <*> (:[])) as


-- part 2
answerTwo :: String -> IO (String, String, Int)
answerTwo fn = do
  strs <- getLines fn
  let tuples = filter (\(_, _, n) -> n > 0) $ allDiffs strs
  return $ minimumBy (\(_, _, a) (_, _, b) -> compare a b) $ tuples

allDiffs :: [String] -> [(String, String, Int)]
allDiffs ss = [(sa, sb, diff sa sb 0) | sa <- ss, sb <- ss]

diff :: String -> String -> Int -> Int
diff [] [] accu = accu
diff (c:cs) (ch:chs) accu =
  case c == ch of
    True -> diff cs chs accu
    False -> diff cs chs accu+1



