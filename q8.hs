import qualified Common as C
import Data.Char
import Data.List

main = putStrLn ""

getInts :: String -> [Int]
getInts strs = fmap read $ fmap snd $ filter (\(b,_) -> odd b) $ zip [1,2..] $ grouped
  where grouped = groupBy (\a b -> isDigit a == isDigit b) strs

-- return siblings, metaNum, and sum
findMeta :: ([Int], Int, Int) -> Int -> ([Int], Int, Int)
findMeta ([], n, ps) _ = ([],n, ps)
findMeta (sibs, mn, prev_sum) 1 = ([], 0, su+prev_sum + (sum $ take mn sibs))
  where  su = findMetaWithSibling $ drop mn sibs
findMeta ((0:meta:rest), 0, prev_sum) num = findMeta (drop meta rest,
                                                   0, prev_sum + (sum $ take meta rest)) (num-1)
findMeta (stuff, mn, prev_sum) num =
  case child of
    0 -> findMeta (r,mn,prev_sum) num
    _ -> findMeta (resultSIbs, retMn, resultNum) (num-1)
  where  (resultSIbs, retMn, resultNum) = findMeta (rest, (meta), prev_sum + (sum $ take mn stuff)) child
         r@(child:meta:rest) = drop (mn) stuff

findMetaWithSibling :: [Int] -> (Int)
findMetaWithSibling [] = 0
findMetaWithSibling (0:numMeta:rest) = (sum (take numMeta $ reverse rest))
findMetaWithSibling (numChild:numMeta:rest) = (sf + sum (take numMeta $ reverse rest))
  where (sibs, mn, sf) = findMeta (children, 0, 0) numChild
        children = reverse $ drop numMeta $ reverse rest

process :: String -> IO String
process fn = do
  strs <- C.getLines fn
  let str = head strs
      ints = getInts str
      (res) = findMetaWithSibling ints
  putStrLn $ show res
  return ""
