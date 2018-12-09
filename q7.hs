import qualified Common as C
import Data.List
import Data.Char
import qualified Data.Map as Map

getSeq :: String -> (Char, Char)
getSeq str = (str!!5, str!!36)

allNonDeps :: [(Char,Char)] -> String
allNonDeps seqs = nub [ (c) | (c, s) <- seqs, not $ elem c allDepends]
  where allDepends = fmap snd seqs

-- ch : all_required
getMap :: [(Char, Char)] -> Map.Map Char [Char] -> Map.Map Char [Char]
getMap [] m = m
getMap ((c, ch):seqs) m = getMap seqs $ Map.update (\a -> Just (c : a)) ch m

condMet :: String -> String -> Bool
condMet accu conds = and $ fmap (\c -> elem c accu) conds

type CondMap = Map.Map Char [Char]

construct :: (CondMap, String, String) -> (CondMap, String, String)
construct (m, accu, nonDeps) =
  case Map.size m of
    0 -> (m,accu, nonDeps)
    _ -> construct (new_m, accu ++ [minAva], new_nonDeps)
  where allAvailable = [ k | k <- Map.keys m, condMet accu (m Map.! k) ] ++ nonDeps
        minAva = minimum allAvailable
        new_m = Map.delete minAva m
        new_nonDeps = nonDeps \\ [minAva]

work :: [(Char, Int)] -> ([(Char, Int)], String)
work schedule = ([ (c,t-1) | (c,t) <- schedule, t>1],
                 [ c | (c,t) <- schedule, t==1])

getWorkTime :: Char -> Int
getWorkTime ch = 61 + (ord ch) - (ord 'A')

pickAvailableJob :: (CondMap, String, String) -> Int -> [(Char, Int)]
pickAvailableJob (m, accu, nonDeps) maxNum = fmap (\j -> (j, getWorkTime j)) minAva
  where allAvailable = nub $ [ k | k <- Map.keys m, condMet accu (m Map.! k) ] ++ nonDeps
        minAva = take maxNum $ sort allAvailable

-- (Char, TimeLeft) , doneJob, condMap, nonDeps, time
tick :: ([(Char, Int)], String, CondMap, [Char], Int) -> ([(Char, Int)], String, CondMap, [Char], Int)
tick r@(schedule, did, m, nonD, num) =
    (u_schedule, did ++ strs, n_m, n_nonD, num+1)
  where (w_schedule, strs) = work schedule
        freeWorker = 5 - length w_schedule
        newJobs = pickAvailableJob (m, did++strs, nonD) freeWorker
        u_schedule = w_schedule ++ newJobs
        n_m = foldr (\(ch, _) ma -> Map.delete ch ma) m newJobs
        n_nonD = nonD \\ fmap fst newJobs

answerTwo :: String -> IO String
answerTwo fn = do
  strs <- C.getLines fn
  let seqs = fmap getSeq strs
      allChar = nub $ fmap fst seqs ++ fmap snd seqs
      allAvailable = allNonDeps seqs
      condMap = getMap seqs $ Map.fromList $ fmap (\c -> (c, [])) allChar
      ini = ([], "", condMap, allAvailable, 0)
      (_,_,_,_,n) = tick ini
      steps = (takeWhile (\(sch,_,m,_,_) -> not $ length sch + Map.size m == 0)$ iterate tick ini)
  putStrLn $ show $length steps
  return $ show n



main = putStrLn ""

answer :: String -> IO String
answer fn = do
  strs <- C.getLines fn
  let seqs = fmap getSeq strs
      allChar = nub $ fmap fst seqs ++ fmap snd seqs
      allAvailable = allNonDeps seqs
      condMap = getMap seqs $ Map.fromList $ fmap (\c -> (c, [])) allChar
      (_, result, _) = construct (condMap, "", allAvailable)
  putStrLn $ show condMap
  return $ result

