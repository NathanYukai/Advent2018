import qualified Common as C
import System.IO
import qualified Data.Map as Map
import Data.List(groupBy, isInfixOf, sortBy, minimumBy, transpose, maximumBy, nub)
import Data.Maybe(fromJust)

main = putStrLn ""

data Action = Begin Int
            | Sleep
            | WakeUp
            deriving(Show, Eq)

-- Mon, day : Id, record([Int])
type Schedule = Map.Map (Int, Int) (Int, [Int])

-- [1518-11-01 23:58] Guard #99 begins shift
readSchedule :: String -> [Int]
readSchedule str = fmap (read) $ fmap snd $ filter (\(b, _) -> even b ) $ zip [1,2 ..] $ getInts str

separator :: [Char]
separator = "[]- :#" ++ ['a','b'..] ++ ['A','B'..]

getInts :: String -> [String]
getInts str = groupBy (\a b -> elem a separator == elem b separator ) str

getInfo :: String -> (Action, [Int])
getInfo str = case str !! 19 of
                'f' -> (Sleep, schedule)
                'w' -> (WakeUp, schedule)
                'G' -> (Begin $ last schedule, init schedule)
  where schedule = readSchedule str


-- 1 as wake up
emptyRecord :: [Int]
emptyRecord = take 60 $ repeat 1

groupIntoMap :: (Action, [Int]) -> Schedule -> Schedule
groupIntoMap (Begin guard, (y:mon:day:23:min:[])) sched = Map.insert (mon, day+1) (guard, emptyRecord) sched
groupIntoMap (Begin guard, (y:mon:day:h:min:[])) sched = Map.insert (mon, day) (guard, emptyRecord) sched
groupIntoMap (act, (y:mon:day:h:min:[])) sched = Map.update
                                              (\r -> (updateRecord act min r))
                                              (mon, day)
                                              sched
  where
        updateRecord Sleep start (g, recd) = Just (g, (take start recd) ++ (take (60-start) $ repeat 0))
        updateRecord WakeUp start (g, recd) = Just (g, (take start recd) ++ (take (60-start) $ repeat 1))

sortSchedule :: [(Action, [Int])] -> [(Action, [Int])]
sortSchedule = sortBy (\(_,l) (_,r) -> mconcat $ zipWith compare l r )

process :: String -> IO [(Action,[Int])]
process fn = do
  strs <- C.getLines fn
  return $ foldr (\e accu -> getInfo e : accu) [] strs

allSleepTime :: Schedule -> Map.Map Int Int
allSleepTime = foldr (\(g, re) accu -> Map.alter (altFunc (count re)) g accu) Map.empty . Map.elems
               where count record = length $ filter (==0) record
                     altFunc num Nothing = Just num
                     altFunc num (Just sth) = Just $ num+sth

preprocess :: String -> IO Schedule
preprocess fn = do
  schedulesRaw <- process fn
  let
    scheduleSorted = sortSchedule schedulesRaw
    record = foldl (flip groupIntoMap) Map.empty scheduleSorted
  writeFile outputFile $ show record
  return record

mostSleepMinute :: Schedule -> Int -> Int
mostSleepMinute schedule guard = fst $ maximumBy (\(m, t) (m2, t2) -> compare t t2)
                                 $ zip [0,1..] sleepTimeEachMin
  where allGuardRecord = foldr (\(g, record) accu -> record : accu) [] $
                         filter (\(g, _) -> g == guard) $ Map.elems schedule
        transed = transpose allGuardRecord
        sleepTimeEachMin = fmap (\row -> length $ filter (\n -> n==0) row) transed

answer :: Schedule -> Int
answer record = laziestGuard * (mostSleepMinute record laziestGuard)
  where
    alt = allSleepTime record
    (laziestGuard, _) = maximumBy (\(g, sleepTime) (gr, str) -> compare sleepTime str) $ Map.toList alt


outputFile :: String
outputFile = "day4Temp"

getScheduleFromFile :: IO Schedule
getScheduleFromFile = do
  str <- readFile outputFile
  return (read str :: Schedule)

--part 2
answerTwo :: IO Schedule -> IO Int
answerTwo io = do
  record <- io
  let allGuard = nub $ fmap fst $ Map.elems record
      allSleepMost = fmap (\gId -> (gId, mostSleepMinute record gId)) allGuard
      (g, time) = maximumBy (\(_, a) (_, b) -> compare a b) allSleepMost
  putStrLn $ show allGuard
  return $ g*time

