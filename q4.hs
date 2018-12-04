import qualified Common as C
import qualified Data.Map as Map
import Data.List(groupBy, isInfixOf, sortBy)
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
getInfo str = case actArr of
                [True,_,_] -> (Sleep, schedule)
                [False, True, _] -> (WakeUp, schedule)
                [False, False, True] -> (Begin $ last schedule, init schedule)
  where schedule = readSchedule str
        actArr = fmap (\kw -> isInfixOf kw str) ["asleep", "wakes", "shift"]


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
  where record = sched Map.! (mon,day)
        updateRecord Sleep start (g, record) = Just (g, (take start record) ++ (take (60-start) $ repeat 0))
        updateRecord WakeUp start (g, record) = Just (g, (take start record) ++ (take (60-start) $ repeat 1))
        updateRecord _ _ _ = Nothing

sortSchedule :: [(Action, [Int])] -> [(Action, [Int])]
sortSchedule = sortBy (\(_,l) (_,r) -> mconcat $ zipWith compare (take 3 l) (take 3 r) )

process :: String -> IO [(Action,[Int])]
process fn = do
  strs <- C.getLines fn
  return $ foldr (\e accu -> getInfo e : accu) [] strs

allSleepTime :: Schedule -> Map.Map Int Int
allSleepTime = foldr (\(g, re) accu -> Map.alter (altFunc (count re)) g accu) Map.empty . Map.elems
               where count record = length $ filter (==0) record
                     altFunc num Nothing = Just num
                     altFunc num (Just sth) = Just $ num+sth


answer :: String -> IO String
answer fn = do
  schedulesRaw <- process fn
  let
    scheduleSorted = sortSchedule schedulesRaw
    map = foldr groupIntoMap Map.empty scheduleSorted

  putStrLn $ show $ allSleepTime map
  return "fjdskl"

