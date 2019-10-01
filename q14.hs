import Data.Sequence as S
import Data.List as L

type State = (S.Seq Int, Int, Int)

main :: IO ()
main = do
  --9411111
  let start = rp initialState 20311612
  putStr $ show $ findRecipe2 start [1,5,7,9,0,1]

  putStrLn ""

rp f 0 = f
rp f nm = rp (process f) (nm-1)

findRecipe :: State -> Int -> S.Seq Int
findRecipe s@(nums, _, _) pos =
  case S.length nums > pos + 11 of
    True -> S.take 10 $ S.drop pos nums
    False -> findRecipe (process s) pos

process :: State -> State
process (nums, elf, dwf) = (nNums, nElf, nDwf)
  where
        newVal = (nums `S.index` elf) + (nums `S.index` dwf)
        newRecipe = case newVal >= 10 of
          True -> S.fromList [1, newVal `mod` 10]
          False -> S.fromList [newVal]
        nNums = nums S.>< newRecipe
        nElf = (elf + nums `S.index` elf + 1 ) `mod` (S.length nNums)
        nDwf = (dwf + nums `S.index` dwf + 1 ) `mod` (S.length nNums)

initialState :: State
initialState = (S.fromList [3,7], 0,1)

-- part 2
findRecipe2 :: State -> [Int] -> Int
findRecipe2 s@(nums, _,_) recipe =
  case recipe == lastFi of
    True -> Prelude.length nums
    False -> findRecipe2 (process s) recipe
  where checkIdxs = [(S.length nums-6) .. (S.length nums -1)]
        lastFi = (foldr (\idx accu-> (nums `S.index` idx) : accu) [] checkIdxs)


