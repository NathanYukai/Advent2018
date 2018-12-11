import qualified Common as C
import qualified Deque as D
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Maybe

type Struct = (M.Map Int Int,D.Deque Int, Int, Int)
-- (scores, marbles, index, curNum, curPlayer)

process :: Struct -> Struct
process (s,m,7082500,p) = (s,m,0,p)
process (scores, marbles, marNum, p) =
  case marNum `mod` 23 of
     0 -> process (updatedScores, (D.tail special_marbles),
                   marNum+1, (p+1) `mod` length scores)
     _ -> process (scores, n_marbles, marNum+1, (p+1)`mod` length scores)
  where n_marbles = D.cons marNum $ D.shiftLeft $ D.shiftLeft marbles
        special_marbles = last $ take 8 $ iterate D.shiftRight marbles
        updatedScores = M.update (\v -> Just $ v + marNum + (fromJust $ D.head special_marbles) ) p scores

main = do
  let ini = (M.fromList $ zip [0.. 427] (repeat 0), D.fromList [0], 1, 0)
      ite =  process ini
      (scores,m ,_,_) = ite

  putStrLn $ show $ maximum $ M.elems scores
  -- putStrLn $ show $ F.toList m
