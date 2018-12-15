import qualified Data.Map as M
import Data.List

-- user input
serial :: Int
serial = 2568

getHun :: Int -> Int
getHun n = ((n `div` 100) + 10) `mod` 10

calculatePower :: (Int,Int) -> Int
calculatePower (x,y) = (-5) + (getHun $ rankId * (rankId * y + serial))
  where rankId = x + 10


getAllPower :: Int -> Int -> M.Map (Int,Int,Int) Int
getAllPower xl yl = M.fromList $ [((x,y,0),calculatePower (x,y))| x <- [1..xl], y <- [1..yl]]


-- x,y,size
findValueAt :: (Int,Int,Int) -> (M.Map (Int,Int, Int) Int, (Int,Int,Int,Int)) -> (M.Map (Int,Int, Int) Int, (Int,Int,Int,Int))
findValueAt (x,y,size) (m,(bx,by,bs,bv)) =(new_m, new_b)
  where
        pos = [(x+size, y+size,0)] ++ [(b,y+size,0) | b <- [x..x+size-1]] ++ [(x+size,e,0) | e <- [y..y+size-1]]
        n = (m M.! (x,y,size-1))
        new_m = mayRemove $ M.insert (x,y,size) val m
        new_b = case val > bv of
          True -> (x,y,size,val)
          False -> (bx,by,bs,bv)
        val = (n + (sum $ fmap (\p -> m M.! p) pos))
        mayRemove = case size > 1 of
          True -> M.delete (x,y,size-1)
          False -> id

main = do
  let mp = getAllPower 300 300
      pos = sortBy (\(_,_,s) (_,_,ss) -> compare ss s) [(x,y,s) | s <- [1..20], x <- [1..300-s], y <- [1..300-s] ]
      (m,val) = foldr (\p -> findValueAt p) (mp,(0,0,0,0)) pos
  putStrLn $ show $ val
  putStrLn ""
