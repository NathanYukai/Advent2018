module Common where

import qualified Data.Map as Map

insertAt :: [a] -> Int -> a -> [a]
insertAt ns idx e = take idx ns ++ [e] ++ drop idx ns

replaceAt :: [a] -> Int -> a -> [a]
replaceAt ns idx e = take idx ns ++ [e] ++ drop (idx + 1) ns

removeAt :: [a] -> Int -> [a]
removeAt ns idx = take idx ns ++ drop (idx+1) ns

getLines :: String -> IO [String]
getLines fName = do
  rawString <- readFile fName
  return $ lines rawString


groupBy :: Ord k => (v -> k) -> [v] -> Map.Map k [v]
groupBy key as = Map.fromListWith (++)  as' where
            as' = map ((,) <$> key <*> (:[])) as


