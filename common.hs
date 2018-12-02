module Common where

import qualified Data.Map as Map


getLines :: String -> IO [String]
getLines fName = do
  rawString <- readFile fName
  return $ lines rawString


groupBy :: Ord k => (v -> k) -> [v] -> Map.Map k [v]
groupBy key as = Map.fromListWith (++)  as' where
            as' = map ((,) <$> key <*> (:[])) as


