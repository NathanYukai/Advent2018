import qualified Text.Parsec     as P

import qualified Common as C
import Data.Char
import Control.Monad
import Data.Either
import Data.List

main = putStrLn ""

getInts :: String -> [Int]
getInts strs = fmap read $ fmap snd $ filter (\(b,_) -> odd b) $ zip [1,2..] $ grouped
  where grouped = groupBy (\a b -> isDigit a == isDigit b) strs

process :: String -> IO String
process fn = do
  strs <- C.getLines fn
  let str = head strs
      ints = getInts str
      (res) = answer ints
      res2 = answerTwo ints
  putStrLn $ show res
  putStrLn $ show res2
  return ""

type Parser = P.Parsec [Int] ()

findMeta :: Parser Int
findMeta = do
  numCh <- P.anyToken
  numMeta  <- P.anyToken
  childs   <- sum <$> replicateM numCh findMeta
  metas    <- sum <$> replicateM numMeta P.anyToken
  pure $ childs + metas

getAllChilds :: [Int] -> [Int] -> [Int]
getAllChilds nums indexes = foldr (\a accu-> (nums !! (a-1)) : accu) [] allValid
  where allValid = filter (\n -> (n -1) < length nums) indexes

findMetaComplex :: Parser Int
findMetaComplex = do
  numCh <- P.anyToken
  numMeta  <- P.anyToken
  childs   <- replicateM numCh findMetaComplex
  metas    <- replicateM numMeta P.anyToken
  let
      validChildren = getAllChilds childs metas
      res = case childs of
        [] -> sum metas
        _ -> sum $ validChildren
  pure res

answerTwo :: [Int] -> Int
answerTwo = fromRight 0 . P.parse findMetaComplex ""

answer :: [Int] -> Int
answer = fromRight 0 . P.parse findMeta ""
