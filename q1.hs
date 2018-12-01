import Data.List

getLines :: String -> IO [String]
getLines fName = do
  rawString <- readFile fName
  return $ lines rawString

readValue :: String -> Int
readValue ('+':v) = read v :: Int
readValue ('-':v) = - (read v :: Int)
readValue _ = 0

process :: [String] -> Int
process = foldr (\n accu -> accu + readValue n) 0

answer :: String -> IO Int
answer fName = do
  numsStr <- getLines fName
  return $ process numsStr

-- part 2
getAllFrequency :: [String] -> [Int]
getAllFrequency numStrs = scanl (\accu n -> accu + readValue n) 0 (cycle numStrs)

findFirstDup :: [Int] -> [Int] -> Int
findFirstDup [] _ = 0
findFirstDup (l:ls) prevs = case (elem l prevs) of
  True -> l
  False -> findFirstDup ls (l : prevs)

answerTwo :: String -> IO Int
answerTwo fName = do
  numStr <- getLines fName
  return $ findFirstDup (getAllFrequency numStr) []


main :: IO()
main = do
  ans1 <- answer "day1.txt"
  ans2 <- answerTwo "day1.txt"
  putStrLn $ show ans1
  putStrLn $ show ans2
