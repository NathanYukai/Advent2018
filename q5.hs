import Data.Char
import Data.List
import Common

cancel :: Char -> Char -> Bool
cancel a b = abs (ord a - ord b)
             == (ord 'a' - ord 'A')

reduceFunc :: Char -> String -> String
reduceFunc ch [] = [ch]
reduceFunc ch (s:ss) =
  case cancel ch s of
    True -> ss
    False -> ch : s : ss

remove :: Char -> String -> String
remove ch str = filter (\c -> not $ isPol ch c) str

isPol :: Char -> Char -> Bool
isPol c ch = c == ch || toUpper c == ch

main = putStrLn ""

answer :: String -> IO Int
answer fn = do
  str <- getLines fn
  let line = head str
      res = foldr reduceFunc "" line
      allRemoved = fmap (\c -> remove c line) ['a','b'..'z']

  putStrLn  $ "answer1" ++ show res
  return $ minimum $ fmap length $ fmap (foldr reduceFunc "") allRemoved
