import System.Environment
import Data.List.Split

data Bag = Bag { red :: Int, green :: Int, blue :: Int } deriving (Show, Eq, Ord)

bagLimit = Bag { red = 12, green = 13, blue = 14 }
emptyBag = Bag { red = 0, green = 0, blue = 0 }

putInBag :: String -> Int -> Bag -> Bag
putInBag "red" n bag = Bag { red = (red bag) + n, green = green bag, blue = blue bag }
putInBag "green" n bag = Bag { red = red bag, green = (green bag) + n, blue = blue bag }
putInBag "blue" n bag = Bag { red = red bag, green = green bag, blue = (blue bag) + n }

-- Evaluate one set. Sets are separeted with ;
evalSet :: String -> Bag
evalSet setStr = sum bags
  where
    split = splitOn ", " setStr
    nums = [read :: Int $ (splitOn " " x) !! 0 | x <- split]
    colors = [read :: Int $ (splitOn " " x) !! 1 | x <- split]
    bags = [putInBag c n emptyBag | c <- colors, n <- nums]



evalSets :: String -> Bag
evalSets sets = Bag { red = red, green = green, blue = blue }
  where
    split = splitOn ',' sets
    red = read :: Int $ split !! 0
    green = read :: Int $ split !! 1
    blue = read :: Int $ split !! 2

evalGame :: String -> Int
evalGame line
  | (red bag) > (red bagLimit) and (green bag) > (green bagLimit) and (blue bag) > (blue bagLimit)  = 0
  | otherwise = gameId
  where
    split = splitOn ": " line
    game = split !! 0
    sets = split !! 1
    gameId = read :: Int $ (splitOn " " game) !! 1

process :: [String] -> Int
process [] = 0

main :: IO ()
main = do
  args <- getArgs
  file <- readFile $ head args
  putStrLn $ show $ process $ lines file
