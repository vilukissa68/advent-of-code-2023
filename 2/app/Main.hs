import System.Environment
import Data.List.Split

data Bag = Bag { red :: Int, green :: Int, blue :: Int } deriving (Show, Eq, Ord)

instance Num Bag where
    Bag r1 g1 b1 + Bag r2 g2 b2 = Bag (r1+r2) (g1+g2) (b1+b2)
    _ * _ = error "Multiplication is not defined for the Bag type."
    abs = error "abs is not defined for the Bag type."
    signum = error "signum is not defined for the Bag type."
    fromInteger n = Bag (fromInteger n) (fromInteger n) (fromInteger n)
    negate (Bag r g b) = Bag (-r) (-g) (-b)

bagLimit = Bag { red = 12, green = 13, blue = 14 }
emptyBag = Bag { red = 0, green = 0, blue = 0 }

putInBag :: String -> Int -> Bag -> Bag
putInBag "red" n bag = Bag { red = (red bag) + n, green = green bag, blue = blue bag }
putInBag "green" n bag = Bag { red = red bag, green = (green bag) + n, blue = blue bag }
putInBag "blue" n bag = Bag { red = red bag, green = green bag, blue = (blue bag) + n }

-- Take larges field in for each color
maxBag :: [Bag] -> Bag
maxBag [] = emptyBag
maxBag bags = Bag { red = maximum [red x | x <- bags], green = maximum [green x | x <- bags], blue = maximum [blue x | x <- bags] }

-- Evaluate one set. Sets are separeted with ;
evalSet :: String -> Bag
evalSet setStr = sum bags
  where
    split = splitOn ", " setStr
    nums = [read $ (splitOn " " x) !! 0 | x <- split] :: [Int]
    colors = [(splitOn " " x) !! 1 | x <- split]
    bags = [putInBag c n emptyBag | (c, n) <- zip colors nums]

evalSets :: String -> Bag
evalSets sets = maxBag bags
  where
    split = splitOn "; " sets
    bags = [evalSet x | x <- split]

evalGame :: String -> Int
evalGame line
  | (red bag) > (red bagLimit) || (green bag) > (green bagLimit) || (blue bag) > (blue bagLimit)  = 0
  | otherwise = gameId
  where
    split = splitOn ": " line
    game = head split 
    sets = split !! 1
    gameId = read $ (splitOn " " game) !! 1 :: Int
    bag = evalSets sets

evalGame2 :: String -> Int
evalGame2 line = (red bag) * (green bag) * (blue bag)
  where
    split = splitOn ": " line
    game = head split
    sets = split !! 1
    gameId = read $ (splitOn " " game) !! 1 :: Int
    bag = evalSets sets


process :: [String] -> Int
process [] = 0
--process (x:xs) = (evalGame x) + (process xs) --part 1
process (x:xs) = (evalGame2 x) + (process xs) --part 2

main :: IO ()
main = do
  args <- getArgs
  file <- readFile $ head args
  putStrLn $ show $ process $ lines file
