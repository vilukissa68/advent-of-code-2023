import System.Environment
import Data.List (subsequences)

numbersLong = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

rmChars :: String -> String
rmChars [] = []
rmChars s = filter (\c -> elem c ['0'..'9']) s

parse :: String -> String
parse [] = []
parse s = [head (rmChars s), last (rmChars s)]

convertLongToShort :: String -> String
convertLongToShort [] = []
convertLongToShort s = show $ snd $ head $ filter (\(x, y) -> x == s) $ zip numbersLong [1..9]

preprocess :: String -> String
preprocess [] = []
preprocess ss
  | elem (take 3 ss) numbersLong = convertLongToShort (take 3 ss) ++ preprocess (drop 1 ss)
  | elem (take 4 ss) numbersLong = convertLongToShort (take 4 ss) ++ preprocess (drop 1 ss)
  | elem (take 5 ss) numbersLong = convertLongToShort (take 5 ss) ++ preprocess (drop 1 ss)
  | otherwise = (take 1 ss) ++ preprocess (drop 1 ss)


process :: [String] -> Int
process [] = 0
process ss = sum  $ map (read . parse) ss

main :: IO ()
main = do
  args <- getArgs
  file <- readFile $ head args
  putStrLn $ show $ process $ map preprocess $ lines file
