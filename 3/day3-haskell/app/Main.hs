import System.Environment
import Data.List.Split
import Text.Read

data Type = Number String | Symbol String | Empty deriving (Show, Eq)
data Coord = Coord {x :: Int, y :: Int, entry :: Type} deriving (Show, Eq)

createType :: String -> Type
createType "" = Empty
createType "." = Empty
createType x = case readMaybe x :: Maybe Int of
  Just x -> Number x
  Nothinf ->  Symbol x

createRow :: String -> [Coord]
createRow [] = []
createRow row =
  let elements splitOn "." row


createField :: [String] -> [Coord]
createField [] = []
createField (x:xs) = (createRow x) ++ (createField xs)

process :: [String] -> Int
process [] = 0
process (x:xs) = 0

main :: IO ()
main = do
  args <- getArgs
  file <- readFile $ head args
  putStrLn $ show $ process $ lines file
