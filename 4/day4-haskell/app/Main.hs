import Data.List.Split
import System.Environment

data Card = Card {game :: Int, winNumbers :: [Int], cardNumbers :: [Int]} deriving (Show, Eq)
data CardIndexEntry = CardIndex {card :: Card, count :: Int} deriving (Show, Eq)
type CardIndex = [CardIndexEntry]

createCardIndex :: [Card] -> CardIndex
createCardIndex [] = []
createCardIndex cards = map (\c -> CardIndex c 1) cards

-- Increment a card in index by x
incrementCardIndex :: CardIndex -> Int -> Int -> CardIndex
incrementCardIndex [] _ _ = []
incrementCardIndex (c:cards) cardId incBy
  | cardId == (game (card c)) = (CardIndex (card c) ((count c) + incBy)) : cards
  | otherwise = c : (incrementCardIndex cards cardId incBy)

process :: Card -> Int
process c = case (length [x | x <- cardNumbers c, x `elem` winNumbers c]) of
  x | x <= 0 -> 0
  x | otherwise -> 2 ^ (x-1)

process2 :: CardIndex -> [Int]
process2 [] = []
process2 (c:rest) = (count c) : (process2 newRest)
  where
    amountToIncrement = length [x | x <- cardNumbers (card c), x `elem` winNumbers (card c)]
    cardsToIncrement = [(game (card c) + 1)..(game (card c) + amountToIncrement)]
    newRest = foldl (\acc x -> incrementCardIndex acc x (count c)) rest cardsToIncrement

parse :: String -> Card
parse line =
  let split1 = splitOn ": " line
      gameId = read (last (splitOn  " " (head split1))) :: Int
      winLine = (splitOn " | "(split1 !! 1)) !! 0
      gameLine = (splitOn " | "(split1 !! 1)) !! 1
      --winNos = map (\x -> read x :: Int) (splitOn " " winLine)
      winNos = [read x ::Int | x <- (splitOn " " winLine), x /= ""]
      --cardNos = map (\x -> read x :: Int) (splitOn " " gameLine)
      cardNos = [read x ::Int | x <- (splitOn " " gameLine), x /= ""]
  in Card gameId winNos cardNos

main :: IO ()
main = do
  args <- getArgs
  file <- readFile $ head args
  --putStrLn $ show $ sum $ map length $ map process2 $ map parse $ lines file
  let cardIndex = createCardIndex $ map parse $ lines file -- Card database (non mutable)
  putStrLn $ show $ sum $ process2 cardIndex
