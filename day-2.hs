import Data.List

data Record = Record
  { recordMin :: Int,
    recordMax :: Int,
    recordChar :: Char,
    recordPassword :: String
  }
  deriving (Show)

main :: IO ()
main = do
  dataString <- readFile "day-2.data"
  let records = map parseRecord . lines $ dataString
      legal = length . filter isLegal $ records
      actuallyLegal = length . filter isActuallyLegal $ records
   in putStrLn ("First\t:" ++ show legal ++ "\nSecond\t:" ++ show actuallyLegal)

isLegal :: Record -> Bool
isLegal (Record min max char password) =
  let occurences = length . filter (== char) $ password
   in min <= occurences && occurences <= max

isActuallyLegal :: Record -> Bool
isActuallyLegal (Record min max char password) = (length . filter (== char) $ [password !! (min - 1), password !! (max - 1)]) == 1

parseRecord :: String -> Record
parseRecord string =
  let [minMax, charColon, password] = words string
      [min, _, max] = groupBy (\x y -> '-' `notElem` [x, y]) minMax
      char = head charColon
   in Record
        { recordMin = read min,
          recordMax = read max,
          recordChar = char,
          recordPassword = password
        }
