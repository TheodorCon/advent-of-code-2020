import Data.List ( sort )

data Pass = Pass
  { passRow :: Int,
    passCol :: Int
  }
  deriving (Show)

main :: IO ()
main = do
  dataString <- readFile "day-5.data"
  let rawPasses = lines dataString
      passes = map toPass rawPasses
      places = map toPlace passes
   in putStrLn ("First:\t" ++ (show . maximum $ places) ++ "\nSecond:\t" ++ (show . findMissing . sort $ places))
  where
    toPass string =
      let row = parseBinary 'F' 'B' $ take 7 string
          col = parseBinary 'L' 'R' $ drop 7 string
       in Pass row col
    toPlace (Pass row col) = row * 8 + col
    findMissing [] = error "Missing not found... Input might be incorrect"
    findMissing (x1 : x2 : xs) =
      if x2 - x1 == 1
        then findMissing xs
        else x1 + 1

parseBinary :: Char -> Char -> String -> Int
parseBinary _ _ [] = 0
parseBinary low high string =
  let capacity = 2 ^ length string
      multiplier
        | head string == high = 1
        | head string == low = 0
        | otherwise = error $ "Incorrect binary input: " ++ string
   in (capacity `div` 2) * multiplier + parseBinary low high (tail string)