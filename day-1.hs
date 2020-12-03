{-# LANGUAGE TupleSections #-}

main :: IO ()
main = do
  dataString <- readFile "day-1.data"
  let numbers = map read . lines $ dataString :: [Int]
      pairs = formPairs numbers
      [(xP, yP)] = filter (\(x, y) -> x + y == 2020) pairs
      triplets = formTriplets numbers
      [(xT, yT, zT)] = filter (\(x, y, z) -> x + y + z == 2020) triplets
   in putStrLn ("First:\t" ++ show (xP * yP) ++ "\nSecond:\t" ++ show (xT * yT * zT))

formPairs :: [Int] -> [(Int, Int)]
formPairs [] = []
formPairs (x : xs) = map (x,) xs ++ formPairs xs

formTriplets :: [Int] -> [(Int, Int, Int)]
formTriplets [] = []
formTriplets (x : xs) = map (\(y, z) -> (x, y, z)) (formPairs xs) ++ formTriplets xs
