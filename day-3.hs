data Slope = Slope
  { slopeRight :: Int,
    slopeDown :: Int
  }

type Pattern = String

firstSlope :: Slope
firstSlope = Slope {slopeRight = 3, slopeDown = 1}

standardSlopes :: [Slope]
standardSlopes =
  [ Slope {slopeRight = 1, slopeDown = 1},
    Slope {slopeRight = 3, slopeDown = 1},
    Slope {slopeRight = 5, slopeDown = 1},
    Slope {slopeRight = 7, slopeDown = 1},
    Slope {slopeRight = 1, slopeDown = 2}
  ]

main :: IO ()
main = do
  dataString <- readFile "day-3.data"
  let patterns = lines dataString
      treesHitFirst = checkSlope firstSlope patterns
      treesHitSecond = map (`checkSlope` patterns) standardSlopes
   in putStrLn $ "First:\t" ++ show treesHitFirst ++ "\nSecond:\t" ++ show (product treesHitSecond)

checkSlope :: Slope -> [Pattern] -> Int
checkSlope slope patterns =
  let relevantPatterns = map snd $ filter (divBy (slopeDown slope) . fst) (zip [0 ..] patterns)
   in length . filter (\(x, y, z) -> isTree x y z) $ zip3 relevantPatterns [0 ..] [slopeRight slope, slopeRight slope ..]

isTree :: String -> Int -> Int -> Bool
isTree pattern patternIndex locationIndex =
  let infinitePattern = concatMap (const pattern) [0 ..]
      checkPosition = patternIndex * locationIndex
   in infinitePattern !! checkPosition == '#'

divBy :: Int -> Int -> Bool
divBy divisor number = number `mod` divisor == 0