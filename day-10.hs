import Data.List (groupBy, nub, sort)

main :: IO ()
main = do
  dataString <- readFile "day-10.data"
  let values = map read $ lines dataString :: [Int]
      sortedValues = 0 : sort values ++ [maximum values + 3]
      differences = zipWith (-) (drop 1 sortedValues) sortedValues
      firstResult = (length . filter (== 1) $ differences) * (length . filter (== 3) $ differences)
      groups =
        filter (\x -> length x > 1 && 3 `notElem` x)
          . groupBy (\a b -> 3 `notElem` [a, b])
          $ differences
      secondResult = product . map (group2Branches 3) $ groups
   in putStrLn ("First:\t" ++ show firstResult ++ "\nSecond:\t" ++ show secondResult)

group2Branches :: Int -> [Int] -> Int
group2Branches max initial = length . nub $ getChildGroups max initial
  where
    getChildGroups _ [] = []
    getChildGroups limit group =
      let nextGroupsRaw = map (\index -> take index group ++ [group !! index + group !! (index + 1)] ++ drop (index + 2) group) [0 .. (length group - 2)]
          nextGroups = filter ((<= limit) . maximum) nextGroupsRaw
       in group : concatMap (getChildGroups limit) nextGroups
