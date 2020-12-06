import Data.List

main :: IO ()
main = do
  dataString <- readFile "day-6.data"
  let groups = filter (/= [""]) . groupBy (\a b -> "" `notElem` [a, b]) . lines $ dataString
      firstResult = sum . map (length . nub . concat) $ groups
      secondResult = sum . map (length . foldl intersect ['a' .. 'z']) $ groups
   in putStrLn ("First:\t" ++ show firstResult ++ "\nSecond:\t" ++ show secondResult)