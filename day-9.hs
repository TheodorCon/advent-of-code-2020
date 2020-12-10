main :: IO ()
main = do
  dataString <- readFile "day-9.data"
  let numbers = map read (lines dataString) :: [Int]
      preambleSize = 25
      firstResult = getFirstInvalid numbers preambleSize
      smallerSubset = takeWhile (< firstResult) numbers
      secondResult = findSetMargins smallerSubset firstResult
   in putStrLn ("First:\t" ++ show firstResult ++ "\nSecond:\t" ++ show secondResult)

getFirstInvalid :: [Int] -> Int -> Int
getFirstInvalid numbers preambleSize
  | length numbers < preambleSize = error "Input is invalid for the preamble size"
  | length numbers == preambleSize = error "No invalid number in the given data set"
  | otherwise =
    let preamble = take preambleSize numbers
        number = numbers !! max 0 preambleSize
     in if not $ checkNumber preamble number
          then number
          else getFirstInvalid (tail numbers) preambleSize

checkNumber :: [Int] -> Int -> Bool
checkNumber [_] _ = False
checkNumber (p : ps) number =
  let valid = checkHelper p ps number
   in if valid then valid else checkNumber ps number
  where
    checkHelper _ [] _ = False
    checkHelper first (second : xs) checked =
      let isValid = checked == first + second
       in if isValid then isValid else checkHelper first xs checked

findSetMargins :: [Int] -> Int -> Int
findSetMargins numbers number = marginHelper numbers number 1
  where
    marginHelper all number offset
      | offset == length all = marginHelper (tail all) number 1
      | sum (take (offset + 1) all) == number =
        let foundSet = take (offset + 1) all
         in minimum foundSet + maximum foundSet
      | otherwise = marginHelper all number (offset + 1)
