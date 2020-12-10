import Control.Applicative ( Alternative((<|>)) )

data CodeLine = CodeLine
  { instruction :: String,
    parameter :: Int
  }
  deriving (Show)

main :: IO ()
main = do
  dataString <- readFile "day-8.data"
  let rawLines = lines dataString
      codeLines = map parseLine rawLines
      firstResult = executeOnce codeLines
      nopJmpCount = length . filter (\l -> instruction l `elem` ["jmp", "nop"]) $ codeLines
      codeVariations = map (replaceNopJmp codeLines) [0 .. (nopJmpCount - 1)]
      Just secondResult = foldl (\acc curr -> acc <|> executeToEnd curr) Nothing codeVariations
   in 
      putStrLn ("First:\t" ++ show firstResult ++ "\nSecond:\t" ++ show secondResult)

parseLine :: String -> CodeLine
parseLine string = case words string of
  [inst, '+' : pos] -> CodeLine inst (read pos :: Int)
  [inst, neg] -> CodeLine inst (read neg :: Int)
  _ -> error "Not sure what happened here"

executeOnce :: [CodeLine] -> Int
executeOnce lines = executeLineAt [] (map Just lines)
  where
    executeLineAt before [] = executeLineAt (take (length before - 1) before) [last before]
    executeLineAt before (line : rest) =
      case line of
        Nothing -> 0
        Just (CodeLine "nop" _) -> executeLineAt (before ++ [Nothing]) rest
        Just (CodeLine "acc" parameter) -> parameter + executeLineAt (before ++ [Nothing]) rest
        Just (CodeLine "jmp" parameter) ->
          let all = before ++ (Nothing : rest)
              jumps = length before + parameter
              newBefore = take jumps all
              newRest = drop jumps all
           in executeLineAt newBefore newRest

executeToEnd :: [CodeLine] -> Maybe Int
executeToEnd lines = executeLineAt [] (map Just lines)
  where
    executeLineAt _ [] = Just 0
    executeLineAt before (line : rest) =
      case line of
        Nothing -> Nothing
        Just (CodeLine "nop" _) -> executeLineAt (before ++ [Nothing]) rest
        Just (CodeLine "acc" parameter) -> (+ parameter) <$> executeLineAt (before ++ [Nothing]) rest
        Just (CodeLine "jmp" parameter) ->
          let all = before ++ (Nothing : rest)
              jumps = length before + parameter 
              newBefore = take jumps all
              newRest = drop jumps all
           in executeLineAt newBefore newRest

replaceNopJmp :: [CodeLine] -> Int -> [CodeLine]
replaceNopJmp = replaceHelper []
  where
    replaceHelper before (l : ls) place = case l of
      CodeLine "nop" parameter ->
        if place == 0
          then before ++ [CodeLine "jmp" parameter] ++ ls
          else replaceHelper (before ++ [l]) ls (place - 1)
      CodeLine "jmp" parameter ->
        if place == 0
          then before ++ [CodeLine "nop" parameter] ++ ls
          else replaceHelper (before ++ [l]) ls (place - 1)
      _ -> replaceHelper (before ++ [l]) ls place