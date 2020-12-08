{-# LANGUAGE LambdaCase #-}

import Control.Applicative (Alternative (empty, (<|>)))
import Data.Char
import Data.List

data BagRule = BagRule
  { outerBag :: String,
    innerBags :: [(Int, String)]
  }
  deriving (Show)

data BagTree = BagTree String [BagTree] | BagLeaf String
  deriving (Show)

newtype AdventParser a = AdventParser {runParser :: [String] -> Maybe (a, [String])}

instance Functor AdventParser where
  fmap function (AdventParser parseFn) = AdventParser $ \tokensA ->
    case parseFn tokensA of
      Nothing -> Nothing
      Just (parsed, tokensB) -> Just (function parsed, tokensB)

instance Applicative AdventParser where
  pure value = AdventParser $ \tokens -> Just (value, tokens)
  pA <*> pB = AdventParser $ \tokensA ->
    case runParser pA tokensA of
      Nothing -> Nothing
      Just (valA, tokensB) -> case runParser pB tokensB of
        Nothing -> Nothing
        Just (valB, tokensC) -> Just (valA valB, tokensC)

instance Alternative AdventParser where
  empty = AdventParser $ const Nothing
  AdventParser pA <|> AdventParser pB = AdventParser $ \tokens -> pA tokens <|> pB tokens

main :: IO ()
main = do
  dataString <- readFile "day-7.data"
  let rawRules = lines dataString
      parsedRules = traverse parseRule rawRules
      bagTree = (`buildBagTree` "shiny gold") <$> parsedRules
      Just firstResult = countTreeNodes <$> bagTree
      Just secondResult = (`countInnerBags` "shiny gold") <$> parsedRules
   in putStrLn ("First:\t" ++ show firstResult ++ "\nSecond:\t" ++ show secondResult)

tokenPrs :: String -> AdventParser String
tokenPrs token = AdventParser parseToken
  where
    parseToken [] = Nothing
    parseToken tokens =
      if head tokens == token
        then Just (token, tail tokens)
        else Nothing

anyTokenPrs :: AdventParser String
anyTokenPrs = AdventParser $ \case
  token : rest -> Just (token, rest)
  [] -> Nothing

integerPrs :: AdventParser Int
integerPrs = AdventParser $ parseToken
  where
    parseToken [] = Nothing
    parseToken tokens =
      case span isDigit . head $ tokens of
        (intString, []) -> Just (read intString :: Int, tail tokens)
        _ -> Nothing

repeatPrs :: AdventParser a -> AdventParser [a]
repeatPrs parser =
  toList <$> parser <*> repeatPrs parser
    <|> (: []) <$> parser
  where
    toList el els = el : els

repeatUntilPrs :: AdventParser a -> AdventParser [String]
repeatUntilPrs parser =
  [] <$ parser
    <|> toList <$> anyTokenPrs <*> repeatUntilPrs parser
  where
    toList el els = el : els

innerBagPrs :: AdventParser (Int, String)
innerBagPrs =
  (,) <$> integerPrs <*> (unwords <$> repeatUntilPrs bagTokenPrs)
  where
    bagTokenPrs = tokenPrs "bag," <|> tokenPrs "bags," <|> tokenPrs "bag." <|> tokenPrs "bags."

bagRulePrs :: AdventParser BagRule
bagRulePrs = BagRule <$> (unwords <$> repeatUntilPrs (tokenPrs "bags") <* tokenPrs "contain") <*> (repeatPrs innerBagPrs <|> noOtherPrs)
  where
    noOtherPrs = [] <$ tokenPrs "no" <* tokenPrs "other" <* tokenPrs "bags."

parseRule :: String -> Maybe BagRule
parseRule string = fst <$> runParser bagRulePrs (words string)

buildBagTree :: [BagRule] -> String -> BagTree
buildBagTree rules root =
  let nextNodes = map outerBag $ filter (\r -> root `elem` map snd (innerBags r)) rules
   in if null nextNodes
        then BagLeaf root
        else BagTree root (map (buildBagTree rules) nextNodes)

countTreeNodes :: BagTree -> Int
countTreeNodes = length . nub . getAllContaining
  where
    fromTree = \case
      BagTree color _ -> color
      BagLeaf color -> color
    getAllContaining (BagLeaf _) = []
    getAllContaining (BagTree _ children) = map fromTree children ++ concatMap getAllContaining children

countInnerBags :: [BagRule] -> String -> Int
countInnerBags rules root =
  let nextNodes = concatMap innerBags $ filter ((== root) . outerBag) rules
   in if null nextNodes
        then 0
        else (sum . map fst $ nextNodes) + (sum . map (\node -> fst node * (countInnerBags rules . snd $ node)) $ nextNodes)
