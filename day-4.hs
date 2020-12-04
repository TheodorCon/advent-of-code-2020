import Data.Char ( isDigit, isHexDigit )
import Data.Ix ( Ix(inRange) )
import Data.List ( find, groupBy )

data Passport = Passport
  { byr :: Maybe String,
    iyr :: Maybe String,
    eyr :: Maybe String,
    hgt :: Maybe String,
    hcl :: Maybe String,
    ecl :: Maybe String,
    pid :: Maybe String,
    cid :: Maybe String
  }
  deriving (Show, Eq)

main :: IO ()
main = do
  dataString <- readFile "day-4.data"
  let rawPassports = map unwords . filter (/= [""]) . groupBy (\a b -> a /= [] && b /= []) $ lines dataString
      passports = map parsePassport rawPassports
      validNumber = length . filter isValid $ passports
      reallyValidNumber = length . filter isReallyValid $ passports
   in putStrLn $ "First:\t" ++ show validNumber ++ "\nSecond:\t" ++ show reallyValidNumber

isValid :: Passport -> Bool
isValid (Passport (Just _) (Just _) (Just _) (Just _) (Just _) (Just _) (Just _) _) = True
isValid _ = False

isReallyValid :: Passport -> Bool
isReallyValid passport =
  let maybeValidList =
        [ byrValidator <$> byr passport,
          iyrValidator <$> iyr passport,
          eyrValidator <$> eyr passport,
          hgtValidator <$> hgt passport,
          hclValidator <$> hcl passport,
          eclValidator <$> ecl passport,
          pidValidator <$> pid passport
        ]
   in Just True == (and <$> sequenceA maybeValidList)
  where
    byrValidator byr = checkYear byr (1920, 2002)
    iyrValidator iyr = checkYear iyr (2010, 2020)
    eyrValidator eyr = checkYear eyr (2020, 2030)
    hgtValidator hgt =
      let (heightString, unit) = span isDigit hgt
          height = read heightString :: Int
       in case unit of
            "cm" -> inRange (150, 193) height
            "in" -> inRange (59, 76) height
            _ -> False
    hclValidator hcl =
      head hcl == '#'
        && let hex = tail hcl
               (hexDigits, empty) = span isHexDigit hex
            in length hexDigits == 6 && empty == ""
    eclValidator ecl = ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    pidValidator pid =
      let (code, empty) = span isDigit pid
       in length code == 9 && empty == ""
    checkYear string range = length string == 4 && inRange range (read string :: Int)

parsePassport :: String -> Passport
parsePassport string =
  let splitPass = words string
      tuplePass = map ((\[a, b] -> (a, b)) . (filter (/= ":") . groupBy (\a b -> ':' `notElem` [a, b]))) splitPass
   in Passport
        { byr = snd <$> find (\(a, _) -> a == "byr") tuplePass,
          iyr = snd <$> find (\(a, _) -> a == "iyr") tuplePass,
          eyr = snd <$> find (\(a, _) -> a == "eyr") tuplePass,
          hgt = snd <$> find (\(a, _) -> a == "hgt") tuplePass,
          hcl = snd <$> find (\(a, _) -> a == "hcl") tuplePass,
          ecl = snd <$> find (\(a, _) -> a == "ecl") tuplePass,
          pid = snd <$> find (\(a, _) -> a == "pid") tuplePass,
          cid = snd <$> find (\(a, _) -> a == "cid") tuplePass
        }