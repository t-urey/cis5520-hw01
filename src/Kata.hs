-- | Thomas Urey, Homework 1, 09/11/2024

--------------------------------------------------------------------------------
-- Problem (Data Munging Kata)
--------------------------------------------------------------------------------

module Kata where

-- libraries

import Data.Char qualified as Char
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Text.Read (readMaybe)
import HW01 (startsWithHO, mapMaybe)

-- Part One: Weather

-- >>> weatherProgram "dat/jul24.dat"
-- "12"
-- >>> weatherProgram "dat/jul23.dat"
-- "16"

-- | Checks whether the given string is a weather file line break, namely that
-- it starts with "="
-- >>> isSectionBreak "="
-- True
-- >>> isSectionBreak "   1"
-- False
isSectionBreak :: String -> Bool
isSectionBreak str =
  startsWithHO "=" str

-- | Splits the given list of lines into sections based on the isSectionBreak
-- predicate.
-- >>> splitIntoSections ["123", "===", "567", "890"]
-- [["123"],["567","890"],[]]
splitIntoSections :: [String] -> [[String]]
splitIntoSections lines =
  case lines of
    [] -> [[]]
    _ -> let (section, rest) = break isSectionBreak lines in
      section:splitIntoSections (drop 1 rest)

-- | Takes a list of lines, splits them into sections, and selects the section
-- indicated by index. 
-- >>> selectSectionNumber 1 ["123", "===", "567", "890"]
-- Just ["567","890"]
-- >>> selectSectionNumber 2 ["123", "===", "567", "890"]
-- Nothing
selectSectionNumber :: Int -> [String] -> Maybe [String]
selectSectionNumber index lines =
  let sections = splitIntoSections lines in
    let skipped = drop index sections in
      let relevant = take 1 skipped in
        case relevant of
          [] -> Nothing
          [[]] -> Nothing
          sec:_ -> Just sec

-- | Splits a line of data on any whitespace, returning Nothing if the line is
-- empty.
-- >>> parseDataLine "a b c"
-- Just ["a","b","c"]
-- >>> parseDataLine "   "
-- Nothing
parseDataLine :: String -> Maybe [String]
parseDataLine line =
  let ws = words line in
    case ws of
      [] -> Nothing
      _ -> Just ws

-- | Parses a file worth of data lines using parseDataLine, returning only 
-- valid lines.
-- >>> parseDataLines [" a b c", "", "d"]
-- [["a","b","c"],["d"]]
parseDataLines :: [String] -> [[String]]
parseDataLines lines =
  mapMaybe parseDataLine lines

-- | Extracts the relevant columns (the first three) from a split line,
-- returning Nothing if there are insufficient columns.
-- >>> extractDayMaxMin ["a", "b", "c", "d"]
-- Just ("a","b","c")
-- >>> extractDayMaxMin []
-- Nothing
extractDayMaxMin :: [String] -> Maybe (String, String, String)
extractDayMaxMin line =
  let firstThree = take 3 line in
    case firstThree of
      [day, max, min] -> Just (day, max, min)
      _ -> Nothing

-- | Parses the day, max, and min as integers, returning Nothing if any of them
-- are not integers.
-- >>> parseDayMaxMin ("1", "2", "3")
-- Just (1,2,3)
-- >>> parseDayMaxMin ("", "2", "3")
-- Nothing
parseDayMaxMin :: (String, String, String) -> Maybe (Int, Int, Int)
parseDayMaxMin (day, max, min) =
  let parsedDay = readInt day in
    let parsedMin = readInt min in
      let parsedMax = readInt max in
        case (parsedDay, parsedMax, parsedMin) of
          (Just day, Just max, Just min) -> Just (day, max, min)
          _ -> Nothing

-- | Converts a day, max, min triple into a day, spread tuple.
-- >>> toDayAndSpread (1, 3, 2)
-- Just (1,1)
toDayAndSpread :: (Int, Int, Int) -> Maybe (Int, Int)
toDayAndSpread (day, max, min) = Just (day, max - min)

-- | Finds the smallest element in a list using the given ranking function. If
-- two elements have the same ranking, return the first.
-- >>> minWith snd [(1, 2), (2, 2)]
-- Just (1,2)
-- >>> minWith snd [(1, 3), (2, 2), (3, 2)]
-- Just (2,2)
-- >>> minWith snd []
-- Nothing
minWith :: (a -> Int) -> [a] -> Maybe a
minWith rank [] = Nothing
minWith rank [x] = Just x
minWith rank (x:xs) =
  let minOfRest = minWith rank xs in
    case minOfRest of
      Nothing -> Just x
      Just y -> if rank x <= rank y then Just x else minOfRest

-- | Processes a data line into a day, spread tuple.
-- >>> dataLineToDayAndSpread "1 3 2"
-- Just (1,1)
dataLineToDayAndSpread :: String -> Maybe (Int, Int)
dataLineToDayAndSpread line =
  parseDataLine line >>= extractDayMaxMin
                     >>= parseDayMaxMin
                     >>= toDayAndSpread

weather :: String -> Maybe String
weather str =
  let strings = lines str in
    let maybeDat = selectSectionNumber 2 strings in
      case maybeDat of
        Nothing -> Nothing
        Just dat -> let daysAndSpreads = mapMaybe dataLineToDayAndSpread dat in
          let min = minWith snd daysAndSpreads in
            case min of
              Nothing -> Nothing
              Just (day, minSpread) -> Just $ show day

weatherProgram :: String -> IO String
weatherProgram file = do
  str <- readFile file
  return
    ( case weather str of
        Just result -> result
        Nothing -> "Cannot read file"
    )

-- | Use this function to parse Ints
readInt :: String -> Maybe Int
readInt = readMaybe

-- Part Two: Soccer League Table

-- >>> soccerProgram "dat/soccer23.dat"
-- "West Ham United"

(&) :: a -> (a -> b) -> b
x & f = f x

-- | Splits a line into sections using a predicate.
-- >>> splitOn (==' ') "a b c"
-- ["a","b","c"]
splitOn :: (Char -> Bool) -> String -> [String]
splitOn predicate str = 
  case dropWhile predicate str of
    [] -> []
    relevant -> 
      let (section, rest) = break predicate relevant in
        section : splitOn predicate rest

-- | Splits a line using tabs as separators, dropping lines that fail the 
-- linePredicated.
-- >>> splitAndFilterLines (\x -> length x > 0) ["", "a\tb"]
-- [["a","b"]]
splitAndFilterLines :: ([String] -> Bool) -> [String] -> [[String]]
splitAndFilterLines linePredicate lines =
  map (splitOn (=='\t')) lines & filter linePredicate

-- | The line predicate that filders for data lines in a soccer file. In
-- particular, passes lines which start with an integer.
-- >>> soccerLinePredicate ["1", "abc"]
-- True
-- >>> soccerLinePredicate ["abc"]
-- False
-- >>> soccerLinePredicate []
-- False
soccerLinePredicate :: [String] -> Bool
soccerLinePredicate [] = False
soccerLinePredicate (w:ws) =
  case readInt w of
    Nothing -> False
    Just _ -> True

-- | Extracts the string at a given index, returning Nothing if the list is too
-- short.
-- >>> extractColumnAtIndex ["a", "b"] 1
-- Just "b"
-- >>> extractColumnAtIndex ["a", "b"] 2
-- Nothing
extractColumnAtIndex :: [String] -> Int -> Maybe String
extractColumnAtIndex words column =
  let rest = List.drop column words in
    let maybeCol = List.take 1 rest in
      case maybeCol of
        [] -> Nothing
        col:_ -> Just col

-- | Extracts all columns in the first list from the second list, returning 
-- Nothing if the second list is not long enough.
-- >>> extractColumns [0, 2] ["yes", "no", "yes"]
-- Just ["yes","yes"]
-- >>> extractColumns [0, 2, 3] ["yes", "no", "yes"]
-- Nothing
extractColumns :: [Int] -> [String] -> Maybe [String]
extractColumns columns line =
  let result = map (extractColumnAtIndex line) columns in
    sequenceA result

-- | Extracts the relevant columns from a soccer line (1, 3, 5).
-- >>> extractSoccerColumns ["#", "Team", "_", "W", "_", "L"]
-- Just ["Team","W","L"]
-- >>> extractSoccerColumns ["#", "Team", "_", "W"]
-- Nothing
extractSoccerColumns :: [String] -> Maybe [String]
extractSoccerColumns = extractColumns [1, 3, 5]

-- | Validates the output of the extract function, converting to a fixed-len
-- tuple if possible.
-- >>> validateSoccerColumns ["a", "b", "c"]
-- Just ("a","b","c")
-- >>> validateSoccerColumns ["a", "b"]
-- Nothing
validateSoccerColumns :: [String] -> Maybe (String, String, String)
validateSoccerColumns [team, wins, losses] = Just (team, wins, losses)
validateSoccerColumns _ = Nothing

-- | Parses the win and loss quantities from a soccer triple.
-- >>> parseSoccerColumns ("", "1", "2")
-- Just ("",1,2)
-- >>> parseSoccerColumns ("", "1", "s")
-- Nothing
parseSoccerColumns :: (String, String, String) -> Maybe (String, Int, Int)
parseSoccerColumns (team, wins, losses) =
  let maybeWins = readInt wins in
    let maybeLosses = readInt losses in
      case (maybeWins, maybeLosses) of
        (Just w, Just l) -> Just (team, w, l)
        _ -> Nothing

-- | Convertes the team, win, loss triple into a team, spread tuple.
-- >>> computeWinLossDifference ("a", 3, 4)
-- ("a",1)
computeWinLossDifference :: (String, Int, Int) -> (String, Int)
computeWinLossDifference (team, wins, losses) =
  (team, abs (wins - losses))

-- | Infix maybe chaining operator.
(&?) :: Maybe a -> (a -> b) -> Maybe b
Nothing &? f = Nothing
Just x &? f = Just $ f x

-- | Takes a maybe maybe and makes it into a maybe.
collapseMaybe :: Maybe (Maybe a) -> Maybe a
collapseMaybe (Just (Just x)) = Just x
collapseMaybe _ = Nothing

soccer :: String -> Maybe String
soccer file =
  lines file &
  splitAndFilterLines soccerLinePredicate &
  map extractSoccerColumns &
  sequenceA &?
  map validateSoccerColumns &?
  sequenceA &
  collapseMaybe &?
  map parseSoccerColumns &?
  sequenceA &
  collapseMaybe &?
  map computeWinLossDifference &?
  minWith snd &
  collapseMaybe &?
  fst

soccerProgram :: String -> IO String
soccerProgram file = do
  str <- readFile file
  return $ case soccer str of
    Just result -> result
    Nothing -> "Cannot read file"

-- Part Three: DRY Fusion

-- | Splits lines using a character predicate and filters using a line
-- predicate.
-- >>> splitAndFilterLinesOn (==' ') (\x -> length x > 0) ["abc abc", ""]
-- [["abc","abc"]]
splitAndFilterLinesOn :: 
  (Char -> Bool) -> 
    ([String] -> Bool) -> 
      [String] -> 
        [[String]]
splitAndFilterLinesOn splitPredicate linePredicate lines =
  map (splitOn splitPredicate) lines & filter linePredicate

-- | Detects a weather data line, aka a non-empty line.
-- >>> weatherLinePredicate []
-- False
-- >>> weatherLinePredicate ["a"]
-- True
weatherLinePredicate :: [String] -> Bool
weatherLinePredicate [] = False
weatherLinePredicate _ = True

-- | Safely indexes into a list, returning Nothing if the list is too short.
-- >>> safeIndex 1 ["no", "yes"]
-- Just "yes"
-- >>> safeIndex 1 ["no"]
-- Nothing
safeIndex :: Int -> [a] -> Maybe a
safeIndex index l =
  let rest = List.drop index l in
    let maybeCol = List.take 1 rest in
      case maybeCol of
        [] -> Nothing
        col:_ -> Just col

-- | Converts a list of exactly three elements to a triple if possible.
-- >>> takeThree [1, 2, 3]
-- Just (1,2,3)
-- >>> takeThree [1, 2]
-- Nothing
takeThree :: [a] -> Maybe (a, a, a)
takeThree [a, b, c] = Just (a, b, c)
takeThree _ = Nothing

-- | Converts a triple of strings into a triple of a string, an int, and an int
-- if possible.
-- >>> parseStringIntInt ("a", "1", "2")
-- Just ("a",1,2)
-- >>> parseStringIntInt ("a", "g", "2")
-- Nothing
parseStringIntInt :: (String, String, String) -> Maybe (String, Int, Int)
parseStringIntInt (str, i1, i2) =
  let maybeI1 = readInt i1 in
    let maybeI2 = readInt i2 in
      case (maybeI1, maybeI2) of
        (Just a, Just b) -> Just (str, a, b)
        _ -> Nothing

-- | Computes the difference between the two ints in a string, int, int triple.
-- >>> computeDifference ("a", 3, 4)
-- ("a",1)
-- >>> computeDifference ("a", 4, 1)
-- ("a",3)
computeDifference :: (String, Int, Int) -> (String, Int)
computeDifference (str, a, b) =
  (str, abs (a - b))

weather2 :: String -> Maybe String
weather2 file = 
  lines file &
  splitIntoSections &
  safeIndex 2 &?
  splitAndFilterLinesOn Char.isSpace weatherLinePredicate &?
  mapMaybe (extractColumns [0, 1, 2]) &?
  mapMaybe takeThree &?
  mapMaybe parseStringIntInt &?
  map computeDifference &?
  minWith snd &
  collapseMaybe &?
  fst

soccer2 :: String -> Maybe String
soccer2 file =
  lines file &
  splitAndFilterLinesOn (=='\t') soccerLinePredicate &
  mapMaybe (extractColumns [1, 3, 5]) &
  mapMaybe takeThree &
  mapMaybe parseStringIntInt &
  map computeDifference &
  minWith snd &?
  fst

-- >>> soccerProgram2 "dat/soccer18.dat"
-- >>> soccerProgram2 "dat/soccer19.dat"
-- >>> soccerProgram2 "dat/soccer20.dat"
-- >>> soccerProgram2 "dat/soccer21.dat"
-- >>> soccerProgram2 "dat/soccer22.dat"
-- >>> soccerProgram2 "dat/soccer23.dat"
-- "Everton"
-- "Burnley"
-- "Aston Villa"
-- "Leicester City"
-- "Fulham"
-- "West Ham United"

soccerProgram2 :: String -> IO String
soccerProgram2 file = do
  str <- readFile file
  return $ case soccer2 str of
    Just result -> result
    Nothing -> "Cannot read file"

-- >>> weatherProgram2 "dat/jul19.dat"
-- >>> weatherProgram2 "dat/jul20.dat"
-- >>> weatherProgram2 "dat/jul21.dat"
-- >>> weatherProgram2 "dat/jul22.dat"
-- >>> weatherProgram2 "dat/jul23.dat"
-- >>> weatherProgram2 "dat/jul24.dat"
-- "8"
-- "10"
-- "18"
-- "26"
-- "16"
-- "12"

weatherProgram2 :: String -> IO String
weatherProgram2 file = do
  str <- readFile file
  return $ case weather2 str of
    Just result -> result
    Nothing -> "Cannot read file"

-- Kata Questions

-- To what extent did the design decisions you made when writing the original
-- programs make it easier or harder to factor out common code?

shortAnswer1 :: String
shortAnswer1 = "My initial weather program had a lot of constants and implicit \
               \predicates which made it almost impossible to re-use anything \
               \for the soccer program."

-- Was the way you wrote the second program influenced by writing the first?

shortAnswer2 :: String
shortAnswer2 = "By the time I started on the second program, I had already \
               \realized a lot of the similarities and was starting to \
               \parameterize and generalize. I was already thinking about \
               \which parts of the two programs would be the same."

-- Is factoring out as much common code as possible always a good thing? Did the
-- readability of the programs suffer because of this requirement? How about the
-- maintainability?

shortAnswer3 :: String
shortAnswer3 = "I think the readability was actually improved by factoring \
               \things out. In general, the programs just got better each time \
               \as I learned more tricks (and especially after introducing the \
               \&? chaining operator). The maintanability also improved as I \
               \further modularized and parameterized the workflow."
