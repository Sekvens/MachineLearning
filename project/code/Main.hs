

-- The purpose of this proram is to extract important feautures from chunks of
-- text. The extracted features include:
--

module Main where
import qualified Data.Text as T
import Data.List as L
import Data.List.Split
import Data.Char 
import Data.Map 
import System.Random;

-- Auxillary function
-- Length of a string filtered by a predicate
countChars :: (Char -> Bool) -> String -> Int
countChars pred = length . L.filter pred 

-- Total no of characters (C)
-- number of characters in a string which are not whitespace
nchars :: String -> Int
nchars = countChars (not . isSpace) 

-- Total no of whitespace chars/C
-- number of characters in a string which are whitespace
nwhite :: String -> Int
nwhite = countChars isSpace

-- Total no of alpha chars 
-- number of characters in a string which are alphabetic
nalpha :: String -> Int
nalpha = countChars isAlpha

-- Total no of digit chars / C
-- number of characters in a string which are digits
ndigit :: String -> Int 
ndigit = countChars isDigit

-- Ratio of alpha chars
-- the float result of dividing the number of alphabetic characters in a 
-- string with the total number of characters in a string 
alphaRatio :: String -> Float
alphaRatio chunk = 
  (/) (fromIntegral $ nalpha chunk) (fromIntegral $ nchars chunk)

-- Character Frequency
-- the list containing the built map from the list of combined key/value pairs
-- with the combining function
charFreq :: (Char -> Bool) -> String -> [(Char, Int)]
charFreq pred = 
  toList . fromListWith (+) . L.map (\x -> (x, 1)) . L.filter pred

-- Auxiallay function
countWords :: (String -> Bool) -> String -> Int
countWords pred = length . L.filter pred . L.words

-- Total no of words (M)
nwords :: String -> Int
nwords = countWords (\s -> True)

-- Total no of short words/M Two letters or less
nshortWords :: String -> Int
nshortWords = countWords (\x -> (length x) < 2)

-- Auxillary function
average :: [[a]] -> Float
average list =
  let 
    lengths = L.map L.length list 
    s       = sum lengths
    len     = length lengths
  in 
    (/) (fromIntegral s) (fromIntegral len)

-- Average word length
avgWordLen :: String -> Float
avgWordLen = average . words 

-- Avg. sentence length in words
avgSentenceLenInWords :: String -> Float
avgSentenceLenInWords = 
  average . L.map words . L.filter (\x -> (length x) > 1) . splitOn (".") 

-- Avg. sentence length in chars
avgSentenceLenInChars :: String -> Float
avgSentenceLenInChars = 
  average . L.filter (\x -> (length x) > 1) . splitOn (".")

-- Auxillary function
stripDot word = if last word == '.' then take (length word - 1) word else word

-- Auxillary function
wordLenFreqAux :: Int -> [(Int, Int)] -> Int -> [Float]
wordLenFreqAux 15 _ _ = []
wordLenFreqAux n [] listLen =  0.0 : wordLenFreqAux (n + 1) [] listLen
wordLenFreqAux n woccs listLen = 
  let 
    (x : xs) = woccs
    (len, num) = x 
    freq       = (fromIntegral num) / (fromIntegral listLen)
  in 
    if len == n then 
      freq : wordLenFreqAux (n + 1) xs listLen
    else 
      0.0 : wordLenFreqAux (n + 1) (x : xs) listLen

wordOccurences :: Ord a => (String -> a) -> String -> [(a, Int)]
wordOccurences folder = 
  toList . fromListWith (+) . L.map (\x -> (folder $ stripDot x, 1)) . words

-- Word length freq. distribution/M Ratio of words of length n, n between 1 and
-- 15
wordLenFreq chunk = 
  wordLenFreqAux 1 (wordOccurences length chunk) $ length $ words chunk

-- Type Token Ratio No. Of unique Words/ M
-- TODO ?

-- List of n times occuring words 
nOccuringWords :: Int -> String -> [String]
nOccuringWords n = L.map fst . L.filter (\(x, y) -> y == n) . wordOccurences id

-- Hapax Legomena Freq. of n-occurring words
nOccurringWordsFreq :: Int -> String -> Float
nOccurringWordsFreq n chunk = 
  let 
    all   = countWords (\x -> True) chunk
    ones  = length $ nOccuringWords n chunk 
  in  
    (fromIntegral ones) / (fromIntegral all) 

comparePairs :: Eq a => [a] -> [Bool]
comparePairs [] = [] 
comparePairs [a] = [False]
comparePairs (a : b : cs) = 
  if a == b then True : comparePairs cs else False : comparePairs cs

yulesAux :: String -> [Float] -> Float
yulesAux chunk rs = 
  let 
    multiplyAndFloor n = floor $ n * (fromIntegral $ nwords chunk)
    getRelativeWord x  = stripDot $ words chunk !! multiplyAndFloor x
    randomWords        = L.map getRelativeWord rs
    truthValues        = (comparePairs randomWords)
    trues              = fromIntegral $ length $ L.filter (== True) truthValues
    falses             = (fromIntegral $ length truthValues)
  in 
     trues / falses

-- Yule’s K measure - 
   --measures the likelyhood that two nouns, chosen at random,
   --being the same. Thus it is a measure of repetiveness aswell as complexity
yules :: String -> IO ()
yules chunk = do 
  n <- randomIO :: IO Float 
  g <- newStdGen
  let arr = take 1000 $ randoms g :: [Float]
  print $ yulesAux chunk arr 


-- Simpson’s D measure -
   --Another diversity index 
   --http://geographyfieldwork.com/Simpson%27sDiversityIndex.htm
-- Frequency of punctuation 18 punctuation chars: . ، ; ? ! : ( ) – “ « » < > [
-- ] { }

teststring1 = "AAA AAB AAC asdkfj 23409 a123 alskdjf )*(*. Hej jag en ye asd wie re sask s s f r er. fixx fuasdf. hejasdf oua sclam asdfiou .asdf.  aosdiuf .aasdfou aosdiu f."

teststring2 = "hej hopp. jag heter. jag hej"

main :: IO ()
main = do 
  putStrLn $ "Analysing: " ++ teststring1
  putStrLn $ "number of chars             " 
    ++ (show $ nchars teststring1) ++ "\n"
  putStrLn $ "Whitespace characters:      " 
    ++ (show $ nwhite teststring1) ++ "\n"
  putStrLn $ "number of alpha             " 
    ++ (show $ nalpha teststring1) ++ "\n"
  putStrLn $ "number of digits            " 
    ++ (show $ ndigit teststring1) ++ "\n"
  putStrLn $ "Alpha char ratio            " 
    ++ (show $ alphaRatio teststring1) ++ "\n"
  putStrLn $ "Letter Frequency            " 
    ++ (show $ charFreq isAlphaNum teststring1) ++ "\n"
  putStrLn $ "Special character Frequency " 
    ++ (show $ charFreq (not . isAlphaNum) teststring1) ++ "\n"
  putStrLn $ "Total number of words       " 
    ++ (show $ nwords teststring1) ++ "\n"
  putStrLn $ "Total number of words       " 
  putStrLn $ "shorter than 2 letters      " 
    ++ (show $ nshortWords teststring1) ++ "\n"
  putStrLn $ "Avarage word length         " 
    ++ (show $ avgWordLen teststring1) ++ "\n"
  putStrLn $ "Avarage sentence length     " 
  putStrLn $ "In words                    " 
    ++ (show $ avgSentenceLenInWords teststring1) ++ "\n"
  putStrLn $ "Avarage sentence length     " 
  putStrLn $ "In characters               " 
    ++ (show $ avgSentenceLenInChars teststring1) ++ "\n"
  putStrLn $ "Word frequency len (1-15)   " 
    ++ (show $ wordLenFreq teststring1) ++ "\n"
  putStrLn $ "Once occuring words freq    " 
    ++ (show $ nOccurringWordsFreq 1 teststring1) ++ "\n"
  putStrLn $ "Once occuring words freq    " 
    ++ (show $ nOccurringWordsFreq 2 teststring1) ++ "\n"
  yules teststring1
