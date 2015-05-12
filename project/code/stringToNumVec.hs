
-- The purpose of this proram is to extract important feautures from chunks of
-- text. The extracted features include:
--

module Main where
import Data.List as L
import Data.List.Split
import Data.Char 
import Data.Map 
import System.Random;
import System.Directory;
import Control.Monad;
import System.Environment;
import Control.Exception;
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Length of a string filtered by a predicate
countChars :: (Char -> Bool) -> String -> Int
countChars pred = length . L.filter pred 

-- Total number of characters not whitespace
nchars :: String -> Int
nchars = countChars (not . isSpace) 

-- Total number of whitespace characters
nwhite :: String -> Int
nwhite = countChars isSpace

-- Total number of alphabetic characters 
nalpha :: String -> Int
nalpha = countChars isAlpha

-- Total number of digit characters
ndigit :: String -> Int 
ndigit = countChars isDigit

-- Total number of punctuation characters
npunct :: String -> Int 
npunct = countChars isPunctuation

-- Ratio of total number of chars / alphabetic chars
alphaRatio :: String -> Float
alphaRatio chunk = 
  (fromIntegral $ nalpha chunk) / (fromIntegral $ nchars chunk)

-- Letter Frequency as a list of kv-pairs
charFreq :: (Char -> Bool) -> String -> [(Char, Int)]
charFreq pred = 
  toList . fromListWith (+) . L.map (\x -> (x, 1)) . L.filter pred

-- Total number of words filtered by a predicate
countWords :: (String -> Bool) -> String -> Int
countWords pred = length . L.filter pred . L.words

-- Total number of words
nwords :: String -> Int
nwords = countWords (\s -> True)

-- Total number of short words Two letters or less
nshortWords :: String -> Int
nshortWords = countWords (\x -> (length x) < 2)

-- The average length of a all elements in a list
average :: [[a]] -> Float
average list =
  let 
    lengths = L.map length list 
    s       = sum lengths
    len     = length lengths
  in 
    (fromIntegral s) / (fromIntegral len)

-- Average word length
avgWordLen :: String -> Float
avgWordLen = average . words 

-- Avg. sentence length in respect to words
avgSentenceLenInWords :: String -> Float
avgSentenceLenInWords = 
  average . L.map words . L.filter (\x -> (length x) > 1) . splitOn (".") 

-- Avg. sentence length in respect to chars
avgSentenceLenInChars :: String -> Float
avgSentenceLenInChars = 
  average . L.filter (\x -> (length x) > 1) . splitOn (".")

-- If the last character of a string is '.', remove it 
stripDot "."  = "." 
stripDot word = if last word == '.' then take (length word - 1) word else word

-- word length frequency, helpin function
wordLenFreqAux :: Int -> Int -> [(Int, Int)] -> Int -> [Float]
wordLenFreqAux n max _ _ | n == max + 1 = []
wordLenFreqAux n max [] listLen     =  
  0.0 : wordLenFreqAux (n + 1) max [] listLen
wordLenFreqAux n max woccs listLen  = 
  let 
    (x : xs) = woccs
    (len, num) = x 
    freq       = (fromIntegral num) / (fromIntegral listLen)
  in 
    if len == n then 
      freq : wordLenFreqAux (n + 1) max xs listLen
    else 
      0.0 : wordLenFreqAux (n + 1) max (x : xs) listLen

-- Word occurences, as a list of kv-pairs
wordOccurences :: Ord a => (String -> a) -> String -> [(a, Int)]
wordOccurences folder = 
  toList . fromListWith (+) . L.map (\x -> (folder $ stripDot x, 1)) . words

-- Word length frequency. Ratio of words of length n, n between 1 and n
wordLenFreq n chunk = 
  wordLenFreqAux 1 n (wordOccurences length chunk) $ length $  words chunk

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

-- Simpson’s D measure, a diversity index 
--ref: http://geographyfieldwork.com/Simpson%27sDiversityIndex.htm
simpsonsDMeasure chunk = 
  let
    n = nwords chunk 
    fstQuads acc (x, y) = y * (y - 1) + acc
    sum = L.foldl fstQuads 0 (wordOccurences id chunk)
  in 
    if n == 1 then 
      1.0 
    else 
      1 - (fromIntegral sum) / (fromIntegral (n * (n - 1)))


str2NumVec chunk = 
  let 
    wlf = wordLenFreq 15 chunk
    rest = 
      [
        realToFrac $ nchars chunk,
        realToFrac $ nwhite chunk,
        realToFrac $ nalpha chunk,
        realToFrac $ ndigit chunk,
        realToFrac $ npunct chunk,
        alphaRatio chunk,
        realToFrac $ nwords chunk,
        realToFrac $ nshortWords chunk,
        avgWordLen chunk,
        avgSentenceLenInWords chunk,
        avgSentenceLenInChars chunk,
        realToFrac $ nOccurringWordsFreq 1 chunk,
        realToFrac $ nOccurringWordsFreq 2 chunk,
        simpsonsDMeasure chunk
      ] 
  in rest ++ wlf

--unused
{-charFreq isAlphaNum chunk,-}
{-charFreq (not . isAlphaNum) chunk,-}

{-teststring2 = "hej hopp. jag heter. jag hej"-}
{-teststring3 = "sh sh sc sc sc sc sc sc sc sc bw pu sp sp sp"-}

vecToRows [] = ""
vecToRows (x : xs) = 
  (show x) ++ "\n" ++ vecToRows (xs)


filterDirectory :: String -> [String] -> [String]
filterDirectory sourcedir list = 
  L.map ((sourcedir ++ "/") ++) (L.filter (\x -> x /= "." && x /= "..") list)

readFileStrict :: FilePath -> IO String
readFileStrict = fmap T.unpack . TIO.readFile

writefiles d _ [] = return ()
writefiles d (name : names) (vec : vecs) = do
  if (head vec) > 10 then 
    writeFile (d ++ "/" ++ (last $ splitOn "/" name)) (vecToRows vec) 
  else 
    return ()
  writefiles d names vecs

main :: IO ()
main = do 
  args <- getArgs
  let sourceDir = (args !! 0)
  let destDir   = (args !! 1)
  contents <- getDirectoryContents sourceDir
  let filtered  = filterDirectory sourceDir contents
  files <- mapM (\x -> readFileStrict x >>= evaluate) filtered 
  let numvecs = L.map (str2NumVec) files  
  writefiles destDir filtered numvecs

