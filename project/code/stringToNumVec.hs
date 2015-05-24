
-- The purpose of this proram is to extract important feautures from chunks of
-- text.

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

linkwords = ["www",
             "http",
             "info",
             "html",
             "/"];

spamwords = ["win",
             "award",
             "offer",
             "lay",
             "sex",
             "secret",
             "check",
             "online",
             "prescription",
             "congrats",
             "free",
             "girls",
             "premium",
             "adult",
             "virus"];

-- Normalizes a list to values 0 - 1
norm x = L.map (\xi -> (xi - minimum x) / (maximum x - minimum x)) x

-- Converts False to 0.0 and True to 1.0
boolToFloat :: Bool -> Float
boolToFloat False = 0.0;
boolToFloat True  = 1.0;

-- 0.0 if chunk and list does have a common element 1.0 otherwise
hasWordInList :: String -> [String] -> Float
hasWordInList chunk list = 
  boolToFloat $ L.foldl (\acc x -> (elem x list) || acc) False (words chunk);

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

-- Ratio of total number of chars / digit characters
digitRatio :: String -> Float
digitRatio chunk = 
  (fromIntegral $ ndigit chunk) / (fromIntegral $ nchars chunk)

-- Ratio of total number of chars / punctuation
punctRatio :: String -> Float
punctRatio chunk = 
  (fromIntegral $ npunct chunk) / (fromIntegral $ nchars chunk)

-- Ratio of total number of chars / punctuation
whiteRatio :: String -> Float
whiteRatio chunk = 
  (fromIntegral $ nwhite chunk) / 
  (fromIntegral $ countChars (\x -> True) chunk)

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
avgWordLen ""    = 0.0
avgWordLen chunk = average (words chunk)

-- Avg. sentence length in respect to words
avgSentenceLenInWords :: String -> Float
avgSentenceLenInWords ""    = 0.0
avgSentenceLenInWords chunk = 
  average $ L.map words $ L.filter (\x -> (length x) > 1) $ splitOn (".") chunk

-- Avg. sentence length in respect to chars
avgSentenceLenInChars :: String -> Float
avgSentenceLenInChars ""    = 0.0
avgSentenceLenInChars chunk = 
  average $ L.filter (\x -> (length x) > 1) $ splitOn (".") chunk

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
  if (head vec) > 0.01 then 
    writeFile (d ++ "/" ++ (last $ splitOn "/" name)) (vecToRows vec) 
  else 
    return ()
  writefiles d names vecs

mergeFiveByElement :: 
  [Float] -> [Float] -> [Float] -> [Float] -> [Float] -> [[Float]]
mergeFiveByElement [] _ _ _ _ = [] 
mergeFiveByElement _ [] _ _ _ = [] 
mergeFiveByElement _ _ [] _ _ = [] 
mergeFiveByElement _ _ _ [] _ = [] 
mergeFiveByElement _ _ _ _ [] = [] 
mergeFiveByElement (a:as) (b:bs) (c:cs) (d:ds) (e:es) = 
  [a, b, c, d, e] : mergeFiveByElement as bs cs ds es

mergetwo :: [[Float]] -> [[Float]] -> [[Float]]
mergetwo [] _ = []
mergetwo _ [] = [] 
mergetwo (a:as) (b:bs) = (a ++ b) : mergetwo as bs

str2NumVec chunk = 
  let 
    wlf = wordLenFreq 3 chunk
    rest =
      [
        alphaRatio chunk,
        digitRatio chunk,
        punctRatio chunk,
        whiteRatio chunk,
        realToFrac $ nOccurringWordsFreq 1 chunk,
        realToFrac $ nOccurringWordsFreq 2 chunk,
        simpsonsDMeasure chunk,
        hasWordInList chunk spamwords,
        hasWordInList chunk linkwords
      ]
  in rest ++ wlf

main :: IO ()
main = do 
  args <- getArgs
  let sourceDir = (args !! 0)
  let destDir   = (args !! 1)
  contents <- getDirectoryContents sourceDir
  let filtered  = filterDirectory sourceDir contents
  files <- mapM (\x -> readFileStrict x >>= evaluate) filtered 
  let wordLens = L.map nwords files 
  let shortWordLens = L.map nshortWords files 
  let averageWordLens = L.map avgWordLen files 
  let averageSentenceLensInWords = L.map avgSentenceLenInWords files
  let averageSentenceLensInChars = L.map avgSentenceLenInChars files
  let nwl = norm $ L.map fromIntegral wordLens
  let nswl = norm $ L.map fromIntegral shortWordLens
  let nawl = norm $ averageWordLens
  let naslic = norm averageSentenceLensInChars
  let nasliw = norm averageSentenceLensInWords
  let normcollection1 = mergeFiveByElement nwl nswl nawl naslic nasliw
  let normcollection2 = L.map (str2NumVec) files
  let numvecs = mergetwo normcollection1 normcollection2
  print $ averageWordLens
  writefiles destDir filtered numvecs

