
import System.Directory
import Data.List.Split
import Data.String.Utils
import Data.List
import Text.Regex
import Data.Maybe

s = 
  [
    "^(to :|cc :|subject :|Subject:)", 
    "|[0-9]{2} : [0-9]{2} (am|pm)",
    "|- - - -"
  ]

ts = concat s

formatString chunk = 
  let 
    lines             = splitOn "\n" chunk 
    doesNotMatchRegex = (not . isJust . (matchRegex $ mkRegex ts))
  in
    intercalate "\n" $ filter doesNotMatchRegex lines

main :: IO ()
main = do 
  all <- getDirectoryContents "samples"
  print all
  {-text <- readFile "samples/file5.txt"-}
  {-writeFile "samples/newfile5.txt" $ formatString text-}
  {-putStrLn $ show $ formatString text-}
