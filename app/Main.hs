module Main where

import Text.Regex.Posix
import Network.Curl
import System.Environment (getArgs)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import System.IO (hFlush, stdout)
import System.Process (callCommand)

menu :: [(String, String)] -> Int -> String
menu [] _ = []
menu (x:xs) i = show i ++ ". " ++ (snd x) ++ "\n" ++ (menu xs (succ i))

selectVideo :: Maybe [(String, String)] -> IO ()
selectVideo Nothing = pure ()
selectVideo videoList = do
  let list = fromJust videoList
  putStr $ menu list 0 ++ "> "
  hFlush stdout
  i <- getLine
  let url = "https://youtu.be/" ++ (fst $ list !! (read i :: Int))
  putStrLn url
  callCommand $ "mpv " ++ url

search query = curlGetString (init query) [CurlNoProgress True] >>= selectVideo . getResults . map (intercalate " ") . map words . lines . snd
  where getResults [] = Nothing
        getResults (x:xs)
          | x =~ "\\.*videoId\\.*" = Just [(init . tail . last . words $ x,
                                             drop 9 . init . head . dropWhile (\x -> not $ x =~ "\\.*title\\.*") $ xs)
                                          ] `mappend` getResults xs
          | otherwise = getResults xs

main = (++) <$> ((++) url <$> intercalate "+" <$> getArgs) <*> ((++) url' <$> ytKey) >>= search

url = "https://www.googleapis.com/youtube/v3/search?part=snippet&q="
url' = "&type=video&maxResults=20&key="
ytKey = readFile "/home/jose/api/youtube"
