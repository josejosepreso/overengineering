module Main where

import Text.Regex.Posix
import Network.Curl (CurlOption(CurlNoProgress), curlGetString)
import System.Environment (getArgs)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import System.IO (hFlush, stdout)
import System.Process (callCommand)

data Video = Video { videoId :: String
                   , title :: String
                   , channel :: String
                   , date :: String
                   }

menu :: [Video] -> Int -> String
menu [] _ = []
menu (x:xs) i = show i ++ ". "
                ++ (title x)
                ++ " (" ++ (channel x)
                ++ ", " ++ (date x) ++ ")\n"
                ++ (menu xs (succ i))

selectVideo :: Bool -> Maybe [Video] -> IO ()
selectVideo _ Nothing = pure ()
selectVideo True videoList = callCommand $ mpv ++ (videoId $ head $ fromJust videoList)
selectVideo _ videoList = do
  let list = fromJust videoList
  putStr $ menu list 0 ++ "> "
  hFlush stdout
  i <- getLine
  callCommand $ mpv ++ (videoId $ list !! (read i :: Int))

search playFirst query = curlGetString (init query) [CurlNoProgress True]
                         >>= selectVideo playFirst . getResults . map (intercalate " ") . map words . lines . snd
  where getResults :: [String] -> Maybe [Video]
        getResults [] = Nothing
        getResults (x:xs)
          | x =~ "\\.*videoId\\.*" = 
              let videoId = init . tail . last . words $ x
                  fromTitle = dropWhile (\a -> not $ a=~ "\\.*title\\.*") $ xs
                  title = drop 9 . init . head $ fromTitle
                  fromChannel = dropWhile (\a -> not $ a =~ "\\.*channelTitle\\.*") $ fromTitle
                  channel = init . init . drop 17 . head $ fromChannel
                  fromDate = dropWhile (\a -> not $ a =~ "\\.*publishTime\\.*") $ fromChannel
                  date = init . drop 16 . head $ fromDate
              in Just [Video videoId title channel date] `mappend` getResults xs
          | otherwise = getResults xs
                  
main = do
  playFirst <- (==) "-A" <$> head <$> getArgs
  let args = if playFirst then tail <$> getArgs else getArgs
  (++)
    <$> ((++) url <$> intercalate "+" <$> args)
    <*> ((++) url' <$> ytKey)
    >>= search playFirst

url = "https://www.googleapis.com/youtube/v3/search?part=snippet&q="
url' = "&type=video&maxResults=20&key="
ytKey = readFile "/home/jose/api/youtube"
mpv = "mpv https://youtu.be/"
