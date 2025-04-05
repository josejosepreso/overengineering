module Main where

import Text.Regex.Posix
import Network.Curl (CurlOption(CurlNoProgress), curlGetString)
import System.Environment (getArgs)
import Data.List (intercalate)
import System.IO (hFlush, stdout)
import System.Process (callCommand)
import System.IO.Unsafe
import Video

menu :: [Video] -> Int -> String
menu [] _ = []
menu (x:xs) i = show i
                ++ ". "
                ++ (title x)
                ++ " (" ++ (channel x)
                ++ ", " ++ (date x)
                ++ ")\n"
                ++ (menu xs (succ i))

selectVideo _ [] = pure ()
selectVideo True videoList = callCommand $ mpv ++ (videoId . head $ videoList)
selectVideo _ videoList = (putStr $ menu videoList 0 ++ "> ")
                          >> hFlush stdout
                          >> getLine >>= callMpv
  where callMpv index = callCommand $ mpv ++ (videoId $ videoList !! (read index :: Int))

search [] = nightz
search args = curlGetString query [CurlNoProgress True]
              >>= selectVideo playFirst
              . getResults
              . map (intercalate " ")
              . map words
              . lines
              . snd
  where playFirst = and [not . null $ args, head args == "-A"]
        query = url
                ++ intercalate "+" (if playFirst then tail args else args)
                ++ url'
                ++ ytKey
        getResults [] = []
        getResults (x:xs)
          | x =~ "\\.*videoId\\.*" = [getVideo (x:xs)] ++ getResults xs
          | otherwise = getResults xs

main = getArgs >>= search

url = "https://www.googleapis.com/youtube/v3/search?part=snippet&q="
url' = "&type=video&maxResults=20&key="
ytKey = init . unsafePerformIO . readFile $ "/home/jose/api/youtube"
mpv = "mpv https://youtu.be/"
nightz = callCommand $ mpv ++ "BjJ_fH4uzRU"
