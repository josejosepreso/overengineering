module Main where

import Text.Regex.Posix
import Network.Curl (CurlOption(CurlNoProgress), curlGetString)
import System.Environment (getArgs)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import System.IO (hFlush, stdout)
import System.Process (callCommand)
import Video

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

search [] = callCommand nightz
search args = curlGetString <$> query <*> pure [CurlNoProgress True]
              >>= id >>= selectVideo playFirst . getResults . map (intercalate " ") . map words . lines . snd
  where playFirst = (not $ null args) && head args == "-A"
        query = init <$> pure (url ++ intercalate "+" (if playFirst then tail args else args) ++ url') `mappend` ytKey
        getResults [] = Nothing
        getResults (x:xs)
          | x =~ "\\.*videoId\\.*" = Just [getVideo (x:xs)] `mappend` getResults xs
          | otherwise = getResults xs

main = getArgs >>= search

url = "https://www.googleapis.com/youtube/v3/search?part=snippet&q="
url' = "&type=video&maxResults=20&key="
ytKey = readFile "/home/jose/api/youtube"
mpv = "mpv https://youtu.be/"
nightz = mpv ++ "BjJ_fH4uzRU?si=JF-QWeaTwtzbV6Do"
