module Main where

import Text.Regex.Posix
import Network.Curl
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
menu (x:xs) i = show i ++ ". " ++ (title x) ++ " (" ++ (channel x) ++ ", " ++ (date x) ++ ")\n" ++ (menu xs (succ i))

selectVideo :: Maybe [Video] -> IO ()
selectVideo Nothing = pure ()
selectVideo videoList = do
  putStr $ menu (fromJust videoList) 0 ++ "> "
  hFlush stdout
  i <- getLine
  callCommand $ "mpv https://youtu.be/" ++ (videoId $ (fromJust videoList) !! (read i :: Int))

search query = curlGetString (init query) [CurlNoProgress True] >>= selectVideo . getResults . map (intercalate " ") . map words . lines . snd
  where getResults [] = Nothing
        getResults (x:xs)
          | x =~ "\\.*videoId\\.*" = let videoId = init . tail . last . words $ x
                                         fromTitle = dropWhile (\x -> not $ x =~ "\\.*title\\.*") $ xs
                                         title = drop 9 . init . head $ fromTitle
                                         fromChannel = dropWhile (\x -> not $ x =~ "\\.*channelTitle\\.*") $ fromTitle
                                         channel = init . init . drop 17 . head $ fromChannel
                                         fromDate = dropWhile (\x -> not $ x =~ "\\.*publishTime\\.*") $ fromChannel
                                         date = init . drop 16 . head $ fromDate
                                     in Just [Video videoId title channel date] `mappend` getResults xs
          | otherwise = getResults xs
                  
main = (++) <$> ((++) url <$> intercalate "+" <$> getArgs) <*> ((++) url' <$> ytKey) >>= search

url = "https://www.googleapis.com/youtube/v3/search?part=snippet&q="
url' = "&type=video&maxResults=20&key="
ytKey = readFile "/home/jose/api/youtube"
