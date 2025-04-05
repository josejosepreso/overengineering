module Video where

import Text.Regex.Posix

data Video = Video { videoId :: String
                   , title :: String
                   , channel :: String
                   , date :: String
                   }

getVideo :: [String] -> Video
getVideo (x:xs) = let videoId = init . tail . last . words $ x
                      fromTitle = dropWhile (\a -> not $ a=~ "\\.*title\\.*") $ xs
                      title = drop 9 . init . head $ fromTitle
                      fromChannel = dropWhile (\a -> not $ a =~ "\\.*channelTitle\\.*") $ fromTitle
                      channel = init . init . drop 17 . head $ fromChannel
                      fromDate = dropWhile (\a -> not $ a =~ "\\.*publishTime\\.*") $ fromChannel
                      date = init . drop 16 . head $ fromDate
                  in Video videoId title channel (getDate date)

getDate :: String -> String
getDate datetime = datetime =~ "^[0-9]{4}-[0-9]{2}-[0-9]{2}" :: String
