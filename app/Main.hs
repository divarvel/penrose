{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad             (join)
import qualified Data.ByteString           as BS
import           Data.ByteString.Char8     (pack)
import           Data.ByteString.Lazy      (ByteString, fromStrict)
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (unpack)
import           Data.Text.Encoding        (decodeUtf8)
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Handler.Warp  (run)
import           Penrose
import           System.Environment
import           Text.Read                 (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  let cmd = case args of
              ["server"]   -> Left ()
              [mode, w, h] -> Right (read mode, read w, read h)
              _            -> error "`server` or `Full 10 10` or `Random 10 10`"
  case cmd of
     Left _          -> run 8080 app
     Right (m, w, h) ->
       putStrLn =<< renderMatrix <$> genMatrix m w h

app :: Application
app req respond =
  let qs = queryString req
      readFromQs name def = fromMaybe def $ readFromString =<< join (lookup name qs)
      readFromString :: Read a => BS.ByteString -> Maybe a
      readFromString = readMaybe . unpack . decodeUtf8
      w, h :: Int
      w = readFromQs "w" 10
      h = readFromQs "h" 10
      m =  readFromQs "m" Full
      svg = renderMatrix <$> genMatrix m w h
      response = case qs of
          [] -> const (uncurry (responseLBS status200) index)
          _  -> responseLBS status200 [("Content-Type", "image/svg+xml")] . fromStrict . pack
  in respond =<< response <$> svg

index :: (ResponseHeaders, ByteString)
index =
  let body = "<html lang=\"en\">\
             \<head>\
             \    <meta charset=\"UTF-8\">\
             \    <title>Document</title>\
             \</head>\
             \<body>\
             \    <img src=\"?m=Random\"/>\
             \</body>\
             \</html>"
  in ([("Content-Type", "text/html")], body)
