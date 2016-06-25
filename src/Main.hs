module Main (main) where

import Data.Time.Clock
import Data.Time.Format
import Data.Csv
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.ByteString.Lazy as BS
import Onliner

main :: IO ()
main = do
  time <- getCurrentTime
  manager <- newManager tlsManagerSettings
  flats <- grab manager
  let csv = encodeDefaultOrderedByName flats
  BS.writeFile (formatTime defaultTimeLocale "onliner-%Y-%m-%dT%H:%M:%S.csv" time) csv
