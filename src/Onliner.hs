{-# LANGUAGE OverloadedStrings, MultiWayIf, LambdaCase, TupleSections #-}

module Onliner where

import Data.Function
import Data.List
import Data.Char
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad
import Control.Exception
import Data.Time
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as ULBS
import Data.Aeson
import Data.Aeson.Types
import Control.Concurrent.Async
import Network.URI
import Network.HTTP.Client
import Network.HTTP.Types.Status
import Control.RateLimit
import Data.Time.Units
import Text.XML.HXT.Core
import Text.HandsomeSoup
import Text.Regex.TDFA
import Flat

onlinerTimeFormat :: String
onlinerTimeFormat = iso8601DateFormat (Just "%H:%M:%S%z")

onlinerMinRooms :: Int
onlinerMinRooms = 1

onlinerMaxRooms :: Int
onlinerMaxRooms = 6

onlinerMinPrice :: Int
onlinerMinPrice = 1

onlinerMaxPrice :: Int
onlinerMaxPrice = 1000000

onlinerPriceStep :: Int
onlinerPriceStep = 10000

onlinerFlatsUrl :: Int -> Int -> Int -> Int -> String
onlinerFlatsUrl rooms minPrice maxPrice page = escapeURIString (\c -> c /= '[' && c /= ']') $
  "https://pk.api.onliner.by/search/apartments" ++
  "?number_of_rooms[]=" ++ show rooms ++
  "&price[min]=" ++ show minPrice ++
  "&price[max]=" ++ show maxPrice ++
  "&currency=usd" ++
  "&page=" ++ show page

onlinerOneFlatUrl :: Int -> String
onlinerOneFlatUrl id' =
  "https://r.onliner.by/pk/apartments/" ++ show id'

onlinerLimit :: Millisecond
onlinerLimit = fromMicroseconds 100000

data OnlinerLocation = OnlinerLocation { onlinerLocationAddress :: String
                                       , onlinerLocationUserAddress :: String
                                       , onlinerLocationLatitude :: Double
                                       , onlinerLocationLongitude :: Double
                                       }

instance FromJSON OnlinerLocation where
  parseJSON (Object location) =
    OnlinerLocation <$>
      location .: "address" <*>
      location .: "user_address" <*>
      location .: "latitude" <*>
      location .: "longitude"
  parseJSON invalid =
    typeMismatch "OnlinerLocation" invalid

data OnlinerPriceConverted = OnlinerPriceConverted { onlinerPriceConvertedAmount :: String
                                                   , onlinerPriceConvertedCurrency :: String
                                                   }

instance FromJSON OnlinerPriceConverted where
  parseJSON (Object price) =
    OnlinerPriceConverted <$>
      price .: "amount" <*>
      price .: "currency"
  parseJSON invalid =
    typeMismatch "OnlinerPrice" invalid

data OnlinerPrice = OnlinerPrice { onlinerPriceAmount :: String
                                 , onlinerPriceCurrency :: String
                                 , onlinerPriceConverted :: Map.Map String OnlinerPriceConverted
                                 }

instance FromJSON OnlinerPrice where
  parseJSON (Object price) =
    OnlinerPrice <$>
      price .: "amount" <*>
      price .: "currency" <*>
      price .: "converted"
  parseJSON invalid =
    typeMismatch "OnlinerPrice" invalid

data OnlinerArea = OnlinerArea { onlinerAreaTotal :: Double
                               , onlinerAreaLiving :: Double
                               , onlinerAreaKitchen :: Double
                               }

instance FromJSON OnlinerArea where
  parseJSON (Object area) =
    OnlinerArea <$>
      area .: "total" <*>
      area .: "living" <*>
      area .: "kitchen"
  parseJSON invalid =
    typeMismatch "OnlinerArea" invalid

data OnlinerSellerType = OnlinerSellerTypeAgent | OnlinerSellerTypeOwner | OnlinerSellerTypeBuilder
  deriving (Eq, Show, Read)

instance FromJSON OnlinerSellerType where
  parseJSON (String "agent") =
    return OnlinerSellerTypeAgent
  parseJSON (String "owner") =
    return OnlinerSellerTypeOwner
  parseJSON (String "builder") =
    return OnlinerSellerTypeBuilder
  parseJSON invalid =
    typeMismatch "OnlinerSellerType" invalid

data OnlinerSeller = OnlinerSeller { onlinerSellerType :: OnlinerSellerType
                                   }

instance FromJSON OnlinerSeller where
  parseJSON (Object seller) =
    OnlinerSeller <$>
      seller .: "type"
  parseJSON invalid =
    typeMismatch "OnlinerSeller" invalid

data OnlinerFlat = OnlinerFlat { onlinerFlatId :: Int
                               , onlinerFlatAuthorId :: Int
                               , onlinerFlatLocation :: OnlinerLocation
                               , onlinerFlatPrice :: OnlinerPrice
                               , onlinerFlatResale :: Bool
                               , onlinerFlatNumberOfRooms :: Int
                               , onlinerFlatFloor :: Int
                               , onlinerFlatNumberOfFloors :: Int
                               , onlinerFlatArea :: OnlinerArea
                               , onlinerFlatPhoto :: String
                               , onlinerFlatSeller :: OnlinerSeller
                               , onlinerFlatCreatedAt :: ZonedTime
                               , onlinerFlatLastTimeUp :: ZonedTime
                               , onlinerFlatUpAvailableIn :: Int
                               , onlinerFlatUrl :: String
                               }

instance FromJSON OnlinerFlat where
  parseJSON (Object flat) =
    OnlinerFlat <$>
      flat .: "id" <*>
      flat .: "author_id" <*>
      flat .: "location" <*>
      flat .: "price" <*>
      flat .: "resale" <*>
      flat .: "number_of_rooms" <*>
      flat .: "floor" <*>
      flat .: "number_of_floors" <*>
      flat .: "area" <*>
      flat .: "photo" <*>
      flat .: "seller" <*>
      (flat .: "created_at" >>= onlinerParseTime) <*>
      (flat .: "last_time_up" >>= onlinerParseTime) <*>
      flat .: "up_available_in" <*>
      flat .: "url" where
        onlinerParseTime = parseTimeM False defaultTimeLocale onlinerTimeFormat
  parseJSON invalid =
    typeMismatch "OnlinerFlat" invalid

data OnlinerPage = OnlinerPage { onlinerPageLimit :: Int
                               , onlinerPageItems :: Int
                               , onlinerPageCurrent :: Int
                               , onlinerPageLast :: Int
                               }

instance FromJSON OnlinerPage where
  parseJSON (Object page) =
    OnlinerPage <$>
      page .: "limit" <*>
      page .: "items" <*>
      page .: "current" <*>
      page .: "last"
  parseJSON invalid =
    typeMismatch "OnlinerPage" invalid

data OnlinerPageResult = OnlinerPageResult { onlinerPageResultApartments :: [OnlinerFlat]
                                           , onlinerPageResultTotal :: Int
                                           , onlinerPageResultPage :: OnlinerPage
                                           }

instance FromJSON OnlinerPageResult where
  parseJSON (Object pageResult) =
    OnlinerPageResult <$>
      pageResult .: "apartments" <*>
      pageResult .: "total" <*>
      pageResult .: "page"
  parseJSON invalid =
    typeMismatch "OnlinerPageResult" invalid

sps :: String -> String
sps s = s >>= \c -> if c == ' ' then "( | )" else [c]

parseYear :: String -> Maybe Int
parseYear option =
  let yearString = concat (option =~ sps "дом [[:digit:]]{4} года$" :: [[String]])
  in read . take 4 . drop 4 <$> listToMaybe yearString

parseHouseType :: String -> Maybe HouseType
parseHouseType option | option =~ sps "^Каркасный дом" = Just HouseTypeFrame
                      | option =~ sps "^Блочный дом" = Just HouseTypeBlock
                      | option =~ sps "^Кирпичный дом" = Just HouseTypeBrick
                      | option =~ sps "^Панельный дом" = Just HouseTypePanel
                      | option =~ sps "^Монолитный дом" = Just HouseTypeSolid
                      | otherwise = Nothing

parseBalcony :: String -> Maybe Bool
parseBalcony option | option =~ sps "^С балконом" = Just True
                    | option =~ sps "^Без балкона" = Just False
                    | otherwise = Nothing

parseParking :: String -> Maybe Parking
parseParking option | option =~ sps "^Без выделенного парковочного места" = Just ParkingNone
                    | option =~ sps "^С выделенным парковочным местом на улице" = Just ParkingStreet
                    | option =~ sps "^С выделенным парковочным местом в гараже" = Just ParkingGarage
                    | otherwise = Nothing

parseCeilingHeight :: String -> Maybe Double
parseCeilingHeight option =
  let heightString = concat (option =~ sps "^Потолки [[:digit:]]*(,[[:digit:]]*)? метра" :: [[String]])
  in read . map (\c -> if c == ',' then '.' else c) . takeWhile (\c -> isDigit c || c == ',') . drop 8 <$> listToMaybe heightString

findYear :: [String] -> Maybe Int
findYear = msum . map parseYear

findHouseType :: [String] -> Maybe HouseType
findHouseType = msum . map parseHouseType

findBalcony :: [String] -> Maybe Bool
findBalcony = msum . map parseBalcony

findParking :: [String] -> Maybe Parking
findParking = msum . map parseParking

findCeilingHeight :: [String] -> Maybe Double
findCeilingHeight = msum . map parseCeilingHeight

toFlat :: OnlinerFlat -> Maybe ([String], String) -> Flat
toFlat flat ext =
  Flat { flatId = Just $ show $ onlinerFlatId flat
       , flatAuthorId = Just $ show $ onlinerFlatAuthorId flat
       , flatLatitude = Just $ onlinerLocationLatitude $ onlinerFlatLocation flat
       , flatLongitude = Just $ onlinerLocationLongitude $ onlinerFlatLocation flat
       , flatAddress = Just $ onlinerLocationAddress $ onlinerFlatLocation flat
       , flatUserAddress = Just $ onlinerLocationUserAddress $ onlinerFlatLocation flat
       , flatPrice = Just $ toPrice $ if onlinerPriceCurrency (onlinerFlatPrice flat) == "USD"
                                      then onlinerPriceAmount $ onlinerFlatPrice flat
                                      else onlinerPriceConvertedAmount $ onlinerPriceConverted (onlinerFlatPrice flat) Map.! "USD"
       , flatResale = Just $ onlinerFlatResale flat
       , flatRooms = Just $ onlinerFlatNumberOfRooms flat
       , flatTotalFloors = Just $ onlinerFlatNumberOfFloors flat
       , flatFloor = Just $ onlinerFlatFloor flat
       , flatEstateAgency = Just $ onlinerSellerType (onlinerFlatSeller flat) == OnlinerSellerTypeAgent
       , flatTotalArea = Just $ onlinerAreaTotal $ onlinerFlatArea flat
       , flatLivingArea = Just $ onlinerAreaLiving $ onlinerFlatArea flat
       , flatKitchenArea = Just $ onlinerAreaKitchen $ onlinerFlatArea flat
       , flatCreated = Just $ onlinerFlatCreatedAt flat
       , flatUpdated = Just $ onlinerFlatLastTimeUp flat
       , flatHouseType = findHouseType options
       , flatYear = findYear options
       , flatBalcony = findBalcony options
       , flatParking = findParking options
       , flatCeilingHeight = findCeilingHeight options
       , flatActual = Just $ isJust ext
       , flatCottage = Just $ description =~ sps "(У|у)часток|(К|к)оттедж"
       } where toPrice = round . (read :: String -> Double)
               options = maybe [] fst ext
               description = maybe "" snd ext

type GrabPageType = (Int, Int, Int, Int) -> IO OnlinerPageResult

grabPage :: Manager -> GrabPageType
grabPage manager (rooms, minPrice, maxPrice, page) = do
  initReq <- parseUrl $ "GET " ++ onlinerFlatsUrl rooms minPrice maxPrice page
  let req = initReq { requestHeaders = [ ("Accept", "application/json")
                                       , ("Accept-Encoding", "gzip, deflate, br")
                                       , ("User-Agent", "Mozilla/5.0 (X11; Linux x86_64; rv:46.0) Gecko/20100101 Firefox/46.0")
                                       ]
                    }
  withResponse req manager $ \response -> do
    body <- responseBody response
    return $ case eitherDecode (LBS.fromStrict body) of
      Left e -> error $ "Onliner json: " ++ e
      Right p -> p

grabAllPages :: GrabPageType -> Int -> Int -> Int -> IO [OnlinerPageResult]
grabAllPages grabPage' rooms minPrice maxPrice = do
  firstPageResult <- grabPage' (rooms, minPrice, maxPrice, 1)
  let firstPage = onlinerPageResultPage firstPageResult
  if | onlinerPageResultTotal firstPageResult == 0 ->
       return []
     | onlinerPageResultTotal firstPageResult > onlinerPageLast firstPage * onlinerPageLimit firstPage ->
       error "Onliner: too big result set; try to decrease onlinerPriceStep"
     | otherwise ->
       do otherPageResults <- forConcurrently [2 .. onlinerPageLast firstPage] $ \page ->
            grabPage' (rooms, minPrice, maxPrice, page)
          return $ firstPageResult : otherPageResults

grabAll :: GrabPageType -> IO [OnlinerFlat]
grabAll grabPage' = concat <$>
  forConcurrently [ grabAllPages grabPage' rooms price (price + onlinerPriceStep - 1)
                  | rooms <- [onlinerMinRooms .. onlinerMaxRooms]
                  , price <- [onlinerMinPrice, onlinerMinPrice + onlinerPriceStep .. onlinerMaxPrice]
                  ] (fmap $ concatMap onlinerPageResultApartments)

grabExtInfo :: Manager -> Int -> IO ([String], String)
grabExtInfo manager id' = do
  initReq <- parseUrl $ "GET " ++ onlinerOneFlatUrl id'
  let req = initReq { requestHeaders = [ ("Accept", "*/*")
                                       , ("Accept-Encoding", "gzip, deflate, br")
                                       , ("User-Agent", "Mozilla/5.0 (X11; Linux x86_64; rv:46.0) Gecko/20100101 Firefox/46.0")
                                       ]
                    }
  response <- httpLbs req manager
  let body = responseBody response
      html = parseHtml $ ULBS.toString body
      options = html >>> css "li.apartment-options__item" >>> getChildren >>> getText
      description = html >>> css "div.apartment-info__sub-line_extended-bottom" >>> getChildren >>> getText
  (,) <$> runX options <*> fmap unwords (runX description)

grabExtInfoSafe :: Manager -> Int -> IO (Maybe ([String], String))
grabExtInfoSafe manager id' =
  catchJust (\case StatusCodeException status _ _ -> if statusCode status == 404 then Just () else Nothing; _ -> Nothing)
            (Just <$> grabExtInfo manager id')
            (\_ -> do putStrLn $ "Error 404 for flat with id: " ++ show id'
                      return Nothing)

deduplicateBy :: Ord b => (a -> b) -> [a] -> [a]
deduplicateBy f = map head . groupBy ((==) `on` f) . sortBy (compare `on` f)

grab :: Manager -> IO [Flat]
grab manager = do
  grabPageLimited <- rateLimitInvocation onlinerLimit $ grabPage manager
  grabExtInfoLimited <- rateLimitInvocation onlinerLimit $ grabExtInfoSafe manager
  flats <- deduplicateBy onlinerFlatId <$> grabAll grabPageLimited
  putStrLn $ "Total flats count: " ++ show (length flats)
  flatsWithOptions <- mapConcurrently (\onlinerFlat -> fmap (onlinerFlat,) $ grabExtInfoLimited $ onlinerFlatId onlinerFlat) flats
  return $ map (uncurry toFlat) flatsWithOptions
