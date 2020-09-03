{-# LANGUAGE OverloadedStrings #-}

module Flat where

import Data.Vector
import Data.Time
import Data.Csv

csvTimeFormat :: String
csvTimeFormat = iso8601DateFormat (Just "%H:%M:%S%z")

data HouseType = HouseTypeFrame | -- Каркасный.
                 HouseTypeBlock | -- Блочный.
                 HouseTypeBrick | -- Кирпичный.
                 HouseTypePanel | -- Панельный.
                 HouseTypeSolid -- Монолитный.
  deriving Eq

data Parking = ParkingNone |
               ParkingStreet |
               ParkingGarage
  deriving Eq

data Flat = Flat { flatId :: Maybe String
                 , flatAuthorId :: Maybe String
                 , flatLatitude :: Maybe Double
                 , flatLongitude :: Maybe Double
                 , flatAddress :: Maybe String
                 , flatUserAddress :: Maybe String
                 , flatPrice :: Maybe Int
                 , flatResale :: Maybe Bool
                 , flatRooms :: Maybe Int
                 , flatTotalFloors :: Maybe Int
                 , flatFloor :: Maybe Int
                 , flatEstateAgency :: Maybe Bool
                 , flatTotalArea :: Maybe Double
                 , flatLivingArea :: Maybe Double
                 , flatKitchenArea :: Maybe Double
                 , flatCreated :: Maybe ZonedTime
                 , flatUpdated :: Maybe ZonedTime
                 , flatHouseType :: Maybe HouseType
                 , flatYear :: Maybe Int
                 , flatBalcony :: Maybe Bool
                 , flatParking :: Maybe Parking
                 , flatCeilingHeight :: Maybe Double
                 , flatCottage :: Maybe Bool
                 , flatActual :: Maybe Bool
                 , flatUrl :: Maybe String
                 }

instance ToNamedRecord Flat where
  toNamedRecord flat =
    namedRecord [ "id" .= flatId flat
                , "author_id" .= flatAuthorId flat
                , "latitude" .= flatLatitude flat
                , "longitude" .= flatLongitude flat
                , "address" .= flatAddress flat
                , "user_address" .= flatUserAddress flat
                , "price" .= flatPrice flat
                , "resale" .= fmap encodeBool (flatResale flat)
                , "rooms" .= flatRooms flat
                , "total_floors" .= flatTotalFloors flat
                , "floor" .= flatFloor flat
                , "estate_agency" .= fmap encodeBool (flatEstateAgency flat)
                , "total_area" .= flatTotalArea flat
                , "living_area" .= flatLivingArea flat
                , "kitchen_area" .= flatKitchenArea flat
                , "created" .= fmap encodeTime (flatCreated flat)
                , "updated" .= fmap encodeTime (flatUpdated flat)
                , "house_type" .= fmap encodeHouseType (flatHouseType flat)
                , "year" .= flatYear flat
                , "balcony" .= fmap encodeBool (flatBalcony flat)
                , "parking" .= fmap encodeParking (flatParking flat)
                , "ceiling_height" .= flatCeilingHeight flat
                , "cottage" .= fmap encodeBool (flatCottage flat)
                , "actual" .= fmap encodeBool (flatActual flat)
                , "url" .= flatUrl flat
                ] where
      encodeBool :: Bool -> Int
      encodeBool b = if b then 1 else 0
      encodeTime :: ZonedTime -> String
      encodeTime = formatTime defaultTimeLocale csvTimeFormat
      encodeHouseType :: HouseType -> String
      encodeHouseType HouseTypeFrame = "frame"
      encodeHouseType HouseTypeBlock = "block"
      encodeHouseType HouseTypeBrick = "brick"
      encodeHouseType HouseTypePanel = "panel"
      encodeHouseType HouseTypeSolid = "solid"
      encodeParking :: Parking -> String
      encodeParking ParkingNone = "none"
      encodeParking ParkingStreet = "street"
      encodeParking ParkingGarage = "garage"

instance DefaultOrdered Flat where
  headerOrder _ = fromList [ "id"
                           , "author_id"
                           , "latitude"
                           , "longitude"
                           , "address"
                           , "user_address"
                           , "price"
                           , "resale"
                           , "rooms"
                           , "total_floors"
                           , "floor"
                           , "estate_agency"
                           , "total_area"
                           , "living_area"
                           , "kitchen_area"
                           , "created"
                           , "updated"
                           , "house_type"
                           , "year"
                           , "balcony"
                           , "parking"
                           , "ceiling_height"
                           , "cottage"
                           , "actual"
                           , "url"
                           ]
