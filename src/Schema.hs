{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Schema where

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Aeson
import           Data.ByteString.Lazy.Char8
import qualified Data.HashMap.Lazy          as HML
import           Data.Time                  (UTCTime, getCurrentTime)
import           GHC.Generics
import           Hasql.Connection

data Entity id entity =
  Entity id entity
  deriving (Show)

type ProductId = String

data Product =
  Product
    { name      :: String
    , price     :: Float
    , createdAt :: UTCTime
    }
  deriving (Show, Generic)

instance ToJSON Product
instance FromJSON Product

data ProductEntity = ProductEntity ProductId Product

newtype ProductInput = ProductInput (String, Float) deriving (Show)
instance FromJSON ProductInput where
  parseJSON = withObject "sample" $ \s -> ProductInput <$> ((,) <$> s .: "name" <*> s .: "price")

instance ToJSON ProductEntity where
  toJSON (ProductEntity pid product) = Object (HML.union (safeObject (toJSON product)) (safeObject idObject)) where
    safeObject jso = case jso of
      Object o -> o
      _        -> HML.empty
    idObject = object [ "id" .= pid ]

instance Show ProductEntity where
  show pe = unpack $ encode pe

productEntity :: ProductId -> Product -> ProductEntity
productEntity = ProductEntity

runSchema :: IO ()
runSchema = do
  currentTime <- getCurrentTime
  let product = Product "Foo" 10.0 currentTime
  let productEntity = ProductEntity "foo" product :: ProductEntity
  print productEntity
