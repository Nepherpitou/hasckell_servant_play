{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api where

import           Control.Monad              (liftM)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Reader.Class (asks)
import           Control.Monad.Trans        (lift)
import           Control.Monad.Writer.Class (tell)
import           Data.Aeson
import           Data.Text
import           Data.Time                  (UTCTime, getCurrentTime)
import           Database
import           GHC.Generics
import           Schema                     (Product (..), ProductEntity, ProductInput(..))
import           Servant
import           Types

-- GET /products
-- POST /products
-- {
--   name: String,
--   price: Double
-- }

type ProductAPI =
  "products" :> Get '[JSON] [ProductEntity]
  :<|> "products" :> ReqBody '[JSON] ProductInput :> Post '[JSON] ProductEntity

api :: Proxy ProductAPI
api = Proxy

appServerT :: ServerT ProductAPI AppM
appServerT = listProducts :<|> createProduct

listProducts :: AppM [ProductEntity]
listProducts = do
  conn <- asks connection
  liftIO $ putStrLn "Conn acquired"
  productsR <- liftIO $ getAllProducts conn
  case productsR of
    Left err       -> error $ show err
    Right products -> pure products

createProduct :: ProductInput -> AppM ProductEntity
createProduct (ProductInput (name, price)) = do
  conn <- asks connection
  time <- liftIO getCurrentTime
  productR <- liftIO $ insertProduct conn (Product name price time) >>= \i -> either (pure . Left) (getProductById conn) i
  case productR of
    Left err      -> error $ show err
    Right product -> pure product

listProducts2 :: AppM [ProductEntity]
listProducts2 = asks connection >>= \c -> pure []

app :: State -> Application
app state = serve api (hoistServer api (makeApp state) appServerT)
