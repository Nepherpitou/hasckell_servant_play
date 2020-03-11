{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Api where

import           Control.Monad                                   (liftM)
import           Control.Monad.IO.Class                          (liftIO)
import           Control.Monad.Reader.Class                      (asks)
import           Control.Monad.Trans                             (lift)
import           Control.Monad.Writer.Class                      (tell)
import           Data.Aeson
import           Data.Text
import           Data.Time                                       (UTCTime,
                                                                  getCurrentTime)
import           Database
import           GHC.Generics
import           Network.HTTP.Types                              (status400,
                                                                  status500)
import           Schema                                          (Product (..),
                                                                  ProductEntity,
                                                                  ProductInput (..))
import           Servant
import           Servant.Checked.Exceptions                      (Envelope (..),
                                                                  OpenUnion,
                                                                  Throws,
                                                                  pureErrEnvelope,
                                                                  pureSuccEnvelope)
import           Servant.Checked.Exceptions.Internal.Servant.API (ErrStatus (toErrStatus))
import           Types

-- GET /products
-- POST /products
-- {
--   name: String,
--   price: Double
-- }

type LBody = ReqBody' [Required, Lenient]

type ProductAPI =
  "products" :> Get '[JSON] [ProductEntity]
  :<|> "products" :> LBody '[JSON] ProductInput :> Throws BadInputError :> Throws DatabaseError :>  Post '[JSON] ProductEntity

data BadInputError = BadInputError deriving (Show)
data DatabaseError = DatabaseError deriving (Show)

instance ToJSON BadInputError where
  toJSON e = object [ "code" .= show e ]
instance ErrStatus BadInputError where
  toErrStatus _ = status400
instance ToJSON DatabaseError where
  toJSON e = object [ "code" .= show e ]
instance ErrStatus DatabaseError where
  toErrStatus _ = status500

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

createProduct :: Either String ProductInput -> AppM (Envelope '[BadInputError, DatabaseError] ProductEntity)
createProduct (Left error) = pureErrEnvelope BadInputError
createProduct (Right (ProductInput (name, price))) = do
  conn <- asks connection
  time <- liftIO getCurrentTime
  productR <- liftIO $ insertProduct conn (Product name price time) >>= \i -> either (pure . Left) (getProductById conn) i
  case productR of
    Left err      -> pureErrEnvelope DatabaseError
    Right product -> pureSuccEnvelope product

listProducts2 :: AppM [ProductEntity]
listProducts2 = asks connection >>= \c -> pure []

app :: State -> Application
app state = serve api (hoistServer api (makeApp state) appServerT)
