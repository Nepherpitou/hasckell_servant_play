{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Database where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Functor.Contravariant (contramap, (>$<))
import           Data.Text
import           Data.Time
import qualified Hasql.Connection           as Conn
import qualified Hasql.Decoders             as D
import qualified Hasql.Encoders             as E
import qualified Hasql.Session              as Session
import           Hasql.Statement
import           Schema
import           Types

selectProductSt :: Statement ProductId ProductEntity
selectProductSt = Statement sql (encoder :: E.Params String) decoder True
  where
    sql = "select id, name, price, created_at from products where id = $1"
    encoder = E.param (E.nonNullable stringEncoder)
    decoder = D.singleRow productEntityDecoder

selectProductsSt :: Statement () [ProductEntity]
selectProductsSt = Statement sql encoder decoder True
  where
    sql = "select id, name, price, created_at from products"
    encoder = E.noParams
    decoder = D.rowList productEntityDecoder

insertProductSt :: Statement Product ProductId
insertProductSt = Statement sql encoder decoder True
  where
    sql =
      "insert into products \
      \ (id, name, price, created_at) \
      \ values \
      \ (uuid_generate_v4(), $1, $2, $3) \
      \ returning id"
    encoder = productEncoder
    decoder = D.singleRow $ D.column (D.nonNullable stringDecoder)


stringEncoder :: E.Value String
stringEncoder = pack >$< E.text

stringDecoder :: D.Value String
stringDecoder = fmap unpack D.text

productDecoder :: D.Row Product
productDecoder = Product
  <$> D.column (D.nonNullable stringDecoder) -- name
  <*> D.column (D.nonNullable D.float4) -- price
  <*> D.column (D.nonNullable D.timestamptz) -- created_at

productEntityDecoder :: D.Row ProductEntity
productEntityDecoder = ProductEntity
  <$> D.column (D.nonNullable stringDecoder)
  <*> productDecoder

productEncoder :: E.Params Product
productEncoder =
  (name >$< E.param (E.nonNullable stringEncoder)) <> -- $1 name
  (price >$< E.param (E.nonNullable E.float4)) <> -- $2 price
  (createdAt >$< E.param (E.nonNullable E.timestamptz)) -- $3 createdAt

getProductById :: Conn.Connection -> ProductId -> IO (Either Session.QueryError ProductEntity)
getProductById conn id = Session.run (Session.statement id selectProductSt) conn

getAllProducts :: Conn.Connection -> IO (Either Session.QueryError [ProductEntity])
getAllProducts = Session.run (Session.statement () selectProductsSt)

insertProduct :: Conn.Connection -> Product -> IO (Either Session.QueryError ProductId)
insertProduct conn product = Session.run (Session.statement product insertProductSt) conn

testRun :: AppConfig -> IO ()
testRun conf = do
  let s = connectionSettings $ psql conf
  Conn.acquire s >>= \case
    Left err -> putStrLn $ "Connection error: " ++ show err
    Right c -> do
      putStrLn "Connection acquired!"
      r <- getProductById c "test"
      putStrLn $ "Query result: " ++ show r
      productId <- getCurrentTime >>= \t -> insertProduct c $ Product "test" 10.0 t
      putStrLn $ "New product created with id " ++ show productId
      pp <- either (pure . Left) (getProductById c) productId
      putStrLn $ "Created product retrieved " ++ show pp
      Conn.release c
  putStrLn "Test done"

withDB :: AppConfig -> (Conn.Connection -> IO ()) -> IO ()
withDB conf server = do
  let s = connectionSettings $ psql conf
  Conn.acquire s >>= \case
    Left err -> putStrLn $ "Connection error: " ++ show err
    Right c -> do
      server c
      Conn.release c
  putStrLn "DB closed"
