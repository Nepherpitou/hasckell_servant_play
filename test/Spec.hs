{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main
  ( main
  ) where

import           Data.Aeson
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Lazy      as LB
import           Data.String               (fromString)
import           Lib                       (Product (..), app)
import           Network.HTTP.Types.Header (hContentType)
import           Network.HTTP.Types.Method (methodPost)
import           Network.Wai.Test          (SResponse)
import           Test.Hspec
import           Test.Hspec.Wai            as Wai
import           Test.Hspec.Wai.JSON       as WJ
import           Test.Hspec.Wai.Matcher    (bodyEquals)
import qualified UserInputs                as IN

main :: IO ()
main = hspec spec

getResponse :: Maybe Product
getResponse = Just $ Product "test" "Sample"

postJson :: ByteString -> LB.ByteString -> WaiSession SResponse
postJson url = Wai.request methodPost url [(hContentType, "application/json")]

createProduct :: IN.Product
createProduct = IN.Product "Sample"

spec :: Spec
spec =
  with (return app) $
  describe "POST /products" $ do
    it "responds with 200" $ postJson "/products" (encode createProduct) `shouldRespondWith` 200
    it "responds with Product" $ do
      let product = ResponseMatcher 200 [] (bodyEquals . encode $ getResponse)
      postJson "/products" (encode createProduct) `shouldRespondWith` product
