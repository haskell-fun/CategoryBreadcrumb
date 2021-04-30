{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

import Data.Aeson (FromJSON, parseJSON, withBool, withObject, (.:))
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API (Capture, Get, JSON, ToHttpApiData, type (:>), (:<|>) ((:<|>)))
import Servant.Client (BaseUrl (BaseUrl), ClientM, Scheme (Https), client, mkClientEnv, runClientM)

newtype CategoryId = CategoryId Int
  deriving (Eq, Show)
  deriving (FromJSON) via Int

data CategoryState = Active | Inactive deriving (Eq, Show)

data CategorySelectableState = Selectable | Unselectable deriving (Eq, Show)

instance FromJSON CategoryState where
  parseJSON = withBool "category state" $ pure . f
    where
      f True = Active
      f False = Inactive

instance FromJSON CategorySelectableState where
  parseJSON = withBool "category selectable state" $ pure . f
    where
      f True = Selectable
      f False = Unselectable

newtype CategoryDescription = CategoryDescription String
  deriving (Eq, Show)
  deriving (FromJSON) via String

data Category = Category
  { categoryId :: CategoryId,
    active :: CategoryState,
    selectable :: CategorySelectableState,
    language :: String,
    left :: Int,
    right :: Int,
    text :: CategoryDescription
  }
  deriving (Eq, Show)

instance FromJSON Category where
  parseJSON = withObject "category" $ \o ->
    Category <$> o .: "categoryId"
      <*> o .: "is_active"
      <*> o .: "selectable"
      <*> o .: "lang"
      <*> o .: "left"
      <*> o .: "right"
      <*> o .: "text"

type CategoryTree = [Category]

newtype CategoryResponse = CategoryResponse {category :: CategoryTree}
  deriving (Eq, Show, Generic)

instance FromJSON CategoryResponse

newtype CategoriesResponse = CategoriesResponse {categories :: CategoryResponse}
  deriving (Eq, Show, Generic)

instance FromJSON CategoriesResponse

newtype ProductId = ProductId Int
  deriving (Eq, Show, ToHttpApiData)
  deriving (FromJSON) via Int

data ProductDetailsResponse = ProductDetailsResponse
  { productId :: ProductId,
    productCategoryIds :: [CategoryId]
  }
  deriving (Eq, Show)

instance FromJSON ProductDetailsResponse where
  parseJSON = withObject "product details" $ \o ->
    ProductDetailsResponse <$> o .: "id"
      <*> o .: "categories"

type API = "api" :> "v1" :> "categories" :> "" :> Get '[JSON] CategoriesResponse
  :<|> "api" :> "v1" :> "products" :> Capture "pId" ProductId :> "" :> Get '[JSON] ProductDetailsResponse

api :: Proxy API
api = Proxy

fetchCategoriesResponse :: ClientM CategoriesResponse
fetchProductDetailsResponse :: ProductId -> ClientM ProductDetailsResponse
fetchCategoriesResponse :<|> fetchProductDetailsResponse = client api

fetchCategoryTree :: ClientM CategoryTree
fetchCategoryTree = fmap (category . categories) fetchCategoriesResponse

runCategoryTree :: IO ()
runCategoryTree = do
  manager <- newManager tlsManagerSettings
  let baseUrl = BaseUrl Https "api.depop.com" 443 ""
  let clientEnv = mkClientEnv manager baseUrl
  res <- runClientM fetchCategoryTree clientEnv
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right categoryTree -> do
      print categoryTree

runProductDetails :: IO ()
runProductDetails = do
  manager <- newManager tlsManagerSettings
  let baseUrl = BaseUrl Https "api.depop.com" 443 ""
  let clientEnv = mkClientEnv manager baseUrl
  res <- runClientM (fetchProductDetailsResponse $ ProductId 399534) clientEnv
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right productDetails -> do
      print productDetails

--data Error = CategoryNotFound | TreeDescriptionCorrupted deriving (Eq, Show)
--breadcrumb :: CategoryTree -> CategoryId -> Either Error (NonEmpty CategoryId)
breadcrumb :: CategoryTree -> CategoryId -> Maybe (NonEmpty CategoryId)
breadcrumb _ _ = Nothing

render :: Maybe (NonEmpty CategoryId) -> String
render _ = undefined

-- cli :: IO ()
-- cli = do
--   cid <- fetchCategoryId
--   t <- fetchCategoryTree
--   let b = breadcrumb t cid
--   let s = render b
--   displayBreadcrumb s

fetchCategoryId :: IO CategoryId
fetchCategoryId = undefined

displayBreadcrumb :: String -> IO ()
displayBreadcrumb _ = undefined

-- curl --verbose \
--   https://api.depop.com/api/v1/categories/ |\
--   jq '.categories.category' |\
--   jq '.[] | { categoryId, text }'

-- curl --silent \
--   https://api.depop.com/api/v1/categories/ |\
--   jq '.variantSet | .[] | { country, variantSetId, id, lang, name }' | jq -s '.'
