{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import CategoryTree (CategoryTree, CategoryId(..), CategoriesResponse(categories), CategoryResponse(category))
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Proxy (Proxy(..))
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import ProductDetails (ProductDetailsResponse, ProductId(..))
import Servant.API (Capture, Get, JSON, type (:>), (:<|>) ((:<|>)))
import Servant.Client (BaseUrl(BaseUrl), ClientError, ClientM, Scheme(Https), client, mkClientEnv, runClientM)

type API = "api" :> "v1" :> "categories" :> "" :> Get '[JSON] CategoriesResponse
  :<|> "api" :> "v1" :> "products" :> Capture "pId" ProductId :> "" :> Get '[JSON] ProductDetailsResponse

api :: Proxy API
api = Proxy

fetchCategoriesResponse :: ClientM CategoriesResponse
fetchProductDetailsResponse :: ProductId -> ClientM ProductDetailsResponse
fetchCategoriesResponse :<|> fetchProductDetailsResponse = client api

fetchCategoryTree :: ClientM CategoryTree
fetchCategoryTree = fmap (category . categories) fetchCategoriesResponse

run :: ProductId -> IO ()
run pId = do
  manager <- newManager tlsManagerSettings
  let baseUrl = BaseUrl Https "api.depop.com" 443 ""
  let clientEnv = mkClientEnv manager baseUrl
  ctRes <- runClientM fetchCategoryTree clientEnv
  pdRes <- runClientM (fetchProductDetailsResponse pId) clientEnv
  case (ctRes, pdRes) of
    (Left ctErr, Left pdErr) -> putStrLn $ "Error: " ++ show ctErr ++ "\nError: " ++ show pdErr
    (Left ctErr, Right _   ) -> putStrLn $ "Error: " ++ show ctErr
    (Right _   , Left pdErr) -> putStrLn $ "Error: " ++ show pdErr
    (Right ct  , Right pd  ) -> do
      print ct
      print pd

cli :: IO ()
cli = do
  cid <- fetchCategoryId
  csRes <- fetchCategoriesResponseIO
  let t = fmap toCategoryTree csRes
  let b = fmap (findBreadcrumb cid) t
  let output = either renderError renderBreadcrumb b
  putStrLn output

fetchCategoryId :: IO CategoryId
fetchCategoryId = pure $ CategoryId 51

fetchCategoriesResponseIO :: IO (Either ClientError CategoriesResponse)
fetchCategoriesResponseIO = do
  manager <- newManager tlsManagerSettings
  let baseUrl = BaseUrl Https "api.depop.com" 443 ""
  let clientEnv = mkClientEnv manager baseUrl
  runClientM fetchCategoriesResponse clientEnv

type Breadcrumb = Maybe (NonEmpty CategoryId)

toCategoryTree :: CategoriesResponse -> CategoryTree
toCategoryTree = category . categories

findBreadcrumb :: CategoryId -> CategoryTree -> Breadcrumb
findBreadcrumb _ _ = Nothing

renderBreadcrumb :: Breadcrumb -> String
renderBreadcrumb Nothing = "N.D."
renderBreadcrumb (Just nonEmptyIds) = joinNonEmptyList " > " nonEmptyIds

renderError :: Show a => a -> String
renderError e = "Error: " ++ show e

joinNonEmptyList :: Show a => String -> NonEmpty a -> String
joinNonEmptyList separator = intercalate separator . toList . fmap show
