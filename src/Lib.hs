{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import CategoryTree (CategoryTree, CategoryId(..), CategoriesResponse(categories), CategoryResponse(category))
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy (Proxy(..))
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import ProductDetails (ProductDetailsResponse, ProductId(..))
import Servant.API (Capture, Get, JSON, type (:>), (:<|>) ((:<|>)))
import Servant.Client (BaseUrl(BaseUrl), ClientM, Scheme(Https), client, mkClientEnv, runClientM)

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

--data Error = CategoryNotFound | TreeDescriptionCorrupted deriving (Eq, Show)
--findBreadcrumb :: CategoryTree -> CategoryId -> Either Error (NonEmpty CategoryId)
findBreadcrumb :: CategoryTree -> CategoryId -> Maybe (NonEmpty CategoryId)
findBreadcrumb _ _ = Nothing

render :: Maybe (NonEmpty CategoryId) -> String
render _ = undefined

-- cli :: IO ()
-- cli = do
--   t <- fetchCategoryTree...
--   cid <- fetchCategoryId
--   let b = findBreadcrumb t cid
--   let s = render b
--   displayBreadcrumb s

fetchCategoryId :: IO CategoryId
fetchCategoryId = pure $ CategoryId 51

displayBreadcrumb :: String -> IO ()
displayBreadcrumb = putStr
