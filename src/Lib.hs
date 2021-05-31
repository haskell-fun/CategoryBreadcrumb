{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import CategoryTree (CategoryTree, CategoryId(..), CategoriesResponse(categories), CategoryResponse(category), Category)
import Data.List.NonEmpty (NonEmpty)
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
  putStrLn $ f1 $ f ctRes pdRes

-- Ansi lib: green on success/ red on error
-- Either monad -> Validate applicative (import Stack pckg)

f1 :: Either ClientError String -> String
f1 = either show id

f :: Either ClientError CategoryTree -> Either ClientError ProductDetailsResponse -> Either ClientError String
-- f ctRes pdRes = do
--   ct <- ctRes
--   pd <- pdRes
--   return $ h ct pd
f ctRes pdRes = h <$> ctRes <*> pdRes
-- f ctRes pdRes = (fmap h ctRes) <*> pdRes

h :: CategoryTree -> ProductDetailsResponse -> String
h ct pd = show ct ++ "\n" ++ show pd

-- f ctRes pdRes = case (ctRes, pdRes) of
--   (Left ctErr, Left pdErr) -> "Error: " ++ show ctErr ++ "\nError: " ++ show pdErr
--   (Left ctErr, Right _   ) -> "Error: " ++ show ctErr
--   (Right _   , Left pdErr) -> "Error: " ++ show pdErr
--   (Right ct  , Right pd  ) -> show ct ++ "\n" ++ show pd

g :: Show a => Either ClientError a -> Either ClientError String
g x = fmap show x -- x.map(::show) -- Kotlin
-- g :: Show a => Either ClientError a -> String
-- g (Left err) = "Error: " ++ show err
-- g (Right x) = show x

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
