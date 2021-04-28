{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}

module Lib (run) where

import Data.List.NonEmpty (NonEmpty)
import Data.Aeson (FromJSON)
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API (JSON, type (:>), Get)
import Servant.Client
    ( client,
      hoistClient,
      mkClientEnv,
      runClientM,
      ClientEnv,
      ClientM,
      BaseUrl(BaseUrl),
      Scheme(Https) )
import Servant.Types.SourceT ()

import qualified Servant.Client.Streaming as S

newtype CategoryId = CategoryId Int deriving (Eq, Show, Generic)

instance FromJSON CategoryId

data Category = Category
  { categoryId :: CategoryId,
    is_active :: Bool,
    selectable :: Bool,
    lang :: String,
    left :: Int,
    right :: Int,
    text :: String
  } deriving (Eq, Show, Generic)

instance FromJSON Category

type CategoryTree = [Category]

newtype CategoryObj = CategoryObj { category :: CategoryTree }
  deriving (Eq, Show, Generic)

instance FromJSON CategoryObj

newtype CategoriesObj = CategoriesObj { categories :: CategoryObj }
  deriving (Eq, Show, Generic)

instance FromJSON CategoriesObj

type CategoryAPI = "api" :> "v1" :> "categories" :> "" :> Get '[JSON] CategoriesObj

categoryAPI :: Proxy CategoryAPI
categoryAPI = Proxy

fetchCategoryTree :: ClientM CategoryTree
fetchCategoryTree = fmap (category . categories) (client categoryAPI)

run :: IO ()
run = do
  manager <- newManager tlsManagerSettings
  let baseUrl = BaseUrl Https "api.depop.com" 443 ""
  let clientEnv = mkClientEnv manager baseUrl
  res <- runClientM fetchCategoryTree clientEnv
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right categoryTree -> do
      print categoryTree

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
