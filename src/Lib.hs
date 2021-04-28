{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}

module Lib (run) where

import Data.List.NonEmpty (NonEmpty)
import Data.Aeson (FromJSON)
import Data.Proxy ( Proxy(..) )
import GHC.Generics (Generic)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API ( JSON, type (:>), Get )
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
    active :: Bool,
    selectable :: Bool,
    language :: String,
    left :: Int,
    right :: Int,
    text :: String
  } deriving (Eq, Show, Generic)

instance FromJSON Category

type CategoryTree = [Category]

type CategoryAPI = "api" :> "v1" :> "categories" :> "" :> Get '[JSON] CategoryTree

categoryAPI :: Proxy CategoryAPI
categoryAPI = Proxy

fetchCategoryTree :: ClientM CategoryTree
fetchCategoryTree = client categoryAPI

-- How can be a (Client IO CategoryAPI) used?
getClients :: ClientEnv -> S.Client IO CategoryAPI
getClients clientEnv = hoistClient categoryAPI
  ( fmap (either (error . show) id)
  . flip runClientM clientEnv
  ) fetchCategoryTree

run :: IO ()
run = do
  manager' <- newManager defaultManagerSettings
  let clientEnv = mkClientEnv manager' (BaseUrl Https "api.depop.com" 8080 "")
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

-- curl --silent \
--   https://api.depop.com/api/v1/categories/ |\
--   jq '.categories.category' |\
--   jq '.[] | { categoryId, text }'

-- curl --silent \
--   https://api.depop.com/api/v1/categories/ |\
--   jq '.variantSet | .[] | { country, variantSetId, id, lang, name }' | jq -s '.'
