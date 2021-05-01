{-# LANGUAGE DerivingVia #-}

module ProductDetails where

import CategoryTree (CategoryId)
import Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import Data.Text (pack)
import Servant.API (ToHttpApiData (toUrlPiece, toQueryParam))

newtype ProductId = ProductId Int
  deriving (Eq, Show)
  deriving (FromJSON) via Int

instance ToHttpApiData ProductId where
  toUrlPiece (ProductId pId) = pack $ show pId
  toQueryParam (ProductId pId) = pack $ show pId

data ProductDetailsResponse = ProductDetailsResponse
  { productId :: ProductId
  , productCategoryIds :: [CategoryId]
  }
  deriving (Eq, Show)

instance FromJSON ProductDetailsResponse where
  parseJSON = withObject "product details" $ \o ->
    ProductDetailsResponse <$> o .: "id"
      <*> o .: "categories"
