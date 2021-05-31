{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module CategoryTree where

import Data.Aeson (FromJSON, parseJSON, withBool, withObject, (.:))
import GHC.Generics (Generic)

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
  { categoryId :: CategoryId
  , active :: CategoryState
  , selectable :: CategorySelectableState
  , language :: String
  , left :: Int
  , right :: Int
  , text :: CategoryDescription
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
