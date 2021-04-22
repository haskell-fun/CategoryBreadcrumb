{-# LANGUAGE DeriveGeneric #-}
module Lib where

import Data.List.NonEmpty ( NonEmpty )
import GHC.Generics ( Generic )


newtype CategoryId = CategoryId Int deriving (Eq, Show, Generic)

data Category = Category {
  categoryId :: CategoryId,
  active :: Bool,
  selectable :: Bool,
  language :: String,
  left :: Int,
  right :: Int,
  text :: String
} deriving (Eq, Show, Generic)
type CategoryTree = [Category]

breadcrumb :: CategoryTree -> CategoryId -> Maybe (NonEmpty CategoryId)
breadcrumb _ _ = Nothing

render :: Maybe (NonEmpty CategoryId) -> String
render _ = undefined

cli :: IO ()
cli = do
  cid <- fetchCategoryId
  t <- fetchCategoryTree
  let b = breadcrumb t cid
  let s = render b
  displayBreadcrumb s

fetchCategoryId :: IO CategoryId
fetchCategoryId = undefined

fetchCategoryTree :: IO CategoryTree
fetchCategoryTree = undefined

displayBreadcrumb :: String -> IO ()
displayBreadcrumb _ = undefined
