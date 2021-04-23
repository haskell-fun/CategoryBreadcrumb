module Lib where

import Data.List.NonEmpty (NonEmpty)

newtype CategoryId = CategoryId Int deriving (Eq, Show)

data Category = Category
  { categoryId :: CategoryId,
    active :: Bool,
    selectable :: Bool,
    language :: String,
    left :: Int,
    right :: Int,
    text :: String
  }
  deriving (Eq, Show)

type CategoryTree = [Category]

--data Error = CategoryNotFound | TreeDescriptionCorrupted deriving (Eq, Show)
--breadcrumb :: CategoryTree -> CategoryId -> Either Error (NonEmpty CategoryId)
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
