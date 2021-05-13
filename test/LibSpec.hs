module LibSpec where

import Data.Maybe (isNothing)
import Lib (findBreadcrumb)
import CategoryTree
  ( CategoryId(..),
    Category(Category, categoryId),
    CategoryDescription(..),
    CategorySelectableState(..),
    CategoryState(..)
  )
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, elements, listOf)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Property (forAll)

genCategoryId :: Gen CategoryId
genCategoryId = CategoryId <$> arbitrary

instance Arbitrary CategoryState where
  arbitrary = elements [Active, Inactive]

instance Arbitrary CategorySelectableState where
  arbitrary = elements [Selectable, Unselectable]

instance Arbitrary CategoryDescription where
  arbitrary = CategoryDescription <$> arbitrary

instance Arbitrary CategoryId where
  arbitrary = genCategoryId

-- this is a silly generator, we want to generate something better next time + avoid orphan instances
instance Arbitrary Category where
  arbitrary = sillyGenCategory

sillyGenCategory :: Gen Category
sillyGenCategory = Category <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

spec :: Spec
spec = describe "findBreadcrumb" $ do
  prop "returns Nothing iff categoryId is not present in the tree (manual monadic generators)" $
    forAll genCategoryId $ \c ->
      forAll (listOf sillyGenCategory) $ \t ->
        let isCategoryPresent c t = c `elem` fmap categoryId t
         in isNothing (findBreadcrumb c t) == not (isCategoryPresent c t)

  prop "returns Nothing iff categoryId is not present in the tree (automatic type class resolution)" $
    \c t ->
      let isCategoryPresent c t = c `elem` fmap categoryId t
       in isNothing (findBreadcrumb c t) == not (isCategoryPresent c t)

-- prop "must return Nothing if categoryId is not present in the tree" $
--   \c t -> not (isCategoryPresent c t) ==> isNothing (findBreadcrumb c t)

-- prop "must return Nothing ONLY if categoryId is not present in the tree" $
--   \c t -> isNothing (findBreadcrumb c t) ==> not (isCategoryPresent c t)
