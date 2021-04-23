module LibSpec where

import Data.Maybe (isNothing)
import Lib
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, listOf)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Property (forAll)

genCategoryId :: Gen CategoryId
genCategoryId = CategoryId <$> arbitrary

instance Arbitrary CategoryId where
  arbitrary = genCategoryId

-- this is a silly generator, we want to generate something better next time + avoid orphan instances
instance Arbitrary Category where
  arbitrary = sillyGenCategory

sillyGenCategory :: Gen Category
sillyGenCategory = Category <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

spec :: Spec
spec = describe "breadcrumb" $ do
  prop "returns Nothing iff categoryId is not present in the tree (manual monadic generators)" $
    forAll genCategoryId $ \c ->
      forAll (listOf sillyGenCategory) $ \t ->
        let isCategoryPresent t cid = cid `elem` fmap categoryId t
         in isNothing (breadcrumb t c) == not (isCategoryPresent t c)

  prop "returns Nothing iff categoryId is not present in the tree (automatic type class resolution)" $
    \c t ->
      let isCategoryPresent t c = c `elem` fmap categoryId t
       in isNothing (breadcrumb t c) == not (isCategoryPresent t c)

-- prop "must return Nothing if categoryId is not present in the tree" $
--   \t c -> not (isCategoryPresent t c) ==> isNothing (breadcrumb t c)

-- prop "must return Nothing ONLY if categoryId is not present in the tree" $
--   \t c -> isNothing (breadcrumb t c) ==> not (isCategoryPresent t c)
