module LibSpec where

import Data.Maybe ( isNothing )
-- import Test.QuickCheck.Arbitrary ( Arbitrary )
import Test.Hspec ( describe, it, shouldBe, Spec )
import Test.Hspec.QuickCheck ( prop )
import Test.QuickCheck.Property ( (==>) )
import Lib ( breadcrumb, Category(categoryId), CategoryId, CategoryTree )

-- instance Arbitrary CategoryId
-- instance Arbitrary Category

spec :: Spec
spec = describe "breadcrumb" $ do

   prop "returns Nothing iff categoryId is not present in the tree" $
     \t c -> isNothing (breadcrumb t c) == not (isCategoryPresent t c)

   -- prop "must return Nothing if categoryId is not present in the tree" $
   --   \t c -> not (isCategoryPresent t c) ==> isNothing (breadcrumb t c)

   -- prop "must return Nothing ONLY if categoryId is not present in the tree" $
   --   \t c -> isNothing (breadcrumb t c) ==> not (isCategoryPresent t c)

isCategoryPresent :: CategoryTree -> CategoryId -> Bool
isCategoryPresent t cid = cid `elem` fmap categoryId t
