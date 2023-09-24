module Ch22.ChapterTest (chapterTests) where

import Ch22.MonadTest
import Ch22.MonoidalCategoryTest
import Ch22.StoreTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Ch22.ChapterTest"
    [ Ch22.MonadTest.tests,
      Ch22.MonoidalCategoryTest.tests,
      Ch22.StoreTest.tests
    ]
