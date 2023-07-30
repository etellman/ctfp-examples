module Ch12.ChapterTest (chapterTests) where

import Ch12.HomSetTest
import Ch12.ProductTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Chapter 12"
    [ Ch12.HomSetTest.tests,
      Ch12.ProductTest.tests
    ]
