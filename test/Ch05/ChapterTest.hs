module Ch05.ChapterTest (chapterTests) where

import Ch05.ProductTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Chapter 5"
    [ Ch05.ProductTest.tests
    ]
