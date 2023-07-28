module Ch12.ChapterTest (chapterTests) where

import Ch12.HomSetTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Chapter 12"
    [ Ch12.HomSetTest.tests
    ]
