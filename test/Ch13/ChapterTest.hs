module Ch13.ChapterTest (chapterTests) where

import Ch13.HomomorphismTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Chapter 13"
    [ Ch13.HomomorphismTest.tests
    ]
