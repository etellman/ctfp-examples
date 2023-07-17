module Ch10.ChapterTest (chapterTests) where

import Ch10.NaturalTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Chapter 10"
    [ Ch10.NaturalTest.tests
    ]
