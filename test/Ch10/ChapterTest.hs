module Ch10.ChapterTest (chapterTests) where

import Ch10.FGNaturalTest
import Ch10.ListToMaybeTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Chapter 10"
    [ Ch10.FGNaturalTest.tests,
      Ch10.ListToMaybeTest.tests
    ]
