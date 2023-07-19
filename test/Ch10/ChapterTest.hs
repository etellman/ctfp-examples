module Ch10.ChapterTest (chapterTests) where

import Ch10.FGNaturalTest
import Ch10.ListNaturalTest
import Ch10.ReaderNaturalTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Chapter 10"
    [ Ch10.FGNaturalTest.tests,
      Ch10.ListNaturalTest.tests,
      Ch10.ReaderNaturalTest.tests
    ]
