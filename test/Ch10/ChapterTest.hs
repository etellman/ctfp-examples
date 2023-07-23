module Ch10.ChapterTest (chapterTests) where

import Ch10.FGNaturalTest
import Ch10.ListNaturalTest
import Ch10.OpNaturalTest
import Ch10.ReaderNaturalTest
import Ch10.TwoCategoryTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Chapter 10"
    [ Ch10.FGNaturalTest.tests,
      Ch10.ListNaturalTest.tests,
      Ch10.ReaderNaturalTest.tests,
      Ch10.OpNaturalTest.tests,
      Ch10.TwoCategoryTest.tests
    ]
