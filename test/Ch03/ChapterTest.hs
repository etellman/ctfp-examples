module Ch03.ChapterTest (chapterTests) where

import Ch03.AndMonoidTest
import Ch03.OrMonoidTest
import Ch03.SumMonoidTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Chapter 3"
    [ Ch03.SumMonoidTest.tests,
      Ch03.AndMonoidTest.tests,
      Ch03.OrMonoidTest.tests
    ]
