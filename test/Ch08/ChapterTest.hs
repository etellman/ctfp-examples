module Ch08.ChapterTest (chapterTests) where

import Ch08.BiCompTest
import Ch08.BifunctorTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Chapter 8"
    [ Ch08.BifunctorTest.tests,
      Ch08.BiCompTest.tests
    ]
