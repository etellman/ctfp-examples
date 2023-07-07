module Ch07.ChapterTest (chapterTests) where

import Ch07.ListFunctorTest
import Ch07.MaybeTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Chapter 7"
    [ Ch07.MaybeTest.tests,
      Ch07.ListFunctorTest.tests
    ]
