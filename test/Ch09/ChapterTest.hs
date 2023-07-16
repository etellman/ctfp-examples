module Ch09.ChapterTest (chapterTests) where

import Ch09.EvalTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Chapter 9"
    [ Ch09.EvalTest.tests
    ]
