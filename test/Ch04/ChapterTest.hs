module Ch04.ChapterTest (chapterTests) where

import Ch04.WriterTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Chapter 4"
    [ Ch04.WriterTest.tests
    ]
