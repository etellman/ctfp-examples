module Ch03.ChapterTest (chapterTests) where

import Ch03.MonoidTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Chapter 3"
    [ Ch03.MonoidTest.tests
    ]
