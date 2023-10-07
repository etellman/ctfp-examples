module Ch24.ChapterTest (chapterTests) where

import Ch24.AlgebraTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Ch24.ChapterTest"
    [ Ch24.AlgebraTest.tests
    ]
