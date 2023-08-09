module Ch15.ChapterTest (chapterTests) where

import Ch15.YonedaTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Ch15.ChapterTest"
    [ Ch15.YonedaTest.tests
    ]
