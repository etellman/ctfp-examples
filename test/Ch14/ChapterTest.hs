module Ch14.ChapterTest (chapterTests) where

import Ch14.RepresentableTest
import Ch14.StreamTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Ch14.ChapterTest"
    [ Ch14.RepresentableTest.tests,
      Ch14.StreamTest.tests
    ]
