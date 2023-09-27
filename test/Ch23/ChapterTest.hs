module Ch23.ChapterTest (chapterTests) where

import Ch23.FixTest
import Ch23.MonFTest
import Ch23.NatFTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Ch23.ChapterTest"
    [ Ch23.FixTest.tests,
      Ch23.MonFTest.tests,
      Ch23.NatFTest.tests
    ]
