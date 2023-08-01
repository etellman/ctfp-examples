module Ch12.ChapterTest (chapterTests) where

import Ch12.ContinuousTest
import Ch12.EqualizerTest
import Ch12.HomSetTest
import Ch12.ProductTest
import Ch12.PullbackTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Chapter 12"
    [ Ch12.HomSetTest.tests,
      Ch12.ProductTest.tests,
      Ch12.EqualizerTest.tests,
      Ch12.PullbackTest.tests,
      Ch12.ContinuousTest.tests
    ]
