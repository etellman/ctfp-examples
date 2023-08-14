module Ch18.ChapterTest (chapterTests) where

import Ch18.FGAdjunctionTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Ch18.ChapterTest"
    [ Ch18.FGAdjunctionTest.tests
    ]
