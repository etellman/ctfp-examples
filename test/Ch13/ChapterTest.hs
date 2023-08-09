module Ch13.ChapterTest (chapterTests) where

import Ch13.HomomorphismTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Ch13.ChapterTest"
    [ Ch13.HomomorphismTest.tests
    ]
