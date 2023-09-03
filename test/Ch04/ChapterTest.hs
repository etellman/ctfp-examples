module Ch04.ChapterTest (chapterTests) where

import Ch04.OptionalTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Ch04.ChapterTest"
    [ Ch04.OptionalTest.tests
    ]
