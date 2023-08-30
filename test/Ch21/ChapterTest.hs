module Ch21.ChapterTest (chapterTests) where

import Ch21.UnitConverterTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Ch21.ChapterTest"
    [ Ch21.UnitConverterTest.tests
    ]
