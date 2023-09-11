module Ch22.ChapterTest (chapterTests) where

import Ch22.MonadTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Ch22.ChapterTest"
    [Ch22.MonadTest.tests]
