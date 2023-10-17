module Atif.ChapterTest (chapterTests) where

import Atif.PartitionTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Atif.ChapterTest"
    [ Atif.PartitionTest.tests
    ]
