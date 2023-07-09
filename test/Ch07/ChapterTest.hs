module Ch07.ChapterTest (chapterTests) where

import Ch07.ConstTest
import Ch07.IdentityTest
import Ch07.ListTest
import Ch07.MaybeTest
import Ch07.ReaderTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Chapter 7"
    [ Ch07.MaybeTest.tests,
      Ch07.ListTest.tests,
      Ch07.ReaderTest.tests,
      Ch07.ConstTest.tests,
      Ch07.IdentityTest.tests
    ]
