module Ch08.ChapterTest (chapterTests) where

import Ch08.ContravariantTest
import Ch08.FstTest
import Ch08.K2Test
import Ch08.Maybe2Test
import Ch08.PreListTest
import Ch08.SndTest
import Ch08.TupleTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Ch08.ChapterTest"
    [ Ch08.TupleTest.tests,
      Ch08.Maybe2Test.tests,
      Ch08.ContravariantTest.tests,
      Ch08.PreListTest.tests,
      Ch08.K2Test.tests,
      Ch08.FstTest.tests,
      Ch08.SndTest.tests
    ]
