module Ch08.ChapterTest (chapterTests) where

import Ch08.BifunctorTest
import Ch08.ContravariantTest
import Ch08.FstTest
import Ch08.K2Test
import Ch08.Maybe2Test
import Ch08.PreListTest
import Ch08.WriterTest
import Test.Tasty

chapterTests :: TestTree
chapterTests =
  testGroup
    "Chapter 8"
    [ Ch08.BifunctorTest.tests,
      Ch08.Maybe2Test.tests,
      Ch08.WriterTest.tests,
      Ch08.ContravariantTest.tests,
      Ch08.PreListTest.tests,
      Ch08.K2Test.tests,
      Ch08.FstTest.tests
    ]
