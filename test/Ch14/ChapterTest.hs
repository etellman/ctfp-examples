module Ch14.ChapterTest (chapterTests) where

import Ch14.RepresentableTest
import Test.Tasty

chapterTests :: TestTree
chapterTests = Ch14.RepresentableTest.tests
