module Lib.LibTest (libTests) where

import Lib.FTest
import Lib.StreamTest
import Test.Tasty

libTests :: TestTree
libTests =
  testGroup
    "Lib.LibTest"
    [ Lib.FTest.tests,
      Lib.StreamTest.tests
    ]
