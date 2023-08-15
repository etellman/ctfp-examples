module Lib.LibTest (libTests) where

import Test.Tasty
import Lib.StreamTest

libTests :: TestTree
libTests =
  testGroup
    "Lib.LibTest"
    [ Lib.StreamTest.tests
    ]
