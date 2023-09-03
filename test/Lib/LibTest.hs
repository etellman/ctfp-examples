module Lib.LibTest (libTests) where

import Lib.FTest
import Lib.Reader2Test
import Lib.StreamTest
import Lib.Writer2Test
import Test.Tasty

libTests :: TestTree
libTests =
  testGroup
    "Lib.LibTest"
    [ Lib.FTest.tests,
      Lib.StreamTest.tests,
      Lib.Reader2Test.tests,
      Lib.Writer2Test.tests
    ]
