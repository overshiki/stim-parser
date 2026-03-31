module Test.Expr where

import Test.HUnit
import StimParser.Expr

tests :: Test
tests = TestList
  [ testFIndNumInstance
  ]


-- Test FInd Num instance (compare using show since no Eq instance)
testFIndNumInstance :: Test
testFIndNumInstance = TestCase $ do
  -- Test In + In
  assertEqual "In + In" (show (In 3)) (show (In 1 + In 2))
  -- Test Fl + Fl
  assertEqual "Fl + Fl" (show (Fl 3.5)) (show (Fl 1.5 + Fl 2.0))
  -- Test In + Fl
  assertEqual "In + Fl" (show (Fl 3.5)) (show (In 1 + Fl 2.5))
  -- Test Fl + In
  assertEqual "Fl + In" (show (Fl 3.5)) (show (Fl 1.5 + In 2))
