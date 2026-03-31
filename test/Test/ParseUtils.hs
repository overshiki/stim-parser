module Test.ParseUtils where

import Test.HUnit
import Text.Megaparsec (runParser)
import StimParser.ParseUtils

tests :: Test
tests = TestList
  [ testParseInt
  , testParseFloat
  , testRunParser
  ]

-- Test parseInt with various inputs
testParseInt :: Test
testParseInt = TestCase $ do
  -- Test positive integer
  case runParser parseInt "" "42" of
    Right r -> assertEqual "parseInt positive" 42 r
    Left _ -> assertFailure "parseInt should parse positive integer"
  -- Test negative integer
  case runParser parseInt "" "-42" of
    Right r -> assertEqual "parseInt negative" (-42) r
    Left _ -> assertFailure "parseInt should parse negative integer"
  -- Test with whitespace
  case runParser parseInt "" "123  " of
    Right r -> assertEqual "parseInt with spaces" 123 r
    Left _ -> assertFailure "parseInt should handle whitespace"

-- Test parseFloat with various inputs
testParseFloat :: Test
testParseFloat = TestCase $ do
  -- Test positive float
  case runParser parseFloat "" "3.14" of
    Right r -> assertEqual "parseFloat positive" 3.14 r
    Left _ -> assertFailure "parseFloat should parse positive float"
  -- Test negative float
  case runParser parseFloat "" "-3.14" of
    Right r -> assertEqual "parseFloat negative" (-3.14) r
    Left _ -> assertFailure "parseFloat should parse negative float"
  -- Test scientific notation
  case runParser parseFloat "" "1.5e2" of
    Right r -> assertEqual "parseFloat scientific" 150.0 r
    Left _ -> assertFailure "parseFloat should parse scientific notation"

-- Test run helper function
testRunParser :: Test
testRunParser = TestList
  [ "run parseInt" ~: 42 ~=? run parseInt "42"
  , "run parseFloat" ~: 3.14 ~=? run parseFloat "3.14"
  ]
