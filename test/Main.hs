module Main where

import Test.HUnit
import System.Exit (exitFailure, exitSuccess)

import qualified Test.Expr as Expr
import qualified Test.ParseUtils as ParseUtils
import qualified Test.Parse as Parse
import qualified Test.Trans as Trans
import qualified Test.DEM.Expr as DEMExpr
import qualified Test.DEM.Parse as DEMParse

main :: IO ()
main = do
  counts <- runTestTT tests
  if errors counts + failures counts == 0
    then exitSuccess
    else exitFailure

tests :: Test
tests = TestList
  [ Expr.tests
  , ParseUtils.tests
  , Parse.tests
  , Trans.tests
  , DEMExpr.tests
  , DEMParse.tests
  ]
