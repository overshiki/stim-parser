module Test.DEM.Parse where

import Test.HUnit
import Text.Megaparsec (runParser)

import StimParser.DEM.Expr
import StimParser.DEM.Parse
import StimParser.ParseUtils (run)

-- Helper to compare using show
tAssertShowEqual :: (Show a) => String -> a -> a -> Test
tAssertShowEqual msg expected actual = TestCase $
  assertEqual msg (show expected) (show actual)

tests :: Test
tests = TestList
  [ testParseDEMError
  , testParseDEMDetector
  , testParseDEMObservable
  , testParseDEMShift
  , testParseDEMRepeat
  , testParseDEMTarget
  , testParseDEMFull
  , testParseDEMHighPrecision
  , testParseDEMWhitespace
  , testParseDEMEdgeCases
  , testParseDEMComments
  , testParseDEMScientificCoords
  , testParseDEMMixedTargets
  , testParseDEMEmpty
  , testParseDEMCaret
  , testParseDEMEofEnforced
  ]

-- | Parse individual error instructions
testParseDEMError :: Test
testParseDEMError = TestList
  [ "error single detector" ~:
      DEMError 0.01 [TargetDetector (DetectorId 0)]
      ~=? run parseDEMError "error(0.01) D0"

  , "error multiple detectors" ~:
      DEMError 0.005 [TargetDetector (DetectorId 0), TargetDetector (DetectorId 1)]
      ~=? run parseDEMError "error(0.005) D0 D1"

  , "error with observable" ~:
      DEMError 0.003 [TargetDetector (DetectorId 0), TargetObservable (ObservableId 0)]
      ~=? run parseDEMError "error(0.003) D0 L0"

  , "error multiple observables" ~:
      DEMError 0.1 [TargetObservable (ObservableId 0), TargetObservable (ObservableId 1)]
      ~=? run parseDEMError "error(0.1) L0 L1"

  , "error no targets" ~:
      DEMError 0.01 []
      ~=? run parseDEMError "error(0.01)"

  , "error probability 0" ~:
      DEMError 0.0 [TargetDetector (DetectorId 0)]
      ~=? run parseDEMError "error(0) D0"

  , "error probability 1" ~:
      DEMError 1.0 [TargetDetector (DetectorId 0)]
      ~=? run parseDEMError "error(1) D0"

  , "error scientific notation" ~:
      DEMError 1.5e-2 [TargetDetector (DetectorId 0)]
      ~=? run parseDEMError "error(1.5e-2) D0"

  , "error very small probability" ~:
      DEMError 1e-15 [TargetDetector (DetectorId 0)]
      ~=? run parseDEMError "error(1e-15) D0"
  ]

-- | Parse detector declarations
testParseDEMDetector :: Test
testParseDEMDetector = TestList
  [ "detector 2D" ~:
      DEMDetector (DetectorId 0) [0.0, 0.0]
      ~=? run parseDEMDetector "detector(0, 0) D0"

  , "detector 3D" ~:
      DEMDetector (DetectorId 5) [1.0, 2.0, 0.0]
      ~=? run parseDEMDetector "detector(1, 2, 0) D5"

  , "detector negative coords" ~:
      DEMDetector (DetectorId 1) [-1.0, 0.5]
      ~=? run parseDEMDetector "detector(-1, 0.5) D1"

  , "detector 1D" ~:
      DEMDetector (DetectorId 3) [2.0]
      ~=? run parseDEMDetector "detector(2) D3"
  ]

-- | Parse logical observable declarations
testParseDEMObservable :: Test
testParseDEMObservable = TestList
  [ "observable L0" ~:
      DEMObservable (ObservableId 0)
      ~=? run parseDEMObservable "logical_observable L0"

  , "observable L5" ~:
      DEMObservable (ObservableId 5)
      ~=? run parseDEMObservable "logical_observable L5"
  ]

-- | Parse shift_detectors
testParseDEMShift :: Test
testParseDEMShift = TestList
  [ "shift 2D" ~:
      DEMShift [1.0, 0.0] 0
      ~=? run parseDEMShift "shift_detectors(1, 0) 0"

  , "shift with det offset" ~:
      DEMShift [0.0, 1.0] 10
      ~=? run parseDEMShift "shift_detectors(0, 1) 10"

  , "shift bare no coords" ~:
      DEMShift [] 96
      ~=? run parseDEMShift "shift_detectors 96"

  , "shift bare zero" ~:
      DEMShift [] 0
      ~=? run parseDEMShift "shift_detectors 0"

  , "shift single coord no coords" ~:
      DEMShift [0.5] 1
      ~=? run parseDEMShift "shift_detectors(0.5) 1"
  ]

-- | Parse repeat blocks
testParseDEMRepeat :: Test
testParseDEMRepeat = TestList
  [ "repeat simple" ~:
      (2, [DEMInstrError (DEMError 0.01 [TargetDetector (DetectorId 0)])])
      ~=? run parseDEMRepeat "repeat 2 { error(0.01) D0 }"

  , "repeat multiple" ~:
      (3, [ DEMInstrError (DEMError 0.01 [TargetDetector (DetectorId 0)])
          , DEMInstrError (DEMError 0.02 [TargetDetector (DetectorId 1)])
          ])
      ~=? run parseDEMRepeat "repeat 3 { error(0.01) D0 error(0.02) D1 }"
  ]

-- | Parse individual targets
testParseDEMTarget :: Test
testParseDEMTarget = TestList
  [ "target D0" ~:
      TargetDetector (DetectorId 0)
      ~=? run parseDEMTarget "D0"

  , "target L3" ~:
      TargetObservable (ObservableId 3)
      ~=? run parseDEMTarget "L3"
  ]

-- | Parse full DEM documents
testParseDEMFull :: Test
testParseDEMFull = TestList
  [ "simple dem" ~:
      let input = unlines
            [ "error(0.01) D0 D1 L0"
            , "error(0.005) D1 D2"
            , "detector(0, 0) D0"
            , "logical_observable L0"
            ]
          expected = DEM
            [ DEMInstrError (DEMError 0.01 [TargetDetector (DetectorId 0), TargetDetector (DetectorId 1), TargetObservable (ObservableId 0)])
            , DEMInstrError (DEMError 0.005 [TargetDetector (DetectorId 1), TargetDetector (DetectorId 2)])
            , DEMInstrDetector (DEMDetector (DetectorId 0) [0.0, 0.0])
            , DEMInstrObservable (DEMObservable (ObservableId 0))
            ]
      in expected ~=? run parseDEM input

  , "dem with repeat" ~:
      let input = "repeat 2 { error(0.01) D0 } detector(0, 0) D0"
          expected = DEM
            [ DEMInstrRepeat 2 [DEMInstrError (DEMError 0.01 [TargetDetector (DetectorId 0)])]
            , DEMInstrDetector (DEMDetector (DetectorId 0) [0.0, 0.0])
            ]
      in expected ~=? run parseDEM input

  , "dem only observables" ~:
      let input = "error(0.1) L0 L1"
          expected = DEM [DEMInstrError (DEMError 0.1 [TargetObservable (ObservableId 0), TargetObservable (ObservableId 1)])]
      in expected ~=? run parseDEM input
  ]

-- | High-precision probability parsing (the motivating case for Double)
testParseDEMHighPrecision :: Test
testParseDEMHighPrecision = TestList
  [ "high precision probability" ~:
      let input = "error(0.002673815958446297981) D0"
          result = run parseDEMError input
      in case result of
           DEMError p [TargetDetector (DetectorId 0)] ->
             -- Double preserves ~15 significant digits
             assertBool "high precision preserved" (abs (p - 0.002673815958446298) < 1e-19)
           _ -> assertFailure "parse failed"
  ]

-- | Whitespace tolerance
testParseDEMWhitespace :: Test
testParseDEMWhitespace = TestList
  [ "error with extra spaces" ~:
      DEMError 0.01 [TargetDetector (DetectorId 0)]
      ~=? run parseDEMError "error( 0.01 ) D0"

  , "detector with tabs" ~:
      DEMDetector (DetectorId 0) [1.0, 2.0]
      ~=? run parseDEMDetector "detector(1,\t2)\tD0"
  ]

-- | Edge cases
testParseDEMEdgeCases :: Test
testParseDEMEdgeCases = TestList
  [ "large detector id" ~:
      TargetDetector (DetectorId 999999)
      ~=? run parseDEMTarget "D999999"

  , "large observable id" ~:
      TargetObservable (ObservableId 999999)
      ~=? run parseDEMTarget "L999999"

  , "repeated same target" ~:
      DEMError 0.1 [TargetDetector (DetectorId 0), TargetDetector (DetectorId 0)]
      ~=? run parseDEMError "error(0.1) D0 D0"

  , "empty dem string" ~:
      DEM [] ~=? run parseDEM ""

  , "dem with trailing newline" ~:
      DEM [DEMInstrError (DEMError 0.01 [TargetDetector (DetectorId 0)])]
      ~=? run parseDEM "error(0.01) D0\n"
  ]

-- | Block and line comments
testParseDEMComments :: Test
testParseDEMComments = TestList
  [ "block comment between tokens" ~:
      DEMError 0.01 [TargetDetector (DetectorId 0)]
      ~=? run parseDEMError "error(0.01) (* comment *) D0"

  , "line comment at end" ~:
      DEMError 0.01 [TargetDetector (DetectorId 0)]
      ~=? run parseDEMError "error(0.01) D0 # end of line comment"

  , "block comment in tuple" ~:
      DEMDetector (DetectorId 0) [1.0, 2.0]
      ~=? run parseDEMDetector "detector(1, (* x *) 2) D0"
  ]

-- | Scientific notation in coordinates
testParseDEMScientificCoords :: Test
testParseDEMScientificCoords = TestList
  [ "scientific notation in detector coords" ~:
      DEMDetector (DetectorId 0) [150.0, 0.0]
      ~=? run parseDEMDetector "detector(1.5e2, 0) D0"

  , "negative scientific in coords" ~:
      DEMDetector (DetectorId 1) [-1.5e-2, 2.0]
      ~=? run parseDEMDetector "detector(-1.5e-2, 2) D1"
  ]

-- | Mixed target orders
testParseDEMMixedTargets :: Test
testParseDEMMixedTargets = TestList
  [ "observable before detector" ~:
      DEMError 0.1 [TargetObservable (ObservableId 0), TargetDetector (DetectorId 0)]
      ~=? run parseDEMError "error(0.1) L0 D0"

  , "interleaved targets" ~:
      DEMError 0.1 [TargetObservable (ObservableId 0), TargetDetector (DetectorId 1), TargetObservable (ObservableId 2)]
      ~=? run parseDEMError "error(0.1) L0 D1 L2"
  ]

-- | Empty / minimal DEMs
testParseDEMEmpty :: Test
testParseDEMEmpty = TestList
  [ "empty string is empty dem" ~:
      DEM [] ~=? run parseDEM ""
  ]

-- | Caret (^) separator handling
testParseDEMCaret :: Test
testParseDEMCaret = TestList
  [ "single caret" ~:
      let input = "error(0.01) D0 ^ D1 L0"
          expected = DEM [DEMInstrError (DEMError 0.01 [TargetDetector (DetectorId 0), TargetDetector (DetectorId 1), TargetObservable (ObservableId 0)])]
      in expected ~=? run parseDEM input

  , "multiple carets" ~:
      let input = "error(0.01) D0 ^ D1 ^ D2 L0"
          expected = DEM [DEMInstrError (DEMError 0.01 [TargetDetector (DetectorId 0), TargetDetector (DetectorId 1), TargetDetector (DetectorId 2), TargetObservable (ObservableId 0)])]
      in expected ~=? run parseDEM input

  , "caret with multiple lines" ~:
      let input = "error(0.01) D0 ^ D1 L0\nerror(0.02) D2"
          expected = DEM
            [ DEMInstrError (DEMError 0.01 [TargetDetector (DetectorId 0), TargetDetector (DetectorId 1), TargetObservable (ObservableId 0)])
            , DEMInstrError (DEMError 0.02 [TargetDetector (DetectorId 2)])
            ]
      in expected ~=? run parseDEM input

  , "caret only between detectors" ~:
      let input = "error(0.01) D0 ^ D1 ^ D2"
          expected = DEM [DEMInstrError (DEMError 0.01 [TargetDetector (DetectorId 0), TargetDetector (DetectorId 1), TargetDetector (DetectorId 2)])]
      in expected ~=? run parseDEM input

  , "caret with observables interleaved" ~:
      let input = "error(0.01) D0 ^ L0 ^ D1"
          expected = DEM [DEMInstrError (DEMError 0.01 [TargetDetector (DetectorId 0), TargetObservable (ObservableId 0), TargetDetector (DetectorId 1)])]
      in expected ~=? run parseDEM input

  , "caret at start of targets" ~:
      let input = "error(0.01) ^ D0 D1"
          expected = DEM [DEMInstrError (DEMError 0.01 [TargetDetector (DetectorId 0), TargetDetector (DetectorId 1)])]
      in expected ~=? run parseDEM input

  , "caret at end of targets" ~:
      let input = "error(0.01) D0 D1 ^"
          expected = DEM [DEMInstrError (DEMError 0.01 [TargetDetector (DetectorId 0), TargetDetector (DetectorId 1)])]
      in expected ~=? run parseDEM input

  , "caret between detector and observable on same line with detector after" ~:
      let input = "error(0.01) D0 ^ D1 L0 ^ D2"
          expected = DEM [DEMInstrError (DEMError 0.01 [TargetDetector (DetectorId 0), TargetDetector (DetectorId 1), TargetObservable (ObservableId 0), TargetDetector (DetectorId 2)])]
      in expected ~=? run parseDEM input
  ]

-- | EOF enforcement — parseDEM must consume all input
testParseDEMEofEnforced :: Test
testParseDEMEofEnforced = TestList
  [ "invalid trailing text fails" ~: TestCase $ do
      let result = runParser parseDEM "" "error(0.01) D0 invalid_trailing"
      case result of
        Left _ -> return ()  -- expected to fail
        Right _ -> assertFailure "parser should fail on unconsumed input"

  , "invalid token after caret fails" ~: TestCase $ do
      let result = runParser parseDEM "" "error(0.01) D0 ^ @#$"
      case result of
        Left _ -> return ()  -- expected to fail
        Right _ -> assertFailure "parser should fail on invalid token after caret"

  , "partial detector id fails" ~: TestCase $ do
      let result = runParser parseDEM "" "error(0.01) D"
      case result of
        Left _ -> return ()  -- expected to fail
        Right _ -> assertFailure "parser should fail on partial detector id"

  , "unclosed parenthesis fails" ~: TestCase $ do
      let result = runParser parseDEM "" "error(0.01"
      case result of
        Left _ -> return ()  -- expected to fail
        Right _ -> assertFailure "parser should fail on unclosed parenthesis"
  ]
