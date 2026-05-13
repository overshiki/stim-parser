module Test.DEM.Expr where

import Test.HUnit
import StimParser.DEM.Expr

-- Helper to compare using show
tAssertShowEqual :: (Show a) => String -> a -> a -> Test
tAssertShowEqual msg expected actual = TestCase $
  assertEqual msg (show expected) (show actual)

tests :: Test
tests = TestList
  [ testFlattenDEMSimple
  , testFlattenDEMRepeat
  , testFlattenDEMShift
  , testFlattenDEMShiftAndRepeat
  , testFlattenDEMDetIdShift
  , testFlattenDEMDimensionMismatch
  , testFlattenDEMBareShift
  , testFlattenDEMIdempotent
  ]

-- | Flattening a DEM with no repeats or shifts is identity
testFlattenDEMSimple :: Test
testFlattenDEMSimple = TestList
  [ "flatten simple error" ~:
      let dem = DEM [DEMInstrError (DEMError 0.01 [TargetDetector (DetectorId 0)])]
      in dem ~=? flattenDEM dem

  , "flatten multiple instructions" ~:
      let dem = DEM
            [ DEMInstrError (DEMError 0.01 [TargetDetector (DetectorId 0)])
            , DEMInstrError (DEMError 0.02 [TargetDetector (DetectorId 1), TargetObservable (ObservableId 0)])
            , DEMInstrDetector (DEMDetector (DetectorId 0) [0.0, 0.0])
            , DEMInstrObservable (DEMObservable (ObservableId 0))
            ]
      in dem ~=? flattenDEM dem
  ]

-- | Repeat blocks are expanded
testFlattenDEMRepeat :: Test
testFlattenDEMRepeat = TestList
  [ "flatten repeat 2" ~:
      let input = DEM
            [ DEMInstrRepeat 2
                [ DEMInstrError (DEMError 0.01 [TargetDetector (DetectorId 0)])
                ]
            ]
          expected = DEM
            [ DEMInstrError (DEMError 0.01 [TargetDetector (DetectorId 0)])
            , DEMInstrError (DEMError 0.01 [TargetDetector (DetectorId 0)])
            ]
      in expected ~=? flattenDEM input

  , "flatten repeat 0" ~:
      let input = DEM [DEMInstrRepeat 0 [DEMInstrError (DEMError 0.01 [])]]
      in DEM [] ~=? flattenDEM input

  , "flatten nested repeat" ~:
      let input = DEM
            [ DEMInstrRepeat 2
                [ DEMInstrRepeat 3
                    [ DEMInstrError (DEMError 0.01 []) ]
                ]
            ]
          expected = DEM (replicate 6 (DEMInstrError (DEMError 0.01 [])))
      in expected ~=? flattenDEM input
  ]

-- | shift_detectors coordinates are applied to subsequent detectors
testFlattenDEMShift :: Test
testFlattenDEMShift = TestList
  [ "flatten shift detectors" ~:
      let input = DEM
            [ DEMInstrShift (DEMShift [1.0, 0.0] 0)
            , DEMInstrDetector (DEMDetector (DetectorId 0) [0.0, 0.0])
            , DEMInstrDetector (DEMDetector (DetectorId 1) [2.0, 3.0])
            ]
          expected = DEM
            [ DEMInstrDetector (DEMDetector (DetectorId 0) [1.0, 0.0])
            , DEMInstrDetector (DEMDetector (DetectorId 1) [3.0, 3.0])
            ]
      in expected ~=? flattenDEM input

  , "flatten cumulative shift" ~:
      let input = DEM
            [ DEMInstrShift (DEMShift [1.0] 0)
            , DEMInstrDetector (DEMDetector (DetectorId 0) [0.0])
            , DEMInstrShift (DEMShift [2.0] 0)
            , DEMInstrDetector (DEMDetector (DetectorId 1) [0.0])
            ]
          expected = DEM
            [ DEMInstrDetector (DEMDetector (DetectorId 0) [1.0])
            , DEMInstrDetector (DEMDetector (DetectorId 1) [3.0])
            ]
      in expected ~=? flattenDEM input
  ]

-- | Combined shift and repeat
testFlattenDEMShiftAndRepeat :: Test
testFlattenDEMShiftAndRepeat = TestList
  [ "flatten shift inside repeat" ~:
      let input = DEM
            [ DEMInstrRepeat 2
                [ DEMInstrShift (DEMShift [1.0] 0)
                , DEMInstrDetector (DEMDetector (DetectorId 0) [0.0])
                ]
            ]
          -- Note: shifts are cumulative across repeat iterations.
          -- Iteration 1: shift=[1.0], detector gets [1.0]
          -- Iteration 2: shift becomes [2.0], detector gets [2.0]
          expected = DEM
            [ DEMInstrDetector (DEMDetector (DetectorId 0) [1.0])
            , DEMInstrDetector (DEMDetector (DetectorId 0) [2.0])
            ]
      in expected ~=? flattenDEM input
  ]

-- | Detector ID shifting
testFlattenDEMDetIdShift :: Test
testFlattenDEMDetIdShift = TestList
  [ "flatten detector id shift" ~:
      let input = DEM
            [ DEMInstrShift (DEMShift [0.0] 5)
            , DEMInstrDetector (DEMDetector (DetectorId 0) [1.0])
            , DEMInstrDetector (DEMDetector (DetectorId 1) [2.0])
            ]
          expected = DEM
            [ DEMInstrDetector (DEMDetector (DetectorId 5) [1.0])
            , DEMInstrDetector (DEMDetector (DetectorId 6) [2.0])
            ]
      in expected ~=? flattenDEM input

  , "flatten combined coord and id shift" ~:
      let input = DEM
            [ DEMInstrShift (DEMShift [1.0, 0.0] 10)
            , DEMInstrDetector (DEMDetector (DetectorId 0) [0.0, 0.0])
            ]
          expected = DEM
            [ DEMInstrDetector (DEMDetector (DetectorId 10) [1.0, 0.0])
            ]
      in expected ~=? flattenDEM input
  ]

-- | Dimension mismatch between shift and detector coordinates
testFlattenDEMDimensionMismatch :: Test
testFlattenDEMDimensionMismatch = TestList
  [ "2D shift on 3D detector" ~:
      let input = DEM
            [ DEMInstrShift (DEMShift [1.0, 0.0] 0)
            , DEMInstrDetector (DEMDetector (DetectorId 0) [0.0, 0.0, 0.0])
            ]
          expected = DEM
            [ DEMInstrDetector (DEMDetector (DetectorId 0) [1.0, 0.0, 0.0])
            ]
      in expected ~=? flattenDEM input

  , "3D shift on 2D detector" ~:
      let input = DEM
            [ DEMInstrShift (DEMShift [1.0, 0.0, 0.0] 0)
            , DEMInstrDetector (DEMDetector (DetectorId 0) [0.0, 0.0])
            ]
          expected = DEM
            [ DEMInstrDetector (DEMDetector (DetectorId 0) [1.0, 0.0])
            ]
      in expected ~=? flattenDEM input
  ]

-- | shift_detectors without coordinates should preserve accumulated coords
testFlattenDEMBareShift :: Test
testFlattenDEMBareShift = TestList
  [ "bare shift preserves coords" ~:
      let input = DEM
            [ DEMInstrShift (DEMShift [1.0, 0.5] 0)
            , DEMInstrDetector (DEMDetector (DetectorId 0) [0.0, 0.0])
            , DEMInstrShift (DEMShift [] 10)
            , DEMInstrDetector (DEMDetector (DetectorId 1) [0.0, 0.0])
            ]
          expected = DEM
            [ DEMInstrDetector (DEMDetector (DetectorId 0) [1.0, 0.5])
            , DEMInstrDetector (DEMDetector (DetectorId 11) [1.0, 0.5])
            ]
      in expected ~=? flattenDEM input

  , "bare shift only det id" ~:
      let input = DEM
            [ DEMInstrShift (DEMShift [] 5)
            , DEMInstrDetector (DEMDetector (DetectorId 0) [2.0, 3.0])
            ]
          expected = DEM
            [ DEMInstrDetector (DEMDetector (DetectorId 5) [2.0, 3.0])
            ]
      in expected ~=? flattenDEM input

  , "mixed coord and bare shifts" ~:
      let input = DEM
            [ DEMInstrShift (DEMShift [1.0] 0)
            , DEMInstrShift (DEMShift [] 5)
            , DEMInstrShift (DEMShift [2.0] 0)
            , DEMInstrDetector (DEMDetector (DetectorId 0) [0.0])
            ]
          expected = DEM
            [ DEMInstrDetector (DEMDetector (DetectorId 5) [3.0])
            ]
      in expected ~=? flattenDEM input
  ]

-- | Idempotency: flattening an already-flattened DEM is a no-op
testFlattenDEMIdempotent :: Test
testFlattenDEMIdempotent = TestList
  [ "flatten is idempotent on simple" ~:
      let dem = DEM
            [ DEMInstrError (DEMError 0.01 [TargetDetector (DetectorId 0)])
            , DEMInstrDetector (DEMDetector (DetectorId 1) [1.0, 2.0])
            , DEMInstrObservable (DEMObservable (ObservableId 0))
            ]
      in flattenDEM dem ~=? flattenDEM (flattenDEM dem)

  , "flatten is idempotent on nested" ~:
      let dem = DEM
            [ DEMInstrRepeat 2
                [ DEMInstrShift (DEMShift [1.0] 0)
                , DEMInstrDetector (DEMDetector (DetectorId 0) [0.0])
                ]
            ]
      in flattenDEM dem ~=? flattenDEM (flattenDEM dem)
  ]
