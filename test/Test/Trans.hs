module Test.Trans where

import Test.HUnit
import Control.Monad.State.Lazy (evalState, runState)

import StimParser.Trans
import StimParser.Expr

tests :: Test
tests = TestList
  [ testFlattenQQ
  , testFlattenQGate
  , testFlattenQMeasure
  , testFlattenQGpp
  , testFlattenQNoise
  , testFlattenQAnn
  , testFlattenQStim
  ]

-- Helper to compare using show (since types don't derive Eq)
assertShowEqual :: (Show a) => String -> a -> a -> Test
assertShowEqual name expected actual = 
  TestCase $ assertEqual name (show expected) (show actual)

-- Test FlattenQ instance for Q
testFlattenQQ :: Test
testFlattenQQ = TestList
  [ assertShowEqual "flattenQ Q simple" 
      (Q 5) (evalState (flattenQ (Q 5)) (0, Coords [In 0]))
  , assertShowEqual "flattenQ QRec with count 0" 
      (Q 0) (evalState (flattenQ (QRec (Rec (-1)))) (0, Coords [In 0]))
  , assertShowEqual "flattenQ QRec with count 5" 
      (Q 5) (evalState (flattenQ (QRec (Rec (-1)))) (5, Coords [In 0]))
  , assertShowEqual "flattenQ QRec rec[-3] with count 10" 
      (Q 8) (evalState (flattenQ (QRec (Rec (-3)))) (10, Coords [In 0]))
  ]

-- Test FlattenQ instance for Gate
testFlattenQGate :: Test
testFlattenQGate = TestList
  [ assertShowEqual "flattenQ Gate simple" 
      (Gate X [Q 0, Q 1]) (evalState (flattenQ (Gate X [Q 0, Q 1])) (0, Coords [In 0]))
  , assertShowEqual "flattenQ Gate with rec" 
      (Gate X [Q 10, Q 9]) (evalState (flattenQ (Gate X [QRec (Rec (-1)), QRec (Rec (-2))])) (10, Coords [In 0]))
  ]

-- Test FlattenQ instance for Measure
testFlattenQMeasure :: Test
testFlattenQMeasure = TestCase $ do
  -- Test that count is updated
  let (result, (count, _)) = runState (flattenQ (Measure M Nothing [Q 0, Q 1])) (0, Coords [In 0])
  assertEqual "count increased by 2" 2 count
  assertEqual "result unchanged" (show (Measure M Nothing [Q 0, Q 1])) (show result)
  
  -- Test single qubit
  let (_, (count2, _)) = runState (flattenQ (Measure MX Nothing [Q 0])) (5, Coords [In 0])
  assertEqual "count increased to 6" 6 count2

-- Test FlattenQ instance for Gpp
testFlattenQGpp :: Test
testFlattenQGpp = TestCase $ do
  let (result, (count, _)) = runState (flattenQ (Gpp MPP Nothing [P [PauliInd PX 1]])) (0, Coords [In 0])
  assertEqual "count increased by 1" 1 count
  assertEqual "result unchanged" (show (Gpp MPP Nothing [P [PauliInd PX 1]])) (show result)

-- Test FlattenQ instance for Noise
testFlattenQNoise :: Test
testFlattenQNoise = TestList
  [ assertShowEqual "flattenQ NoiseNormal simple" 
      (NoiseNormal X_ERROR Nothing [0.01] [Q 0, Q 1]) 
      (evalState (flattenQ (NoiseNormal X_ERROR Nothing [0.01] [Q 0, Q 1])) (0, Coords [In 0]))
  , assertShowEqual "flattenQ NoiseNormal with rec" 
      (NoiseNormal X_ERROR Nothing [0.01] [Q 10, Q 9]) 
      (evalState (flattenQ (NoiseNormal X_ERROR Nothing [0.01] [QRec (Rec (-1)), QRec (Rec (-2))])) (10, Coords [In 0]))
  , assertShowEqual "flattenQ NoiseE" 
      (NoiseE CORRELATED_ERROR 0.2 [PauliInd PX 1, PauliInd PZ 2]) 
      (evalState (flattenQ (NoiseE CORRELATED_ERROR 0.2 [PauliInd PX 1, PauliInd PZ 2])) (0, Coords [In 0]))
  ]

-- Test FlattenQ instance for Ann
testFlattenQAnn :: Test
testFlattenQAnn = TestList
  [ assertShowEqual "flattenQ Ann TICK" 
      (Just (Ann TICK [] [])) (evalState (flattenQ (Ann TICK [] [])) (0, Coords [In 0]))
  , assertShowEqual "flattenQ Ann DETECTOR" 
      (Just (Ann DETECTOR [In 1, In 0] [Q 10])) 
      (evalState (flattenQ (Ann DETECTOR [In 1, In 0] [Q 10])) (10, Coords [In 0, In 0]))
  ]
  -- Note: SHIFT_COORDS test moved to separate test case

-- Test FlattenQ instance for Stim
testFlattenQStim :: Test
testFlattenQStim = TestList
  [ assertShowEqual "flattenQ StimG" 
      (Just (StimG (Gate X [Q 0]))) 
      (evalState (flattenQ (StimG (Gate X [Q 0]))) (0, Coords [In 0]))
  , TestCase $ do
      let (result, (count, _)) = runState (flattenQ (StimM (Measure M Nothing [Q 0, Q 1]))) (0, Coords [In 0])
      assertEqual "count increased by 2" 2 count
      assertEqual "result" (show (Just (StimM (Measure M Nothing [Q 0, Q 1])))) (show result)
  , assertShowEqual "flattenQ StimList" 
      (Just (StimList [StimG (Gate X [Q 0]), StimG (Gate Y [Q 1])])) 
      (evalState (flattenQ (StimList [StimG (Gate X [Q 0]), StimG (Gate Y [Q 1])])) (0, Coords [In 0]))
  , TestCase $ do
      let (result, (count, _)) = runState (flattenQ (StimRepeat 2 (StimM (Measure M Nothing [Q 0])))) (0, Coords [In 0])
      assertEqual "count increased by 2" 2 count
      case result of
        Just (StimList ms) -> assertEqual "result has 2 elements" 2 (length ms)
        _ -> assertFailure "Expected Just (StimList _)"
  , assertShowEqual "flattenQ StimAnn SHIFT_COORDS returns Nothing" 
      (Nothing :: Maybe Stim) 
      (evalState (flattenQ (StimAnn (Ann SHIFT_COORDS [In 1] []))) (0, Coords [In 0]))
  ]
