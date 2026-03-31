module Test.Parse where

import Test.HUnit
import StimParser.Parse
import StimParser.ParseUtils (run)
import StimParser.Expr

tests :: Test
tests = TestList
  [ testParsePauli
  , testParsePauliInd
  , testParsePauliChain
  , testParseQ
  , testParseGateTy
  , testParseGate
  , testParseMeasureTy
  , testParsePh
  , testParseMeasure
  , testParseGppTy
  , testParseGpp
  , testParseNoiseTy
  , testParseErrorTag
  , testParseAnnTy
  , testParseFInd
  , testParseAnn
  ]

-- Helper to compare using show (since types don't derive Eq)
assertShowEqual :: (Show a) => String -> a -> a -> Test
assertShowEqual name expected actual = 
  TestCase $ assertEqual name (show expected) (show actual)

-- Test Pauli parsing
testParsePauli :: Test
testParsePauli = TestList
  [ assertShowEqual "parsePauli X" PX (run parsePauli "X")
  , assertShowEqual "parsePauli Y" PY (run parsePauli "Y")
  , assertShowEqual "parsePauli Z" PZ (run parsePauli "Z")
  ]

-- Test PauliInd parsing
testParsePauliInd :: Test
testParsePauliInd = TestList
  [ assertShowEqual "parsePauliInd X0" (PauliInd PX 0) (run parsePauliInd "X0")
  , assertShowEqual "parsePauliInd Y5" (PauliInd PY 5) (run parsePauliInd "Y5")
  , assertShowEqual "parsePauliInd Z10" (PauliInd PZ 10) (run parsePauliInd "Z10")
  ]

-- Test PauliChain parsing
testParsePauliChain :: Test
testParsePauliChain = TestList
  [ assertShowEqual "parsePauliChain simple" 
      (P [PauliInd PX 1]) (run parsePauliChain "X1")
  , assertShowEqual "parsePauliChain with *" 
      (P [PauliInd PX 1, PauliInd PX 2]) (run parsePauliChain "X1*X2")
  , assertShowEqual "parsePauliChain multiple" 
      (P [PauliInd PX 1, PauliInd PY 2, PauliInd PZ 3]) (run parsePauliChain "X1*Y2*Z3")
  , assertShowEqual "parsePauliChain negated" 
      (N [PauliInd PZ 5]) (run parsePauliChain "!Z5")
  , assertShowEqual "parsePauliChain negated multiple" 
      (N [PauliInd PX 1, PauliInd PZ 2]) (run parsePauliChain "!X1*Z2")
  ]

-- Test Q parsing
testParseQ :: Test
testParseQ = TestList
  [ assertShowEqual "parseQ simple" (Q 5) (run parseQ "5")
  , assertShowEqual "parseQ rec" (QRec (Rec (-1))) (run parseQ "rec[-1]")
  , assertShowEqual "parseQ sweep" (QSweep (Sweep 5)) (run parseQ "sweep[5]")
  , assertShowEqual "parseQ not" (Not 3) (run parseQ "!3")
  ]

-- Test GateTy parsing
testParseGateTy :: Test
testParseGateTy = TestList
  [ assertShowEqual "parseGateTy I" I (run parseGateTy "I")
  , assertShowEqual "parseGateTy X" X (run parseGateTy "X")
  , assertShowEqual "parseGateTy H" H (run parseGateTy "H")
  , assertShowEqual "parseGateTy CNOT" CNOT (run parseGateTy "CNOT")
  , assertShowEqual "parseGateTy SQRT_X" SQRT_X (run parseGateTy "SQRT_X")
  , assertShowEqual "parseGateTy ISWAP_DAG" ISWAP_DAG (run parseGateTy "ISWAP_DAG")
  ]

-- Test Gate parsing
testParseGate :: Test
testParseGate = TestList
  [ assertShowEqual "parseGate single qubit" 
      (Gate X [Q 0]) (run parseGate "X 0")
  , assertShowEqual "parseGate multiple qubits" 
      (Gate H [Q 0, Q 1, Q 2]) (run parseGate "H 0 1 2")
  , assertShowEqual "parseGate with rec" 
      (Gate CY [QRec (Rec (-1)), Q 6]) (run parseGate "CY rec[-1] 6")
  , assertShowEqual "parseGate with sweep" 
      (Gate CY [QSweep (Sweep 5), Q 7, QSweep (Sweep 5), Q 8]) (run parseGate "CY sweep[5] 7 sweep[5] 8")
  , assertShowEqual "parseGate with not" 
      (Gate X [Not 1, Q 2]) (run parseGate "X !1 2")
  ]

-- Test MeasureTy parsing
testParseMeasureTy :: Test
testParseMeasureTy = TestList
  [ assertShowEqual "parseMeasureTy M" M (run parseMeasureTy "M")
  , assertShowEqual "parseMeasureTy MX" MX (run parseMeasureTy "MX")
  , assertShowEqual "parseMeasureTy MXX" MXX (run parseMeasureTy "MXX")
  , assertShowEqual "parseMeasureTy MR" MR (run parseMeasureTy "MR")
  ]

-- Test Ph (phase) parsing
testParsePh :: Test
testParsePh = TestList
  [ "parsePh float" ~: 0.02 ~=? run parsePh "(0.02)"
  , "parsePh negative" ~: (-0.5) ~=? run parsePh "(-0.5)"
  ]

-- Test Measure parsing
testParseMeasure :: Test
testParseMeasure = TestList
  [ assertShowEqual "parseMeasure simple" 
      (Measure M Nothing [Q 5]) (run parseMeasure "M 5")
  , assertShowEqual "parseMeasure with phase" 
      (Measure MZ (Just 0.02) [Q 2, Q 3, Q 5]) (run parseMeasure "MZ(0.02) 2 3 5")
  , assertShowEqual "parseMeasure pair" 
      (Measure MXX Nothing [Q 1, Q 2]) (run parseMeasure "MXX 1 2")
  , assertShowEqual "parseMeasure pair with phase" 
      (Measure MXX (Just 0.01) [Q 2, Q 3]) (run parseMeasure "MXX(0.01) 2 3")
  , assertShowEqual "parseMeasure with not" 
      (Measure MZ Nothing [Not 5]) (run parseMeasure "MZ !5")
  ]

-- Test GppTy parsing
testParseGppTy :: Test
testParseGppTy = TestList
  [ assertShowEqual "parseGppTy MPP" MPP (run parseGppTy "MPP")
  , assertShowEqual "parseGppTy SPP" SPP (run parseGppTy "SPP")
  , assertShowEqual "parseGppTy SPP_DAG" SPP_DAG (run parseGppTy "SPP_DAG")
  ]

-- Test Gpp parsing
testParseGpp :: Test
testParseGpp = TestList
  [ assertShowEqual "parseGpp simple" 
      (Gpp MPP Nothing [P [PauliInd PX 2, PauliInd PX 3, PauliInd PX 5, PauliInd PX 7]]) 
      (run parseGpp "MPP X2*X3*X5*X7")
  , assertShowEqual "parseGpp with negation" 
      (Gpp MPP Nothing [N [PauliInd PZ 5]]) 
      (run parseGpp "MPP !Z5")
  , assertShowEqual "parseGpp with phase" 
      (Gpp MPP (Just 0.001) [P [PauliInd PZ 1, PauliInd PZ 2], P [PauliInd PX 1, PauliInd PX 2]]) 
      (run parseGpp "MPP(0.001) Z1*Z2 X1*X2")
  , assertShowEqual "parseGpp mixed" 
      (Gpp SPP Nothing [N [PauliInd PX 1, PauliInd PY 2, PauliInd PZ 3]]) 
      (run parseGpp "SPP !X1*Y2*Z3")
  ]

-- Test NoiseTy parsing
testParseNoiseTy :: Test
testParseNoiseTy = TestList
  [ assertShowEqual "parseNoiseTy E" E (run parseNoiseTy "E")
  , assertShowEqual "parseNoiseTy X_ERROR" X_ERROR (run parseNoiseTy "X_ERROR")
  , assertShowEqual "parseNoiseTy DEPOLARIZE1" DEPOLARIZE1 (run parseNoiseTy "DEPOLARIZE1")
  , assertShowEqual "parseNoiseTy CORRELATED_ERROR" CORRELATED_ERROR (run parseNoiseTy "CORRELATED_ERROR")
  ]

-- Test ErrorTag parsing
testParseErrorTag :: Test
testParseErrorTag = TestList
  [ assertShowEqual "parseErrorTag simple" 
      (ErrorTag MULTIPLE_NOISE_MECHANISMS) (run parseErrorTag "MULTIPLE_NOISE_MECHANISMS")
  , assertShowEqual "parseErrorTag with coef" 
      (ErrorTagCoef LEAKAGE_NOISE_FOR_AN_ADVANCED_SIMULATOR 0.1) 
      (run parseErrorTag "LEAKAGE_NOISE_FOR_AN_ADVANCED_SIMULATOR:0.1")
  ]

-- Test AnnTy parsing
testParseAnnTy :: Test
testParseAnnTy = TestList
  [ assertShowEqual "parseAnnTy DETECTOR" DETECTOR (run parseAnnTy "DETECTOR")
  , assertShowEqual "parseAnnTy TICK" TICK (run parseAnnTy "TICK")
  , assertShowEqual "parseAnnTy SHIFT_COORDS" SHIFT_COORDS (run parseAnnTy "SHIFT_COORDS")
  , assertShowEqual "parseAnnTy QUBIT_COORDS" QUBIT_COORDS (run parseAnnTy "QUBIT_COORDS")
  ]

-- Test FInd parsing
testParseFInd :: Test
testParseFInd = TestList
  [ assertShowEqual "parseFInd Int" (In 5) (run parseFInd "5")
  , assertShowEqual "parseFInd Float" (Fl 3.14) (run parseFInd "3.14")
  , assertShowEqual "parseFInd negative Int" (In (-3)) (run parseFInd "-3")
  , assertShowEqual "parseFInd negative Float" (Fl (-2.5)) (run parseFInd "-2.5")
  ]

-- Test Ann parsing
testParseAnn :: Test
testParseAnn = TestList
  [ assertShowEqual "parseAnn TICK" 
      (Ann TICK [] []) (run parseAnn "TICK")
  , assertShowEqual "parseAnn DETECTOR with coords and rec" 
      (Ann DETECTOR [In 1, In 0] [QRec (Rec (-3)), QRec (Rec (-6))]) 
      (run parseAnn "DETECTOR(1, 0) rec[-3] rec[-6]")
  , assertShowEqual "parseAnn DETECTOR without coords" 
      (Ann DETECTOR [] [QRec (Rec (-3)), QRec (Rec (-4))]) 
      (run parseAnn "DETECTOR rec[-3] rec[-4]")
  , assertShowEqual "parseAnn SHIFT_COORDS" 
      (Ann SHIFT_COORDS [Fl 500.5] []) 
      (run parseAnn "SHIFT_COORDS(500.5)")
  , assertShowEqual "parseAnn OBSERVABLE_INCLUDE" 
      (Ann OBSERVABLE_INCLUDE [In 0] [QRec (Rec (-1))]) 
      (run parseAnn "OBSERVABLE_INCLUDE(0) rec[-1]")
  ]
