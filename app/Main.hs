module Main where
import Expr
import Parse
import ParseUtils

main :: IO ()
main = do 
  print gateTyList
  print $ run parseGateTy "I"
  print $ run parseGateTy "X"
  print $ run parseGateTy "C_NXYZ"
  print $ run parseGate "CY sweep[5] 7 sweep[5] 8 \n CY"
  print $ run parseMeasureTy "MXX"
  print $ run parseMeasure "MXX(0.01) 2 3 \n CY"
  print $ run parsePauliChain "X1*Z1*Y2 X1*Z1*Y2"
  print $ run parsePauliChain "!X1*Z1*Y2 !X1*Z1*Y2"
  print $ run parsePauliChain "X1*Z1 !X1*Z1*Y2"
  print $ run (parseExhaust parsePauliChain) "X1*Z1 !X1*Z1*Y2 \n CY"
  print $ run parseGpp "MPP(0.001) Z1*Z2 X1*X2 \n CY"
  print $ run parseGpp "MPP Z1*Z2 X1*X2 \n CY"
  print $ run parseNoise "HERALDED_PAULI_CHANNEL_1(0.01, 0.02, 0.03, 0.04) 0 1 \n CY"
  print $ run parseNoise "CORRELATED_ERROR(0.2) X1 Y2 \n CY"
  print $ run parseNoise "II_ERROR 0 1 \n CY"
  print $ run parseNoise "II_ERROR[TWO_QUBIT_LEAKAGE_NOISE_FOR_AN_ADVANCED_SIMULATOR:0.1] 0 2 4 6 \n CY"
  print $ run parseNoise "II_ERROR[MULTIPLE_TWO_QUBIT_NOISE_MECHANISMS](0.1, 0.2) 0 2 4 6 \n CY"

