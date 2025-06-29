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
