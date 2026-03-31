import StimParser.Parse
import StimParser.ParseUtils (run)

-- Parse a stim gate
main :: IO ()
main = do
  -- Parse individual elements
  print $ run parseGate "CNOT 0 1 2 3"
  -- => Gate CNOT [Q 0,Q 1,Q 2,Q 3]
  
  print $ run parseMeasure "MXX(0.01) 2 3"
  -- => Measure MXX (Just 0.01) [Q 2,Q 3]
  
  -- Parse a full stim file
  s <- readFile "data/example.stim"
  print $ run parseStim ("!!!Start " ++ s)
  -- => StimList [...]