# stim-parser 

parser combinator to parse `stim`'s `.stim` file into `AST`.

## Usage
stim-parser is expected to be used as a `haskell` library, for example:
```haskell
import StimParser.Expr
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
  s <- readFile "example.stim"
  print $ run parseStim ("!!!Start " ++ s)
  -- => StimList [...]
```

See `data/example.stim` for a sample input file.

## Executables & Tests

### stim-parser-example
Main example from the README usage section. Parses a sample stim file(`data/example.stim`):
```bash
cabal run stim-parser-example
```

### stim-parser-unit-example
Multiple unit examples demonstrating individual parsers (gates, measurements, noise channels, annotations, etc.):
```bash
cabal run stim-parser-unit-example
```

### Test Suite
Unit tests for all modules (Expr, ParseUtils, Parse, Trans):
```bash
cabal test
```
