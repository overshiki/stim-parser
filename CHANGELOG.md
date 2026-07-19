# Revision history for stim-parser

## 0.2.0.1 -- 2026-07-18

* Fix noise-channel parsing order in `parseStim`.
  Noise-channel names such as `X_ERROR`, `Y_ERROR`, `Z_ERROR`, and
  `I_ERROR` share a prefix with gate names (`X`, `Y`, `Z`, `I`). The
  top-level dispatcher now tries noise before gates, preventing the gate
  parser from partially consuming these names and leaving the rest of the
  input orphaned.
* Add regression tests for noise-channel parsing through `parseStim`.

## 0.2.0.0 -- 2026-04-01

* Add DEM (Detector Error Model) parsing support.
  New modules: `StimParser.DEM.Expr`, `StimParser.DEM.Parse`.
* Add `flattenDEM` utility to expand `repeat` blocks and apply
  `shift_detectors` coordinate/ID shifts.
* Migrate all floating-point types from `Float` to `Double` for
  higher precision probability representation.
* Move `parseExhaust`, `parseTuple`, and `parseTupleFloat` to
  `StimParser.ParseUtils` as shared lexer utilities.
* Add `parseNumber` for parsing bare integers or floats as `Double`.
* Expand test suite with DEM parser and flattening tests.

## 0.1.0.0 -- 2026-03-31

* Initial release.
* Parse STIM quantum circuit files into a Haskell AST.
* Supported circuit elements:
    * Gates (Pauli, single-qubit Clifford, two-qubit Clifford,
      collapsing gates)
    * Measurements (M, MXX, MYY, MZZ, MRX, MRY, MRZ, MX, MY, MZ, MR)
    * Generalized Pauli Product gates (MPP, SPP, SPP_DAG)
    * Noise channels (DEPOLARIZE, PAULI_CHANNEL, X/Y/Z_ERROR,
      CORRELATED_ERROR, etc.)
    * Annotations (DETECTOR, OBSERVABLE_INCLUDE, QUBIT_COORDS,
      SHIFT_COORDS, TICK, MPAD)
    * REPEAT blocks
    * Tags on instructions
* `StimParser.Trans.flattenQ` transform to resolve `rec[]` references
  into absolute qubit indices, with coordinate shift support.
* Two example executables: `stim-parser-example`,
  `stim-parser-unit-example`.
* Test suite covering expression types, parsers, parse utilities,
  and transformations.
