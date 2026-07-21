# Revision history for stim-parser

## 0.4.1.0 -- 2026-07-20

* Parse DEM separator targets (`^`) inside `error(...)` instructions.
  Added `TargetSeparator` constructor to `DEMTarget`. This is a non-breaking
  API addition; downstream code pattern-matching on `DEMTarget` should add a
  wildcard or handle the new constructor.
* Require at least one target for gate and measurement instructions.
  `parseGate` and `parseMeasure` now use `some parseQ` instead of
  `parseExhaust parseQ`. This prevents single-letter gate/measure names such
  as `R`, `S`, `H`, `I`, and `M` from consuming the leading character of
  keywords like `REPEAT` and `SHIFT_COORDS` and producing misleading parse
  errors.

## 0.4.0.0 -- 2026-07-20

* Add Pauli-target support for `OBSERVABLE_INCLUDE` and `DETECTOR`
  annotations.
  - New `AnnTarget` sum type: `AnnQ Ind | AnnRec Rec | AnnPauli Pauli Ind`.
  - `Ann` now stores `[AnnTarget]` instead of `[Q]`.
  - Single-qubit Pauli targets such as `X0`, `Y1`, `Z2` are accepted and
    preserved, matching Python Stim behavior.
  - Mixed `rec[-k]` and Pauli targets are supported.
  - The `*` combiner remains unsupported inside annotations.
* This is a **breaking API change**. Downstream code pattern-matching on
  `Ann` or constructing annotation values needs to use `AnnTarget`.

## 0.3.0.0 -- 2026-07-20

* Add optional tag support for DEM detector and observable declarations.
  Stim's DEM format allows `detector[tag](...) D#` and
  `logical_observable[tag] L#`; these are now parsed and exposed in the AST.
  This is a **breaking API change**: `DEMDetector` and `DEMObservable`
  gained new `ddTag` / `doTag` fields of type `Maybe Tag`.
* Move `parseTag` from `StimParser.Parse` to `StimParser.ParseUtils` so it
  can be shared by the DEM parser. Modules importing `parseTag` from
  `StimParser.Parse` should import it from `StimParser.ParseUtils` instead.
* Add `Eq` instance for `StimParser.Expr.Tag`.
* Add DEM parser regression tests for tagged detectors/observables and
  rejection of double tags.

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
