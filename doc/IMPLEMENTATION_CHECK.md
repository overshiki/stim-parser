# Implementation Compliance Check: stim-parser vs stim.md Spec

This document analyzes the current `stim-parser` implementation against the `doc/stim.md` specification.

## Summary

| Category | Status | Notes |
|----------|--------|-------|
| **Basic Structure** | ✅ Mostly Compliant | Comments, whitespace handled correctly |
| **Target Types** | ⚠️ Partial | `rec[]` accepts any integer, not just negative |
| **Instructions - Gates** | ✅ Compliant | All 70+ Clifford gates supported |
| **Instructions - Measurements** | ✅ Compliant | All measurement types supported |
| **Instructions - Pauli Products** | ✅ Compliant | MPP, SPP, SPP_DAG supported |
| **Instructions - Noise** | ⚠️ Partial | Missing some argument combinations for noise tags |
| **Instructions - Annotations** | ✅ Compliant | All annotation types supported |
| **Control Flow** | ✅ Compliant | REPEAT blocks work correctly |
| **Case Sensitivity** | ❌ Non-Compliant | Parser is case-sensitive, spec requires case-insensitive |
| **General Tags** | ❌ Missing | v1.15+ tags like `TICK[100ns]` not supported |

---

## Detailed Analysis

### 1. File Encoding ✅

**Spec:** UTF-8 encoding, non-ASCII only in comments/tags  
**Implementation:** Haskell's String type uses Unicode, parser handles UTF-8 correctly.

### 2. Line Structure ✅

**Spec:**
```
<LINE> ::= <INDENT> (<INSTRUCTION> | <BLOCK_START> | <BLOCK_END>)? <COMMENT>? '\n'
<COMMENT> ::= '#' /[^\n]*/
```

**Implementation:** 
- Line comments: `L.skipLineComment "#"` ✅
- Block comments: `(* ... *)` ✅
- Whitespace handling via `L.space` ✅
- `!!!Start` prefix required (implementation-specific)

### 3. Case Sensitivity ❌

**Spec:** Names are case-insensitive (`CNOT` = `cnot` = `Cnot`)  
**Implementation:** Uses `string` which is case-sensitive.

**Example:**
```haskell
-- Current (case-sensitive)
parseGateTy = parseEnum gateTyList  -- Matches exact case

-- Would need to be case-insensitive
```

**Impact:** HIGH - Stim files using lowercase will fail to parse.

### 4. Instruction Tags (v1.15+) ❌

**Spec:** `INSTRUCTION[TAG]` format, e.g., `TICK[100ns]`, `X_ERROR[test](0.1) 5 6`

**Current Implementation:**
- Only supports tags for `II_ERROR` and `I_ERROR` noise channels
- General instruction tags not supported

**Missing:**
```stim
TICK[100ns]                    # Duration tag
X_ERROR[custom](0.1) 0         # Custom tag
H[my_gate] 0                   # Tagged gate
```

### 5. Target Types

#### 5.1 Qubit Targets ✅

**Spec:** `'!'? <uint>`  
**Implementation:** `Q Int` and `Not Int` - supports negation.

```haskell
data Q = Q Int | QRec Rec | QSweep Sweep | Not Int
```

#### 5.2 Measurement Record ⚠️

**Spec:** `rec[-N]` where N is a positive integer (negative index required)  
**Implementation:**
```haskell
qrec = do
  lstring "rec"
  lstring "["
  i <- parseInt      -- Accepts ANY signed integer
  lstring "]"
  return $ QRec (Rec i)
```

**Issue:** Parser accepts `rec[0]`, `rec[5]` (positive indices), which don't conform to spec.

#### 5.3 Sweep Bit Targets ✅

**Spec:** `sweep[<uint>]`  
**Implementation:** Correctly implemented.

#### 5.4 Pauli Targets ✅

**Spec:** `'!'? /[XYZ]/ <uint>`  
**Implementation:** 
```haskell
data PauliInd = PauliInd Pauli Ind  -- Pauli = PX | PY | PZ
```

Negation handled in `PauliChain` (N constructor).

### 6. Clifford Gates ✅

**Status:** All gates from spec are implemented.

| Category | Gates | Status |
|----------|-------|--------|
| Pauli | I, X, Y, Z | ✅ |
| Single Clifford | H, S, S_DAG, SQRT_X, SQRT_X_DAG, SQRT_Y, SQRT_Y_DAG, SQRT_Z, SQRT_Z_DAG | ✅ |
| Single Variants | H_XY, H_XZ, H_YZ, H_NXY, H_NXZ, H_NYZ | ✅ |
| 24 Clifford Rotations | C_NXYZ, C_NZYX, C_XNYZ, C_XYNZ, C_XYZ, C_ZNYX, C_ZYNX, C_ZYX | ✅ |
| Two-Qubit | CNOT/CX, CY, CZ, CZSWAP, CXSWAP | ✅ |
| SWAP family | SWAP, ISWAP, ISWAP_DAG, SWAPCX, SWAPCZ | ✅ |
| Square Root | SQRT_XX, SQRT_XX_DAG, SQRT_YY, SQRT_YY_DAG, SQRT_ZZ, SQRT_ZZ_DAG | ✅ |
| Controlled | XCX, XCY, XCZ, YCX, YCY, YCZ, ZCX, ZCY, ZCZ | ✅ |
| Identity | II | ✅ |

### 7. Collapsing Gates (Measurements & Resets) ✅

**Measurements:**
| Gate | Status |
|------|--------|
| M, MX, MY, MZ | ✅ |
| MXX, MYY, MZZ | ✅ |
| MR, MRX, MRY, MRZ | ✅ |

**Resets (in GateTy):**
| Gate | Status |
|------|--------|
| R, RX, RY, RZ | ✅ |

**Phase Argument:** `MZ(0.02) 2 3` format supported ✅

### 8. Generalized Pauli Product Gates ✅

| Gate | Status |
|------|--------|
| MPP | ✅ |
| SPP | ✅ |
| SPP_DAG | ✅ |

**Pauli Chain Syntax:**
- `X1*Y2*Z3` ✅
- `!Z5` (negation) ✅

### 9. Noise Channels ⚠️

**Implemented Types:**
| Type | Status |
|------|--------|
| X_ERROR, Y_ERROR, Z_ERROR | ✅ |
| DEPOLARIZE1, DEPOLARIZE2 | ✅ |
| PAULI_CHANNEL_1, PAULI_CHANNEL_2 | ✅ |
| CORRELATED_ERROR (E) | ✅ |
| ELSE_CORRELATED_ERROR | ✅ |
| HERALDED_ERASE | ✅ |
| HERALDED_PAULI_CHANNEL_1 | ✅ |
| II_ERROR, I_ERROR | ✅ |

**Error Tags Support:**
- `II_ERROR[TAG]` ✅
- `I_ERROR[TAG:coef]` ✅
- `II_ERROR[TAG](p1, p2)` ✅

**Note:** General instruction tags not supported (see section 4).

### 10. Annotations ✅

| Annotation | Status |
|------------|--------|
| TICK | ✅ |
| QUBIT_COORDS | ✅ |
| DETECTOR | ✅ |
| OBSERVABLE_INCLUDE | ✅ |
| SHIFT_COORDS | ✅ |
| MPAD | ✅ (in type, not well tested) |

**Coordinate handling:** Supports both integer and float coordinates ✅

### 11. Control Flow ✅

**REPEAT Blocks:**
```haskell
parseRepeat = do 
  lstring "REPEAT"
  i <- parseInt
  lstring "{"
  us <- parseUnitList
  lstring "}"
  return $ StimRepeat i us
```

**Spec compliance:**
- `REPEAT <count> { ... }` format ✅
- Nested blocks supported ✅
- Zero-count blocks (vacuous) not explicitly rejected ⚠️

### 12. Broadcasting ✅

**Spec:** Gates can apply to multiple targets: `CNOT 0 1 2 3`  
**Implementation:** `parseExhaust` handles variable number of targets ✅

---

## Issues Found

### Critical Issues

#### Issue 1: Case Sensitivity ❌
**Description:** Parser is case-sensitive, spec requires case-insensitive.

**Test:**
```haskell
run parseGate "cnot 0 1"  -- Will fail
run parseGate "CNOT 0 1"  -- Works
```

**Fix:** Modify `parseEnum` or add case-insensitive string matching.

#### Issue 2: rec[] Index Constraint ⚠️
**Description:** Parser accepts any integer in `rec[]`, spec requires negative indices.

**Spec:** `rec[-N]` where N > 0  
**Current:** Accepts `rec[0]`, `rec[5]` (positive)

**Fix:** Add validation to ensure negative values only.

### Minor Issues

#### Issue 3: General Instruction Tags ❌
**Description:** v1.15+ feature `INSTRUCTION[TAG]` not supported.

**Missing:**
```stim
TICK[100ns]
H[custom] 0
M[flag] 0
```

**Note:** This is a newer feature (v1.15+), may be intentional omission.

#### Issue 4: MPAD Annotation
**Description:** `MPAD` is in the type definition but not well-documented or tested.

#### Issue 5: Pauli Target Negation
**Description:** The `Not` constructor is for qubit indices, but the spec allows `!X5` (negated Pauli target).

Current Pauli negation only works in PauliChain context, not standalone Pauli targets.

---

## Test Coverage

Running `cabal test`:
```
Cases: 86  Tried: 86  Errors: 0  Failures: 0
Test suite stim-parser-test: PASS
```

All existing tests pass. However, tests don't cover:
- Case insensitivity (no tests for lowercase)
- Invalid rec[] indices (positive values)
- Tagged instructions

---

## Recommendations

### Priority 1 (Critical)
1. **Fix case sensitivity** - Make instruction names case-insensitive to comply with Stim spec
2. **Validate rec[] indices** - Ensure only negative integers are accepted

### Priority 2 (High)
3. Add comprehensive tests for edge cases
4. Document the `!!!Start` prefix requirement (implementation-specific)

### Priority 3 (Medium)
5. Add support for general instruction tags (v1.15+ feature)
6. Consider supporting positive rec[] indices as a compiler warning rather than parse error

### Priority 4 (Low)
7. Add more examples with complex circuits
8. Test MPAD annotation

---

## Conclusion

The `stim-parser` implementation is **mostly compliant** with the Stim circuit file format specification. The core functionality (gates, measurements, noise, annotations, control flow) is correctly implemented.

**Main gaps:**
1. Case sensitivity (HIGH priority fix needed)
2. `rec[]` validation (MEDIUM priority)
3. General instruction tags (lower priority, v1.15+ feature)

**Estimated compliance:** ~85-90% for core Stim features, ~70% including v1.15+ features.
