# Stim Circuit File Format (.stim) Reference

## Overview

Stim is a fast stabilizer circuit simulator for quantum error correction (QEC) circuits. The `.stim` file format is a human-readable specification of an annotated stabilizer circuit. This document describes the complete syntax and semantics of the `.stim` file format based on the original [Stim project](https://github.com/quantumlib/stim) by Craig Gidney.

---

## File Encoding

- Stim circuit files are always encoded using **UTF-8**
- Non-ASCII characters are only valid inside comments and tags
- Case-insensitive instruction names

---

## Basic Syntax

### File Structure

A stim circuit file consists of a series of lines. Each line can be:
- **Blank** (empty or whitespace-only)
- **Instruction** - a quantum operation, annotation, or control flow
- **Block initiator** - starts a block with `{`
- **Block terminator** - ends a block with `}`

### Line Format

```
<LINE> ::= <INDENT> (<INSTRUCTION> | <BLOCK_START> | <BLOCK_END>)? <COMMENT>? '\n'
<INDENT> ::= /[ \t]*/          # Leading whitespace (decorative)
<COMMENT> ::= '#' /[^\n]*/     # Hash-style comments to end of line
```

### Instruction Format

```
<INSTRUCTION> ::= <NAME> <TAG>? <PARENS_ARGUMENTS>? <TARGETS>
<TAG> ::= '[' /[^\r\]\n]/* ']'              # Optional tag (v1.15+)
<PARENS_ARGUMENTS> ::= '(' <ARGUMENTS> ')'  # Optional arguments
<ARGUMENTS> ::= /[ \t]*/ <ARG> /[ \t]*/ (',' <ARGUMENTS>)?
<TARGETS> ::= /[ \t]+/ <TARG> <TARGETS>?
```

### Names

```
<NAME> ::= /[a-zA-Z][a-zA-Z0-9_]*/   # Starts with letter, followed by letters/digits/underscores
```

### Arguments

Arguments are double-precision floating-point numbers representing probabilities or other numeric parameters.

```
<ARG> ::= <double>   # e.g., 0.001, 0.5, -1.0
```

---

## Targets

Targets specify which qubits or measurement records an operation affects.

### Target Types

```
<TARG> ::= <QUBIT_TARGET> 
         | <MEASUREMENT_RECORD_TARGET> 
         | <SWEEP_BIT_TARGET> 
         | <PAULI_TARGET> 
         | <COMBINER_TARGET>
```

#### 1. Qubit Target

```
<QUBIT_TARGET> ::= '!'? <uint>
```
- A non-negative integer identifying a qubit
- Optional `!` prefix negates the measurement result
- Examples: `0`, `5`, `!3` (qubit 3 with negation)

#### 2. Measurement Record Target

```
<MEASUREMENT_RECORD_TARGET> ::= "rec[(-)?" <uint> "]"
```
- References a previous measurement result
- Negative indices count from most recent measurement (`rec[-1]` = most recent)
- Used for feedforward operations and detector annotations
- Examples: `rec[-1]`, `rec[-3]`, `rec[0]`

#### 3. Sweep Bit Target

```
<SWEEP_BIT_TARGET> ::= "sweep[" <uint> "]"
```
- References a bit from sweep data for parameterized simulations
- Examples: `sweep[0]`, `sweep[5]`

#### 4. Pauli Target

```
<PAULI_TARGET> ::= '!'? /[XYZ]/ <uint>
```
- Specifies a Pauli operator (X, Y, or Z) acting on a qubit
- Used in correlated error definitions and Pauli product measurements
- Optional `!` prefix negates the result
- Examples: `X5`, `Z3`, `!Y2`

#### 5. Combiner Target

```
<COMBINER_TARGET> ::= '*'
```
- Combines adjacent Pauli targets into a product
- Used in Pauli chains: `X1*Y2*Z3`

---

## Instruction Categories

### 1. Clifford Gates

Single-qubit Clifford gates:
| Gate | Description |
|------|-------------|
| `I` | Identity |
| `X`, `Y`, `Z` | Pauli gates |
| `H` | Hadamard |
| `H_XY`, `H_XZ`, `H_YZ` | Hadamard-like rotations between planes |
| `H_NXY`, `H_NXZ`, `H_NYZ` | Negated Hadamard variants |
| `S` | Phase gate (√Z) |
| `S_DAG` | Adjoint phase gate |
| `SQRT_X`, `SQRT_X_DAG` | √X and its adjoint |
| `SQRT_Y`, `SQRT_Y_DAG` | √Y and its adjoint |
| `SQRT_Z`, `SQRT_Z_DAG` | √Z and its adjoint (same as S, S_DAG) |
| `C_NXYZ` .. `C_ZYX` | 24 single-qubit Clifford rotations |

Two-qubit Clifford gates:
| Gate | Description |
|------|-------------|
| `CNOT`, `CX` | Controlled-NOT |
| `CY` | Controlled-Y |
| `CZ` | Controlled-Z |
| `CZSWAP`, `CXSWAP` | Controlled-SWAP variants |
| `SWAP` | SWAP gate |
| `SWAPCX`, `SWAPCZ` | SWAP followed by CX/CZ |
| `ISWAP`, `ISWAP_DAG` | iSWAP and its adjoint |
| `SQRT_XX`, `SQRT_XX_DAG` | √(XX) and adjoint |
| `SQRT_YY`, `SQRT_YY_DAG` | √(YY) and adjoint |
| `SQRT_ZZ`, `SQRT_ZZ_DAG` | √(ZZ) and adjoint |
| `XCX`, `XCY`, `XCZ` | X-controlled rotations |
| `YCX`, `YCY`, `YCZ` | Y-controlled rotations |
| `ZCX`, `ZCY`, `ZCZ` | Z-controlled rotations |
| `II` | Two-qubit identity |

**Syntax:**
```
GATE_NAME <target1> <target2> ...
```

**Examples:**
```stim
H 0 1 2          # Hadamard on qubits 0, 1, 2
CNOT 0 1         # CNOT with control 0, target 1
CNOT 0 1 2 3     # Two CNOTs: (0→1) and (2→3)
CY sweep[5] 7    # Controlled-Y with sweep bit control
CZ rec[-1] 5     # Controlled-Z conditioned on last measurement
```

### 2. Collapsing Gates (Measurements and Resets)

#### Single-Qubit Measurements
| Gate | Description |
|------|-------------|
| `M` | Measure in computational (Z) basis |
| `MX` | Measure in X basis |
| `MY` | Measure in Y basis |
| `MZ` | Measure in Z basis (same as M) |
| `MR` | Measure and reset to \|0⟩ |
| `MRX` | Measure in X basis and reset to \|+⟩ |
| `MRY` | Measure in Y basis and reset |
| `MRZ` | Measure in Z basis and reset to \|0⟩ (same as MR) |

#### Two-Qubit Measurements
| Gate | Description |
|------|-------------|
| `MXX` | Measure XX parity |
| `MYY` | Measure YY parity |
| `MZZ` | Measure ZZ parity |

#### Resets
| Gate | Description |
|------|-------------|
| `R` | Reset to \|0⟩ (Z basis) |
| `RX` | Reset to \|+⟩ (X basis) |
| `RY` | Reset to Y eigenstate |
| `RZ` | Reset to \|0⟩ (same as R) |

**Syntax:**
```
MEASURE_NAME(<probability>)? <target1> <target2> ...
```

The optional probability argument specifies a flip probability applied to the measurement result.

**Examples:**
```stim
M 0 1 2          # Measure qubits 0, 1, 2 in Z basis
MZ(0.02) 2 3     # Measure with 2% flip probability
MXX !1 2         # Measure XX parity, negate result for qubit 1
MX 5             # Measure qubit 5 in X basis
MR 1 3 5         # Measure and reset qubits 1, 3, 5
```

### 3. Generalized Pauli Product Gates

These gates measure or apply Pauli product operators.

| Gate | Description |
|------|-------------|
| `MPP` | Measure Pauli product |
| `SPP` | Apply Pauli product |
| `SPP_DAG` | Apply adjoint Pauli product |

**Syntax:**
```
GATE_NAME(<probability>)? <pauli_chain1> <pauli_chain2> ...
```

**Pauli Chain Syntax:**
```
PauliChain ::= ['!']? <Pauli> <qubit> ('*' <Pauli> <qubit>)*
Pauli ::= 'X' | 'Y' | 'Z'
```

**Examples:**
```stim
MPP X2*X3*X5*X7           # Measure product X₂X₃X₅X₇
MPP !Z5                   # Measure Z₅ with negation
MPP X1*Y2 !Z3*Z4*Z5       # Measure two products, second negated
MPP(0.001) Z1*Z2 X1*X2    # Measure with 0.1% flip probability
SPP !X1*Y2*Z3             # Apply Pauli product with negation
```

### 4. Noise Channels

Noise operations represent errors in the circuit.

#### Single-Qubit Noise
| Gate | Description |
|------|-------------|
| `X_ERROR(p)` | Apply X with probability p |
| `Y_ERROR(p)` | Apply Y with probability p |
| `Z_ERROR(p)` | Apply Z with probability p |
| `I_ERROR(p)` | Apply identity with probability p (leakage) |
| `DEPOLARIZE1(p)` | Apply random Pauli (X/Y/Z) with probability p |
| `PAULI_CHANNEL_1(px, py, pz)` | Apply X/Y/Z with given probabilities |

#### Two-Qubit Noise
| Gate | Description |
|------|-------------|
| `II_ERROR(p)` | Two-qubit identity error (leakage) |
| `DEPOLARIZE2(p)` | Two-qubit depolarizing noise |
| `PAULI_CHANNEL_2(...)` | Custom two-qubit Pauli channel (15 probabilities) |

#### Special Noise
| Gate | Description |
|------|-------------|
| `HERALDED_ERASE(p)` | Erase error with herald |
| `HERALDED_PAULI_CHANNEL_1(...)` | Heralded Pauli channel (4 probabilities) |
| `E` / `CORRELATED_ERROR(p)` | Correlated Pauli error |
| `ELSE_CORRELATED_ERROR(p)` | Conditional correlated error |

**Syntax:**
```
NOISE_NAME(<arg1>, <arg2>, ...) <target1> <target2> ...
CORRELATED_ERROR(p) <pauli_target1> <pauli_target2> ...
```

**Special Error Tags (for advanced simulators):**
```
II_ERROR[<tag>] <target1> <target2> ...
I_ERROR[<tag>] <target1> ...
```

Available tags:
- `TWO_QUBIT_LEAKAGE_NOISE_FOR_AN_ADVANCED_SIMULATOR`
- `LEAKAGE_NOISE_FOR_AN_ADVANCED_SIMULATOR`
- `MULTIPLE_TWO_QUBIT_NOISE_MECHANISMS`
- `MULTIPLE_NOISE_MECHANISMS`

Tag with coefficient: `TAG_NAME:value`

**Examples:**
```stim
DEPOLARIZE1(0.01) 2 3 5           # 1% depolarizing on qubits 2, 3, 5
DEPOLARIZE2(0.01) 0 1 2 3         # Two-qubit depolarizing on pairs (0,1) and (2,3)
X_ERROR(0.001) 5 42               # 0.1% X errors on qubits 5 and 42
PAULI_CHANNEL_1(0.1, 0.15, 0.2) 1 2 4   # Custom Pauli channel
CORRELATED_ERROR(0.2) X1 Y2       # Correlated X₁Y₂ error with 20% probability
ELSE_CORRELATED_ERROR(0.25) Z2 Z3 # Conditional Z₂Z₃ error
II_ERROR[LEAKAGE_NOISE_FOR_AN_ADVANCED_SIMULATOR:0.1] 0 1
```

### 5. Annotations

Annotations provide metadata for visualization, detector definitions, and circuit structure.

#### TICK
```
TICK
```
Marks a cycle boundary in the circuit. Used for timing visualization and noise modeling.

#### QUBIT_COORDS
```
QUBIT_COORDS(<x>, <y>?, <z>?) <qubit>
```
Assign spatial coordinates to a qubit for visualization.

**Examples:**
```stim
QUBIT_COORDS(0, 0) 0      # 2D coordinate
QUBIT_COORDS(1, 2, 3) 5   # 3D coordinate
```

#### DETECTOR
```
DETECTOR(<coord1>, <coord2>, ...)? <rec_target1> <rec_target2> ...
```
Defines a detector - a set of measurements that should have deterministic parity. Used for error detection in QEC.

**Examples:**
```stim
DETECTOR(1, 0) rec[-3] rec[-6]      # Detector at (1,0) comparing two measurements
DETECTOR rec[-3] rec[-4]            # Detector without coordinates
DETECTOR(2, 0, 0) rec[-8]           # 3D coordinates
```

#### OBSERVABLE_INCLUDE
```
OBSERVABLE_INCLUDE(<observable_index>) <rec_target1> <rec_target2> ...
```
Declares which measurements contribute to a logical observable.

**Example:**
```stim
OBSERVABLE_INCLUDE(0) rec[-1] rec[-2]    # Logical observable 0
```

#### SHIFT_COORDS
```
SHIFT_COORDS(<offset1>, <offset2>, ...)
```
Adds offsets to subsequent coordinate annotations. Useful for repeat blocks.

**Example:**
```stim
SHIFT_COORDS(0, 1)        # Shift second coordinate by 1
SHIFT_COORDS(500.5)       # Shift first coordinate
```

#### MPAD
```
MPAD <target1> <target2> ...
```
Measurement padding annotation.

---

## Control Flow

### REPEAT Blocks

Repeat a block of instructions multiple times.

```
REPEAT <count> {
    <instructions>
}
```

**Example:**
```stim
REPEAT 1000 {
    CNOT 0 1
    M 1
    DETECTOR rec[-1] rec[-2]
}
```

**Note:** Vacuous repeat blocks (count = 0) are not allowed.

---

## Broadcasting

Most operations support broadcasting - applying the same gate to multiple qubits:

```stim
# Apply H to qubits 0, 1, 2, 3
H 0 1 2 3

# Apply CNOT pairs: (0→1), (2→3), (4→5)
CNOT 0 1 2 3 4 5

# Apply DEPOLARIZE2 to pairs: (0,1), (2,3), (4,5)
DEPOLARIZE2(0.001) 0 1 2 3 4 5
```

---

## State Space Semantics

A Stim simulator maintains:

1. **Qubits** - Start in |0⟩ state. Number of qubits is implied by the largest qubit target.

2. **Measurement Record** - Immutable log of all measurement results. New results are appended.

3. **Correlated Error Flag** - Boolean tracking if the previous `CORRELATED_ERROR` occurred, used by `ELSE_CORRELATED_ERROR`.

---

## Complete Examples

### Example 1: Teleportation Circuit

```stim
# Distribute a Bell Pair.
H 0
CNOT 0 99

# Sender creates an arbitrary qubit state to send.
H 1
S 1

# Sender performs a Bell Basis measurement.
CNOT 0 1
H 0
M 0 1  # Measure both of the sender's qubits.

# Receiver performs frame corrections based on measurement results.
CZ rec[-2] 99
CNOT rec[-1] 99
```

### Example 2: Repetition Code

```stim
# Measure the parities of adjacent data qubits.
# Data qubits are 0, 2, 4, 6.
# Measurement qubits are 1, 3, 5.
CNOT 0 1 2 3 4 5
CNOT 2 1 4 3 6 5
MR 1 3 5

# Annotate that the measurements should be deterministic.
DETECTOR rec[-3]
DETECTOR rec[-2]
DETECTOR rec[-1]

# Perform 1000 more rounds of measurements.
REPEAT 1000 {
    # Measure the parities of adjacent data qubits.
    CNOT 0 1 2 3 4 5
    CNOT 2 1 4 3 6 5
    MR 1 3 5

    # Annotate that the measurements should agree with previous round.
    DETECTOR rec[-3] rec[-6]
    DETECTOR rec[-2] rec[-5]
    DETECTOR rec[-1] rec[-4]
}

# Measure data qubits.
M 0 2 4 6

# Annotate that the data measurements should agree with the parity measurements.
DETECTOR rec[-3] rec[-4] rec[-7]
DETECTOR rec[-2] rec[-3] rec[-6]
DETECTOR rec[-1] rec[-2] rec[-5]

# Declare one of the data qubit measurements to a logical measurement result.
OBSERVABLE_INCLUDE(0) rec[-1]
```

### Example 3: Noisy Surface Code Circuit

```stim
QUBIT_COORDS(1, 1) 1
QUBIT_COORDS(2, 0) 2
RX 1 3 5 8
R 2 9 11
TICK
H 2 11
DEPOLARIZE1(0.001) 2 11
TICK
CX 2 3 16 17 11 12
DEPOLARIZE2(0.001) 2 3 16 17 11 12
TICK
MR 2 9 11
DETECTOR(2, 0, 0) rec[-3]
REPEAT 999 {
    TICK
    H 2 11
    DEPOLARIZE1(0.001) 2 11
    TICK
    CX 2 3 16 17 11 12
    DEPOLARIZE2(0.001) 2 3 16 17 11 12
    TICK
    MR 2 9 11
    SHIFT_COORDS(0, 0, 1)
    DETECTOR(2, 0, 0) rec[-3] rec[-6]
}
MX 1 3 5 8
OBSERVABLE_INCLUDE(0) rec[-1]
```

---

## References

1. **Stim GitHub Repository**: https://github.com/quantumlib/stim
2. **Stim Paper**: Gidney, C. (2021). "Stim: a fast stabilizer circuit simulator", Quantum 5, 497
3. **Gate Reference**: See [Stim Gate Reference](https://github.com/quantumlib/stim/wiki)
4. **Python API**: `pip install stim`

---

## Version History

This documentation is based on Stim v1.13+ file format specifications. Key version features:

- **v1.15+**: Added instruction tags (e.g., `TICK[100ns]`)
- Earlier versions support all core features documented above

---

## Summary Table: Instruction Types

| Category | Examples | Purpose |
|----------|----------|---------|
| **Clifford Gates** | `H`, `CNOT`, `CZ`, `S`, `SWAP` | Unitary operations |
| **Measurements** | `M`, `MX`, `MXX`, `MR` | Collapsing measurements |
| **Resets** | `R`, `RX`, `RY` | Initialize qubit states |
| **Pauli Products** | `MPP`, `SPP` | Multi-qubit Pauli ops |
| **Noise** | `DEPOLARIZE1`, `X_ERROR`, `CORRELATED_ERROR` | Error channels |
| **Annotations** | `TICK`, `DETECTOR`, `OBSERVABLE_INCLUDE` | Metadata |
| **Control Flow** | `REPEAT` | Repetition blocks |
