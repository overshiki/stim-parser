module Expr where 

type Ind = Int
type Ph = Float
newtype Rec = Rec Int
newtype Sweep = Sweep Int 

-- example:
--    CY sweep[5] 7 sweep[5] 8
--    CY rec[-1] 6
--    CY 2 5 4 2
--    MXX !1 2
data Q = Q Int | QRec Rec | QSweep Sweep | Not Int

data Pauli = PX | PY | PZ 
data PauliInd = PauliS Pauli Ind

data PauliChain = P [PauliInd]
  | N [PauliInd]

data GateTy =
  -- Pauli
  I | X | Y | Z 
  -- Single Qubit Clifford Gates  
  | C_NXYZ | C_NZYX | C_XNYZ | C_XYNZ | C_XYZ | C_ZNYX | C_ZYNX | C_ZYX | H | H_NXY | H_NXZ | H_NYZ | H_XY | H_XZ | H_YZ | S | SQRT_X | SQRT_X_DAG | SQRT_Y | SQRT_Y_DAG | SQRT_Z | SQRT_Z_DAG | S_DAG 
  -- Two Qubit Clifford Gates 
  | CNOT | CX | CXSWAP | CY | CZ | CZSWAP | II | ISWAP | ISWAP_DAG | SQRT_XX | SQRT_XX_DAG | SQRT_YY | SQRT_YY_DAG | SQRT_ZZ | SQRT_ZZ_DAG | SWAP | SWAPCX | SWAPCZ | XCX | XCY | XCZ | YCX | YCY | YCZ | ZCX | ZCY | ZCZ
  -- Collapsing Gates 
  | R | RX | RY | RZ

data Gate = Gate GateTy [Q] 

data MeasureTy = 
  -- Collapsing Gates 
  M | MR | MRX | MRY | MRZ | MX | MY | MZ 
  -- Pair Measurement Gates 
  | MXX | MYY | MZZ

-- Collapsing Gates examples:
--    M 5
--    MZ !5
--       only single ph
--    MZ(0.02) 2 3 5
-- Pair Measurement Gates examples:
--    MXX 1 2
--    MXX !1 2
--    MXX(0.01) 2 3

data Measure = Measure MeasureTy (Maybe Ph) [Q]


-- Generalized Pauli Product Gates 
data GppTy = 
  MPP | SPP | SPP_DAG
  
-- Generalized Pauli Product Gates  examples:
--    MPP X2*X3*X5*X7
--    MPP !Z5
--    MPP X1*Y2 !Z3*Z4*Z5
--    MPP(0.001) Z1*Z2 X1*X2
--    SPP !X1*Y2*Z3

data Gpp = Gpp GppTy (Maybe Ph) [PauliChain]

-- Noise Channels 
data NoiseTy = 
  CORRELATED_ERROR | DEPOLARIZE1 | DEPOLARIZE2 | E | ELSE_CORRELATED_ERROR | HERALDED_ERASE | HERALDED_PAULI_CHANNEL_1 | II_ERROR | I_ERROR | PAULI_CHANNEL_1 | PAULI_CHANNEL_2 | X_ERROR | Y_ERROR | Z_ERROR

-- Noise Channels examples:
-- normal type:
--    DEPOLARIZE1(0.01) 2 3 5
--    DEPOLARIZE2(0.01) 2 3 5 7
--    HERALDED_ERASE(0.01) 2 3 5 7
--    HERALDED_PAULI_CHANNEL_1(0.01, 0.02, 0.03, 0.04) 0 1
--    PAULI_CHANNEL_1(0.1, 0.15, 0.2) 1 2 4
--    PAULI_CHANNEL_2(0,0,0, 0.1,0,0,0, 0,0,0,0.2, 0,0,0,0) 1 2 5 6 8 3
--    X_ERROR(0.001) 5 42
--    Z_ERROR(0.001) 5 42
--    Y_ERROR(0.001) 5 42
--  E type:
--    CORRELATED_ERROR(0.2) X1 Y2
--    ELSE_CORRELATED_ERROR(0.25) Z2 Z3
-- xx_ERROR type
--    II_ERROR 0 1
--    II_ERROR(0.1) 0 1
--    II_ERROR[TWO_QUBIT_LEAKAGE_NOISE_FOR_AN_ADVANCED_SIMULATOR:0.1] 0 2 4 6
--    II_ERROR[MULTIPLE_TWO_QUBIT_NOISE_MECHANISMS](0.1, 0.2) 0 2 4 6
--    I_ERROR 0
--    I_ERROR(0.1) 0
--    I_ERROR[LEAKAGE_NOISE_FOR_AN_ADVANCED_SIMULATOR:0.1] 0 2 4
--    I_ERROR[MULTIPLE_NOISE_MECHANISMS](0.1, 0.2) 0 2 4

data ErrorTag = TWO_QUBIT_LEAKAGE_NOISE_FOR_AN_ADVANCED_SIMULATOR
  | LEAKAGE_NOISE_FOR_AN_ADVANCED_SIMULATOR
  | MULTIPLE_TWO_QUBIT_NOISE_MECHANISMS
  | MULTIPLE_NOISE_MECHANISMS
  | ErrorTagCoef ErrorTag Float 

data Noise = 
  NoiseNormal NoiseTy [Ph] [Int]
  | NoiseE NoiseTy Float [PauliInd]
  | NoiseError NoiseTy (Maybe ErrorTag) [Ph] [Int]

-- Annotations 
data AnnTy = 
  DETECTOR | MPAD | OBSERVABLE_INCLUDE | QUBIT_COORDS | SHIFT_COORDS | TICK

-- Annotations examples:
--    DETECTOR(1, 0) rec[-3] rec[-6]
--    DETECTOR(2, 0, 0) rec[-8]
--    DETECTOR rec[-3] rec[-4] rec[-7]
--    OBSERVABLE_INCLUDE(0) rec[-1] rec[-2]
--    QUBIT_COORDS(0, 0) 0
--    MPAD 0 0 1 0 1
--    SHIFT_COORDS(500.5)
--    SHIFT_COORDS(0, 1)  # Advance 2nd coordinate to track loop iterations.
--    TICK

data FInd = I Ind | F Float
data Ann = Ann [FInd] [Q] | Tick


data Stim = 
  StimG Gate 
  | StimM Measure
  | StimGpp Gpp
  | StimNoise Noise
  | StimAnn Ann
  | StimList [Stim]
  | StimRepeat Int Circuit