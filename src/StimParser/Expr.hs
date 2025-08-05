module StimParser.Expr where 

type Ind = Int
type Ph = Float
newtype Rec = Rec Int
  deriving (Show)
newtype Sweep = Sweep Int
  deriving (Show)

-- example:
--    CY sweep[5] 7 sweep[5] 8
--    CY rec[-1] 6
--    CY 2 5 4 2
--    MXX !1 2
data Q = Q Int | QRec Rec | QSweep Sweep | Not Int
  deriving (Show)

data Pauli = PX | PY | PZ 
  deriving (Show)
data PauliInd = PauliInd Pauli Ind
  deriving (Show)

data PauliChain = P [PauliInd]
  | N [PauliInd]
  deriving (Show)

-- the order is tricky for parser. TODO: more robust imp
-- the first one is actually the last, in the parser order
data GateTy =
  -- Pauli
  I | X | Y | Z 
  -- Single Qubit Clifford Gates  
  | C_NXYZ | C_NZYX | C_XNYZ | C_XYNZ | C_XYZ | C_ZNYX | C_ZYNX | C_ZYX | H | H_NXY | H_NXZ | H_NYZ | H_XY | H_XZ | H_YZ | S | SQRT_X | SQRT_X_DAG | SQRT_Y | SQRT_Y_DAG | SQRT_Z | SQRT_Z_DAG | S_DAG 
  -- Two Qubit Clifford Gates 
  | CNOT | CX | CXSWAP | CY | CZ | CZSWAP | II | ISWAP | ISWAP_DAG | SQRT_XX | SQRT_XX_DAG | SQRT_YY | SQRT_YY_DAG | SQRT_ZZ | SQRT_ZZ_DAG | SWAP | SWAPCX | SWAPCZ | XCX | XCY | XCZ | YCX | YCY | YCZ | ZCX | ZCY | ZCZ
  -- Collapsing Gates 
  | RX | RY | RZ
  | R 
  deriving (Show, Eq, Ord, Bounded, Enum)

-- property of dataclass Enum from GHC.Enum
gateTyList :: [GateTy]
gateTyList = [I ..] 

-- Gate examples:
--    CY sweep[5] 7 sweep[5] 8
--    CY rec[-1] 6
--    CY 2 5 4 2
data Gate = Gate GateTy [Q] 
  deriving (Show)

-- Collapsing Gates and Pair Measurement Gates 
-- the order is tricky for parser. TODO: more robust imp
-- the first one is actually the last, in the parser order
data MeasureTy = 
  M 
  | MXX | MYY | MZZ -- Pair Measurement Gates 
  | MRX | MRY | MRZ 
  | MX | MY | MZ 
  | MR 
  deriving (Show, Eq, Ord, Bounded, Enum)

measureTyList :: [MeasureTy]
measureTyList = [M ..] 

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
  deriving (Show)

-- Generalized Pauli Product Gates 
-- the order is tricky for parser. TODO: more robust imp
-- the first one is actually the last, in the parser order
data GppTy = 
  MPP 
  | SPP_DAG
  | SPP 
  deriving (Show, Eq, Ord, Bounded, Enum)

gppTyList :: [GppTy]
gppTyList = [MPP ..] 

-- Generalized Pauli Product Gates  examples:
--    MPP X2*X3*X5*X7
--    MPP !Z5
--    MPP X1*Y2 !Z3*Z4*Z5
--    MPP(0.001) Z1*Z2 X1*X2
--    SPP !X1*Y2*Z3

data Gpp = Gpp GppTy (Maybe Ph) [PauliChain]
  deriving (Show)

-- Noise Channels 
-- the order is tricky for parser. TODO: more robust imp
-- the first one is actually the last, in the parser order
data NoiseTy = 
  E 
  | CORRELATED_ERROR | DEPOLARIZE1 | DEPOLARIZE2 
  | ELSE_CORRELATED_ERROR | HERALDED_ERASE | HERALDED_PAULI_CHANNEL_1 | II_ERROR | I_ERROR | PAULI_CHANNEL_1 | PAULI_CHANNEL_2 | X_ERROR | Y_ERROR | Z_ERROR
  deriving (Show, Eq, Ord, Bounded, Enum)

noiseTyList :: [NoiseTy]
noiseTyList = [E ..] 

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
--    II_ERROR(0.1) 0 1
--    I_ERROR(0.1) 0
--  E type:
--    CORRELATED_ERROR(0.2) X1 Y2
--    ELSE_CORRELATED_ERROR(0.25) Z2 Z3
-- xx_ERROR type
--    II_ERROR 0 1
--    II_ERROR[TWO_QUBIT_LEAKAGE_NOISE_FOR_AN_ADVANCED_SIMULATOR:0.1] 0 2 4 6
--    II_ERROR[MULTIPLE_TWO_QUBIT_NOISE_MECHANISMS](0.1, 0.2) 0 2 4 6
--    I_ERROR 0
--    I_ERROR[LEAKAGE_NOISE_FOR_AN_ADVANCED_SIMULATOR:0.1] 0 2 4
--    I_ERROR[MULTIPLE_NOISE_MECHANISMS](0.1, 0.2) 0 2 4

data ErrorTagTy = TWO_QUBIT_LEAKAGE_NOISE_FOR_AN_ADVANCED_SIMULATOR
  | LEAKAGE_NOISE_FOR_AN_ADVANCED_SIMULATOR
  | MULTIPLE_TWO_QUBIT_NOISE_MECHANISMS
  | MULTIPLE_NOISE_MECHANISMS
  deriving (Show, Eq, Ord, Bounded, Enum)

errorTagTyList :: [ErrorTagTy]
errorTagTyList = [TWO_QUBIT_LEAKAGE_NOISE_FOR_AN_ADVANCED_SIMULATOR ..] 

data ErrorTag = ErrorTag ErrorTagTy
  | ErrorTagCoef ErrorTagTy Float 
  deriving (Show)

data Noise = 
  NoiseNormal NoiseTy (Maybe ErrorTag) [Ph] [Q]
  | NoiseE NoiseTy Float [PauliInd]
  deriving (Show)

-- Annotations 
data AnnTy = 
  DETECTOR | MPAD | OBSERVABLE_INCLUDE | QUBIT_COORDS | SHIFT_COORDS | TICK
  deriving (Show, Eq, Ord, Bounded, Enum)

annTyList :: [AnnTy]
annTyList = [DETECTOR ..] 

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

-- seperate Integer and Float cases
data FInd = In Ind | Fl Float
  deriving (Show)
data Ann = Ann AnnTy [FInd] [Q]
  deriving (Show)

-- repeat example:
-- REPEAT 0 {
--     M 0
--     OBSERVABLE_INCLUDE(0) rec[-1]
-- }

data Stim = 
  StimG Gate 
  | StimM Measure
  | StimGpp Gpp
  | StimNoise Noise
  | StimAnn Ann
  | StimList [Stim]
  | StimRepeat Int Stim
  deriving (Show)
