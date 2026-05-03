{-# LANGUAGE InstanceSigs #-}
module StimParser.DEM.Expr where

-- | A detector index (check node in the bipartite graph).
newtype DetectorId = DetectorId { unDetectorId :: Int }
  deriving (Show, Eq, Ord)

-- | A logical observable index.
newtype ObservableId = ObservableId { unObservableId :: Int }
  deriving (Show, Eq, Ord)

-- | Targets of an error instruction: detectors and/or observables.
data DEMTarget
    = TargetDetector !DetectorId
    | TargetObservable !ObservableId
    deriving (Show, Eq)

-- | A single error instruction: error(p) D0 D1 L0
--
-- Probability is stored as raw Double (not LLR). Downstream packages
-- compute LLRs if needed.
data DEMError = DEMError
    { deProbability :: !Double
    , deTargets     :: ![DEMTarget]
    } deriving (Show, Eq)

-- | A detector declaration: detector(0, 0) D0
--
-- Coordinates are stored as [Double] to support arbitrary dimensionality.
data DEMDetector = DEMDetector
    { ddId     :: !DetectorId
    , ddCoords :: ![Double]
    } deriving (Show, Eq)

-- | A logical observable declaration: logical_observable L0
data DEMObservable = DEMObservable
    { doId :: !ObservableId
    } deriving (Show, Eq)

-- | Shift detectors instruction: shift_detectors(1, 0) 0
--
-- First argument is coordinate shift, second is detector ID shift.
data DEMShift = DEMShift
    { dsCoordShift :: ![Double]
    , dsDetShift   :: !Int
    } deriving (Show, Eq)

-- | A DEM instruction (top-level line).
data DEMInstruction
    = DEMInstrError      !DEMError
    | DEMInstrDetector   !DEMDetector
    | DEMInstrObservable !DEMObservable
    | DEMInstrShift      !DEMShift
    | DEMInstrRepeat     !Int ![DEMInstruction]
    deriving (Show, Eq)

-- | A complete Detector Error Model.
newtype DEM = DEM { unDEM :: [DEMInstruction] }
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Flattening utilities
-- ---------------------------------------------------------------------------

-- | Flatten a DEM by expanding all repeat blocks and applying shift_detectors.
--
-- After flattening, the resulting DEM contains only:
--   - 'DEMInstrError'
--   - 'DEMInstrDetector'
--   - 'DEMInstrObservable'
--
-- All 'DEMInstrRepeat' are expanded into their body repeated N times.
-- All 'DEMInstrShift' are applied to subsequent instructions' coordinates
-- and detector IDs.
--
-- This mirrors 'StimParser.Trans.flattenQ' for circuits.
flattenDEM :: DEM -> DEM
flattenDEM (DEM instrs) = DEM (flattenInstrs [] 0 instrs)

-- | Flatten a list of instructions with accumulated coordinate shift
-- and detector ID shift.
flattenInstrs :: [Double] -> Int -> [DEMInstruction] -> [DEMInstruction]
flattenInstrs _ _ [] = []
flattenInstrs coordShift detShift (i : is) =
  case i of
    DEMInstrShift (DEMShift cs ds) ->
      let newCoordShift = zipWith (+) (padCoords (length cs) coordShift) cs
          newDetShift = detShift + ds
      in flattenInstrs newCoordShift newDetShift is

    DEMInstrRepeat n body ->
      flattenInstrs coordShift detShift (concat (replicate n body) ++ is)

    DEMInstrDetector (DEMDetector (DetectorId did) coords) ->
      let shiftPadded = padCoords (length coords) coordShift
          shiftedCoords = zipWith (+) shiftPadded coords
          shiftedId = DetectorId (did + detShift)
      in DEMInstrDetector (DEMDetector shiftedId shiftedCoords) : flattenInstrs coordShift detShift is

    DEMInstrError err ->
      DEMInstrError err : flattenInstrs coordShift detShift is

    DEMInstrObservable obs ->
      DEMInstrObservable obs : flattenInstrs coordShift detShift is

-- | Pad a coordinate list to a given length with zeros.
-- This handles cases where shift_detectors has fewer coordinates than
-- the detectors being shifted.
padCoords :: Int -> [Double] -> [Double]
padCoords n cs = take n (cs ++ repeat 0)
