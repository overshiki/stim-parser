{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module StimParser.DEM.Parse where

import Text.Megaparsec
import Text.Megaparsec.Char ()

import StimParser.ParseUtils
import StimParser.Expr (Tag)
import StimParser.DEM.Expr

-- | Parse a complete DEM string.
parseDEM :: Parser DEM
parseDEM = DEM <$> many parseDEMInstruction <* eof

-- | Parse a single DEM instruction.
parseDEMInstruction :: Parser DEMInstruction
parseDEMInstruction =
      DEMInstrError      <$> parseDEMError
  <|> DEMInstrDetector   <$> parseDEMDetector
  <|> DEMInstrObservable <$> parseDEMObservable
  <|> DEMInstrShift      <$> parseDEMShift
  <|> uncurry DEMInstrRepeat <$> parseDEMRepeat

-- | Parse an error instruction: error(0.01) D0 D1 L0
parseDEMError :: Parser DEMError
parseDEMError = do
  lstring "error"
  lstring "("
  p <- parseNumber
  lstring ")"
  targets <- many parseDEMTarget
  return $ DEMError p targets

-- | Parse a single target: D0 or L0
parseDEMTarget :: Parser DEMTarget
parseDEMTarget =
      TargetDetector   . DetectorId   <$> (lstring "D" *> parseInt)
  <|> TargetObservable . ObservableId <$> (lstring "L" *> parseInt)

-- | Parse a detector declaration: detector(0, 0) D0
parseDEMDetector :: Parser DEMDetector
parseDEMDetector = do
  lstring "detector"
  tag <- optional parseTag :: Parser (Maybe Tag)
  coords <- parseTupleNumber
  did <- DetectorId <$> (lstring "D" *> parseInt)
  return $ DEMDetector did coords tag

-- | Parse a logical observable declaration: logical_observable L0
parseDEMObservable :: Parser DEMObservable
parseDEMObservable = do
  lstring "logical_observable"
  tag <- optional parseTag :: Parser (Maybe Tag)
  oid <- ObservableId <$> (lstring "L" *> parseInt)
  return $ DEMObservable oid tag

-- | Parse a shift_detectors instruction: shift_detectors(1, 0) 0 or shift_detectors 96
--
-- Coordinates are optional. When absent, only the detector ID is shifted.
parseDEMShift :: Parser DEMShift
parseDEMShift = do
  lstring "shift_detectors"
  coords <- option [] parseTupleNumber
  shift <- parseInt
  return $ DEMShift coords shift

-- | Parse a repeat block: repeat 100 { ... }
parseDEMRepeat :: Parser (Int, [DEMInstruction])
parseDEMRepeat = do
  lstring "repeat"
  n <- parseInt
  lstring "{"
  instrs <- many parseDEMInstruction
  lstring "}"
  return (n, instrs)
