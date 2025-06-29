{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Parse where
import Expr
import ParseUtils
import Control.Monad
import Data.List
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- parsePauli is not lexemed
parsePauli :: Parser Pauli
parsePauli = 
  try (string "X" >> return PX) 
  <|> try (string "Y" >> return PY)
  <|> (string "Z" >> return PZ)

-- parsePauliInd is not lexemed
parsePauliInd :: Parser PauliInd
parsePauliInd = do 
  p <- parsePauli
  i <- L.decimal
  return $ PauliInd p i

parseExhaust :: Parser a -> Parser [a]
parseExhaust parseElm = safeManyTill parseElm (notFollowedBy parseElm)

parsePauliChain :: Parser PauliChain
parsePauliChain = do 
  let
    parseElm = do 
      pp <- parsePauliInd
      string "*"
      return pp 

    parsePCs_ = do 
      pcs <- parseExhaust parseElm
      pp <- parsePauliInd
      return $ pcs ++ [pp]

    -- parser for case: X2*X3*X5*X7
    parsePCs = P <$> parsePCs_
    -- parser for case: !Z3*Z4*Z5
    parseNPCs = do 
      string "!"
      N <$> parsePCs_
  lexeme $ try parseNPCs <|> parsePCs
      

parseQ :: Parser Q
parseQ = do
  let
    qint = Q <$> parseInt
    qrec = do
      lstring "rec"
      lstring "["
      i <- parseInt
      lstring "]"
      return $ QRec (Rec i)
    qsweep = do
      lstring "sweep"
      lstring "["
      i <- parseInt
      lstring "]"
      return $ QSweep (Sweep i)
    qnot = do
      lstring "!"
      Not <$> parseInt
  try qint <|> try qrec <|> try qsweep <|> qnot


parseShow :: (Show a) => a -> Parser a
parseShow x = lstring (show x) >> return x

parseEnum :: (Show a) => [a] -> Parser a
parseEnum (x:xs) = do
  let
    parseTail = msum $ try . parseShow <$> xs
  parseTail <|> parseShow x
parseEnum [] = error "value error"

parseGateTy :: Parser GateTy
parseGateTy = parseEnum gateTyList

-- -- always remember to update this, when new syntax element is supported
-- preparseNext :: Parser ()
-- preparseNext = 
--   try (void parseGateTy)
--   <|> try (void parseMeasureTy)
--   <|> void parseGppTy

parseGate :: Parser Gate
parseGate = do
  gty <- parseGateTy
  -- qs <- safeManyTill parseQ preparseNext
  qs <- parseExhaust parseQ
  return $ Gate gty qs

parseMeasureTy :: Parser MeasureTy
parseMeasureTy = parseEnum measureTyList

parsePh :: Parser Float 
parsePh = do 
  lstring "("
  ph <- parseFloat
  lstring ")"
  return ph

parseMeasure :: Parser Measure
parseMeasure = do 
  let 
    -- parser for case: MXX !1 2
    pm1 = do 
      mty <- parseMeasureTy
      -- qs <- safeManyTill parseQ preparseNext
      qs <- parseExhaust parseQ
      return $ Measure mty Nothing qs
    -- parser for case: MZ(0.02) 2 3 5
    pm2 = do 
      mty <- parseMeasureTy
      ph <- parsePh
      -- qs <- safeManyTill parseQ preparseNext
      qs <- parseExhaust parseQ
      return $ Measure mty (Just ph) qs
  -- the order is tricky
  try pm2 <|> pm1


parseGppTy :: Parser GppTy
parseGppTy = parseEnum gppTyList

parseGpp :: Parser Gpp 
parseGpp = do 
  let 
    -- parser for case: MPP X1*Y2 !Z3*Z4*Z5
    pm1 = do 
      gty <- parseGppTy
      -- pcs <- safeManyTill parsePauliChain preparseNext
      pcs <- parseExhaust parsePauliChain
      return $ Gpp gty Nothing pcs
    -- parser for case: MPP(0.001) Z1*Z2 X1*X2
    pm2 = do 
      gty <- parseGppTy
      ph <- parsePh
      -- pcs <- safeManyTill parsePauliChain preparseNext
      pcs <- parseExhaust parsePauliChain
      return $ Gpp gty (Just ph) pcs
  -- the order is tricky
  try pm2 <|> pm1

parseNoiseTy :: Parser NoiseTy
parseNoiseTy = parseEnum noiseTyList

parseErrorTagTy :: Parser ErrorTagTy
parseErrorTagTy = parseEnum errorTagTyList

parseErrorTag :: Parser ErrorTag
parseErrorTag = do 
  let 
    -- parser for case: MULTIPLE_NOISE_MECHANISMS
    pm1 = ErrorTag <$> parseErrorTagTy
    -- parser for case: LEAKAGE_NOISE_FOR_AN_ADVANCED_SIMULATOR:0.1
    pm2 = do 
      ett <- parseErrorTagTy
      lstring ":"
      ph <- parseFloat
      return $ ErrorTagCoef ett ph 
  -- the order is tricky
  try pm2 <|> pm1

parseTupleFloat :: Parser [Float]
parseTupleFloat = do 
  lstring "("
  let 
    parseE = do 
      f <- parseFloat
      lstring ","
      return f
    parseTuple = (++) <$> parseExhaust parseE <*> ((: []) <$> parseFloat)
  phs <- parseTuple
  lstring ")"
  return phs


parseNoise :: Parser Noise
parseNoise = do 
  let 
    -- parser for case: HERALDED_PAULI_CHANNEL_1(0.01, 0.02, 0.03, 0.04) 0 1
    pm1 = do 
      ty <- parseNoiseTy
      -- lstring "("
      -- let 
      --   parseE = parseFloat << (lstring ",")
      --   parseTuple = (++) <$> parseExhaust parseE <*> ([] <$> parseFloat)
      -- phs <- parseTuple
      -- lstring ")"
      phs <- parseTupleFloat
      qs <- parseExhaust parseQ 
      return $ NoiseNormal ty Nothing phs qs 
    -- parser for case: CORRELATED_ERROR(0.2) X1 Y2
    pm2 = do 
      ty <- parseNoiseTy
      lstring "("
      ph <- parseFloat
      lstring ")"
      ps <- parseExhaust parsePauliInd
      return $ NoiseE ty ph ps 
    -- parser for case: II_ERROR 0 1
    pm3 = do 
      ty <- parseNoiseTy
      qs <- parseExhaust parseQ 
      return $ NoiseNormal ty Nothing [] qs
    -- parser for case: II_ERROR[TWO_QUBIT_LEAKAGE_NOISE_FOR_AN_ADVANCED_SIMULATOR:0.1] 0 2 4 6
    pm4 = do 
      ty <- parseNoiseTy
      lstring "["
      et <- parseErrorTag
      lstring "]"
      qs <- parseExhaust parseQ 
      return $ NoiseNormal ty (Just et) [] qs
    -- parser for case: II_ERROR[MULTIPLE_TWO_QUBIT_NOISE_MECHANISMS](0.1, 0.2) 0 2 4 6
    pm5 = do 
      ty <- parseNoiseTy
      lstring "["
      et <- parseErrorTag
      lstring "]"
      phs <- parseTupleFloat
      qs <- parseExhaust parseQ 
      return $ NoiseNormal ty (Just et) phs qs
  -- the order is tricky 
  try pm2 <|> try pm5 <|> try pm4 <|> try pm1 <|> pm3