{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module StimParser.Parse where
import StimParser.Expr
import StimParser.ParseUtils
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
      if i < 0 
        then return $ QRec (Rec i)
        else fail "rec[] index must be negative (e.g., rec[-1])"
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

-- | Case-insensitive version of parseShow
parseShowCI :: (Show a) => a -> Parser a
parseShowCI x = lstringCI (show x) >> return x

-- parseEnum :: (Show a) => [a] -> Parser a
-- parseEnum (x:xs) = do
--   let
--     parseTail = msum $ try . parseShow <$> xs
--   parseTail <|> parseShow x
-- parseEnum [] = error "value error"

parseEnum :: (Show a) => [a] -> Parser a
parseEnum [] = error "value error"
parseEnum xs = msum $ map parseShowCI $ sortOn (negate . length . show) xs

-- | Parse an optional tag: [tag_content]
-- Tag content can be any character except ], \r, \n
parseTag :: Parser Tag
parseTag = do
  lstring "["
  content <- many $ satisfy (\c -> c /= ']' && c /= '\r' && c /= '\n')
  lstring "]"
  return $ Tag content

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
  tag <- optional parseTag
  qs <- parseExhaust parseQ
  return $ Gate gty tag qs

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
    -- parser for case: M[tag](0.02) 2 3 5
    pm1 = do 
      mty <- parseMeasureTy
      tag <- optional parseTag
      ph <- parsePh
      qs <- parseExhaust parseQ
      return $ Measure mty tag (Just ph) qs
    -- parser for case: M[tag] 0
    pm2 = do 
      mty <- parseMeasureTy
      tag <- optional parseTag
      qs <- parseExhaust parseQ
      return $ Measure mty tag Nothing qs
  -- the order is tricky - try phase version first
  try pm1 <|> pm2


parseGppTy :: Parser GppTy
parseGppTy = parseEnum gppTyList

parseGpp :: Parser Gpp 
parseGpp = do 
  let 
    -- parser for case: MPP[tag](0.001) Z1*Z2 X1*X2
    pm1 = do 
      gty <- parseGppTy
      tag <- optional parseTag
      ph <- parsePh
      pcs <- parseExhaust parsePauliChain
      return $ Gpp gty tag (Just ph) pcs
    -- parser for case: MPP[tag] X1*Y2 !Z3*Z4*Z5
    pm2 = do 
      gty <- parseGppTy
      tag <- optional parseTag
      pcs <- parseExhaust parsePauliChain
      return $ Gpp gty tag Nothing pcs
  -- the order is tricky - try phase version first
  try pm1 <|> pm2

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

parseTuple :: Parser a -> Parser [a]
parseTuple pm = do 
  lstring "("
  let 
    parseE = do 
      f <- pm
      lstring ","
      return f
    parseTuple_ = (++) <$> parseExhaust parseE <*> ((: []) <$> pm)
  phs <- parseTuple_
  lstring ")"
  return phs

parseTupleFloat :: Parser [Float]
parseTupleFloat = parseTuple parseFloat
  -- lstring "("
  -- let 
  --   parseE = do 
  --     f <- parseFloat
  --     lstring ","
  --     return f
  --   parseTuple = (++) <$> parseExhaust parseE <*> ((: []) <$> parseFloat)
  -- phs <- parseTuple
  -- lstring ")"
  -- return phs


parseNoise :: Parser Noise
parseNoise = do 
  let 
    -- parser for case: X_ERROR[tag](0.01) 0 - with general tag and args
    pm1 = do 
      ty <- parseNoiseTy
      tag <- optional parseTag
      phs <- parseTupleFloat
      qs <- parseExhaust parseQ 
      return $ NoiseNormal ty tag Nothing phs qs 
    -- parser for case: CORRELATED_ERROR[tag](0.2) X1 Y2
    pm2 = do 
      ty <- parseNoiseTy
      tag <- optional parseTag
      lstring "("
      ph <- parseFloat
      lstring ")"
      ps <- some (lexeme parsePauliInd)
      return $ NoiseE ty tag ph ps 
    -- parser for case: X_ERROR[tag] 0 - with general tag, no args
    pm3 = do 
      ty <- parseNoiseTy
      tag <- optional parseTag
      qs <- parseExhaust parseQ 
      return $ NoiseNormal ty tag Nothing [] qs
    -- parser for case: II_ERROR[ErrorTag:coef] 0 2 4 6 - error tag without args
    pm4 = do 
      ty <- parseNoiseTy
      lstring "["
      et <- parseErrorTag
      lstring "]"
      tag <- optional parseTag  -- Optional general tag after error tag
      qs <- parseExhaust parseQ 
      return $ NoiseNormal ty tag (Just et) [] qs
    -- parser for case: II_ERROR[ErrorTag](0.1, 0.2) 0 2 4 6 - error tag with args
    pm5 = do 
      ty <- parseNoiseTy
      lstring "["
      et <- parseErrorTag
      lstring "]"
      tag <- optional parseTag  -- Optional general tag after error tag
      phs <- parseTupleFloat
      qs <- parseExhaust parseQ 
      return $ NoiseNormal ty tag (Just et) phs qs
  -- the order is tricky - try most specific first
  try pm2 <|> try pm5 <|> try pm4 <|> try pm1 <|> pm3

parseAnnTy :: Parser AnnTy
parseAnnTy = parseEnum annTyList

parseFInd :: Parser FInd
parseFInd = do 
  let 
    -- In Ind
    pm1 = do 
      i <- parseInt
      return $ In i 
    -- Fl Float
    pm2 = do
      f <- parseFloat
      return $ Fl f 
  -- the order is tricky 
  try pm2 <|> pm1

parseAnn :: Parser Ann 
parseAnn = do 
  let 
    -- parser for case: DETECTOR[tag](1, 0) rec[-3] rec[-6]
    pm1 = do 
      ty <- parseAnnTy 
      tag <- optional parseTag
      fds <- parseTuple parseFInd
      qs <- parseExhaust parseQ
      return $ Ann ty tag fds qs
    -- parser for case: DETECTOR[tag] rec[-3] rec[-4] rec[-7]
    pm2 = do 
      ty <- parseAnnTy
      tag <- optional parseTag
      qs <- parseExhaust parseQ
      return $ Ann ty tag [] qs
    -- parser for case: SHIFT_COORDS[tag](500.5)
    pm3 = do 
      ty <- parseAnnTy
      tag <- optional parseTag
      fds <- parseTuple parseFInd
      return $ Ann ty tag fds []
    -- parser for case: TICK[tag]
    pm4 = do
      ty <- parseShowCI TICK
      tag <- optional parseTag
      return $ Ann ty tag [] []
  -- the order is tricky
  try pm1 <|> try pm2 <|> try pm3 <|> pm4 

parseStart :: Parser ()
parseStart = void (lstring "!!!Start") 

parseStim :: Parser Stim 
parseStim = do 
  let 
    -- the order may be tricky
    parseUnit = 
      try (StimG <$> parseGate)
      <|> try (StimM <$> parseMeasure)
      <|> try (StimGpp <$> parseGpp)
      <|> try (StimNoise <$> parseNoise)
      <|> (StimAnn <$> parseAnn)

    parseUnitList = StimList <$> parseExhaust parseUnit
    parseRepeat = do 
      lstring "REPEAT"
      i <- parseInt
      lstring "{"
      us <- parseUnitList
      lstring "}"
      return $ StimRepeat i us

    parseUR = try parseRepeat <|> parseUnit

  parseStart
  
  StimList <$> safeManyTill parseUR eof


