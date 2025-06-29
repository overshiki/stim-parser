module Parse where 
import Expr 
import ParseUtils
import Control.Monad
import Data.List
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char


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
    parseTail = msum $ (\g -> try (parseShow g)) <$> xs
  parseTail <|> parseShow x
parseEnum [] = error "value error"

parseGateTy :: Parser GateTy
parseGateTy = parseEnum gateTyList 
--  let 
--    parseGateTy_ :: GateTy -> Parser GateTy
--    parseGateTy_ gt = lstring (show gt) >> return gt
--    -- it is safe to do this, since we know gateTyList is not empty
--    g0:gremain = gateTyList
--    parseGateTyTail = msum $ (\g -> try (parseGateTy_ g)) <$> gremain
--  parseGateTyTail <|> parseGateTy_ g0

-- always remember to update this, when new syntax element is supported
preparseNext :: Parser ()
preparseNext = parseGateTy >> return ()

parseGate :: Parser Gate
parseGate = do
  gty <- parseGateTy
  qs <- safeManyTill parseQ preparseNext
  return $ Gate gty qs

parseMeasureTy :: Parser MeasureTy
parseMeasureTy = parseEnum measureTyList
