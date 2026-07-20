{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use <&>" #-}
module StimParser.ParseUtils where
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
-- import System.IO
import Control.Applicative
import GHC.Stack (HasCallStack)
import Control.Monad.State.Lazy

import qualified Data.HashMap.Strict as HS
import qualified Data.Set as Set
import Data.Maybe
import Data.Char (toLower)

import StimParser.Expr (Tag (..))

type Parser = Parsec Void String
-- type Parser = ParsecT Void String (State Env)

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "#")
  (L.skipBlockComment "(*" "*)")

-- cosume all the white spaces following a Parser
-- white spaces include: " " "\n" "\t"
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

lstring :: String -> Parser String
lstring = lexeme . string

-- | Case-insensitive string parser (custom implementation)
stringCI :: String -> Parser String
stringCI s = try $ mapM charCI s
  where
    charCI c = do
      x <- satisfy (\x -> toLower x == toLower c)
      return x  -- Return the actual matched character, not the expected one

-- | Case-insensitive lexeme string parser  
lstringCI :: String -> Parser String
lstringCI = lexeme . stringCI

safeManyTill :: MonadParsec e s f => f a -> f b -> f [a]
safeManyTill p end = go
  where
    go = try ([] <$ end) <|> liftA2 (:) p go

parseExhaust :: Parser a -> Parser [a]
parseExhaust parseElm = safeManyTill parseElm (notFollowedBy parseElm)

manyBetween :: Parser a -> Parser a -> Parser String
manyBetween s e = s *> safeManyTill L.charLiteral e

run :: HasCallStack => Parser a -> String -> a
run p s = case m of
  Left bundle -> error (errorBundlePretty bundle)
  Right r -> r
  where
   m = runParser p "" s

parseInt :: Parser Int
parseInt = lexeme $ L.signed sc L.decimal

parseFloat :: Parser Double
parseFloat = lexeme $ L.signed sc L.float

-- | Parse a number that may be an integer or a float.
-- This is useful for coordinates and other contexts where integers
-- like @0@ or @1@ should be accepted as @0.0@ or @1.0@.
parseNumber :: Parser Double
parseNumber = lexeme $ L.signed sc (try L.float <|> fromIntegral <$> L.decimal)

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

parseTupleFloat :: Parser [Double]
parseTupleFloat = parseTuple parseFloat

-- | Parse a tuple of numbers (integers or floats).
-- Used for DEM coordinates where bare integers like @(0, 0)@ are common.
parseTupleNumber :: Parser [Double]
parseTupleNumber = parseTuple parseNumber

parseVar :: Parser String
parseVar = do
  lexeme $ safeManyTill L.charLiteral (lookAhead (try space1 <|> eof))

-- | Parse an optional tag: [tag_content]
-- Tag content can be any character except ], \r, \n
tagContentForbidden :: Char -> Bool
tagContentForbidden c = c == ']' || c == '\r' || c == '\n'

parseTag :: Parser Tag
parseTag = do
  lstring "["
  content <- Text.Megaparsec.many $ satisfy (not . tagContentForbidden)
  lstring "]"
  return $ Tag content

excludePredict :: Parser a -> Parser ()
excludePredict p = lookAhead $ notFollowedBy p

