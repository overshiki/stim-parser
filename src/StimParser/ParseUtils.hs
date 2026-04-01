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

parseFloat :: Parser Float 
parseFloat = lexeme $ L.signed sc L.float

parseVar :: Parser String
parseVar = do
  lexeme $ safeManyTill L.charLiteral (lookAhead (try space1 <|> eof))

excludePredict :: Parser a -> Parser ()
excludePredict p = lookAhead $ notFollowedBy p

