{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TOML.Parser (
  parseTOML,
) where

import qualified Data.Map as Map
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char hiding (space)
import qualified Text.Megaparsec.Char.Lexer as L

import TOML.Internal (TOMLError (..), Table, Value (..))

parseTOML :: String -> Text -> Either TOMLError Value
parseTOML filename input =
  case runParser parseTOMLDocument filename input of
    Left e -> Left $ TOMLError e
    Right result -> normalize result
  where
    normalize = undefined

type Parser = Parsec Void Text

data TOMLDoc = TOMLDoc
  { rootTable :: Table
  , subTables :: [TableSection]
  }

data TableSection
  = TableSection Text Table
  | TableArraySection Text Table

parseTOMLDocument :: Parser TOMLDoc
parseTOMLDocument = do
  rootTable <- parseTable
  subTables <- many parseTableSection
  eof
  return TOMLDoc{..}

parseTable :: Parser Table
parseTable =
  fmap Map.fromList . many $ do
    space
    key <- parseKey
    hspace
    _ <- string "="
    hspace
    value <- parseValue
    space
    return (key, value)

parseTableSection :: Parser TableSection
parseTableSection = undefined

parseKey :: Parser Text
parseKey = undefined

parseValue :: Parser Value
parseValue = undefined

{--- Helpers ---}

space :: Parser ()
space = L.space space1 (L.skipLineComment "#") empty
