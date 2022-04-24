{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TOML.Parser (
  parseTOML,
) where

import qualified Data.Map as Map
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char hiding (newline, space)
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

data TableSection = TableSection
  { tableSectionHeader :: TableSectionHeader
  , tableSectionTable :: Table
  }

data TableSectionHeader = SectionTable Text | SectionTableArray Text

parseTOMLDocument :: Parser TOMLDoc
parseTOMLDocument = do
  space
  rootTable <- parseTable
  space
  subTables <- many parseTableSection
  space
  eof
  return TOMLDoc{..}

parseTable :: Parser Table
parseTable =
  fmap Map.fromList . many $ do
    key <- parseKey
    hspace
    _ <- string "="
    hspace
    value <- parseValue
    newline
    space
    return (key, value)

parseTableSection :: Parser TableSection
parseTableSection = do
  tableSectionHeader <-
    choice
      [ SectionTableArray <$> parseHeader "[[" "]]"
      , SectionTable <$> parseHeader "[" "]"
      ]
  newline
  space
  tableSectionTable <- parseTable
  space
  return TableSection{..}
  where
    parseHeader brackStart brackEnd = do
      _ <- string brackStart
      hspace
      key <- parseKey
      hspace
      _ <- string brackEnd
      return key

parseKey :: Parser Text
parseKey = undefined

parseValue :: Parser Value
parseValue = undefined

{--- Helpers ---}

-- | Parse trailing whitespace + newline
newline :: Parser ()
newline = hspace >> eol >> pure ()

space :: Parser ()
space = L.space space1 (L.skipLineComment "#") empty
