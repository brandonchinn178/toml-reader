{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Parse a TOML document.

References:
* https://toml.io/en/v1.0.0
* https://github.com/toml-lang/toml/blob/1.0.0/toml.abnf
-}
module TOML.Parser (
  parseTOML,
) where

import Data.Char (ord)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char hiding (newline)
import qualified Text.Megaparsec.Char.Lexer as L

import TOML.Internal (TOMLError (..), Value (..))

parseTOML :: String -> Text -> Either TOMLError Value
parseTOML filename input =
  case runParser parseTOMLDocument filename input of
    Left e -> Left $ TOMLError e
    Right result -> normalize result
  where
    normalize = undefined

type Parser = Parsec Void Text

type Key = [Text]
type Table = Map Key Value

data TOMLDoc = TOMLDoc
  { rootTable :: Table
  , subTables :: [TableSection]
  }

data TableSection = TableSection
  { tableSectionHeader :: TableSectionHeader
  , tableSectionTable :: Table
  }

data TableSectionHeader = SectionTable Key | SectionTableArray Key

parseTOMLDocument :: Parser TOMLDoc
parseTOMLDocument = do
  emptyLines
  rootTable <- parseTable
  emptyLines
  subTables <- many parseTableSection
  emptyLines
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
    emptyLines
    return (key, value)

parseTableSection :: Parser TableSection
parseTableSection = do
  tableSectionHeader <-
    choice
      [ SectionTableArray <$> parseHeader "[[" "]]"
      , SectionTable <$> parseHeader "[" "]"
      ]
  newline
  emptyLines
  tableSectionTable <- parseTable
  emptyLines
  return TableSection{..}
  where
    parseHeader brackStart brackEnd = do
      _ <- string brackStart
      hspace
      key <- parseKey
      hspace
      _ <- string brackEnd
      return key

parseKey :: Parser Key
parseKey =
  fmap (map Text.pack) $
    (`sepBy` hsymbol ".") . choice $
      [ between (hsymbol "\"") (hsymbol "\"") (many $ satisfy isBasicChar)
      , between (hsymbol "'") (hsymbol "'") (many $ satisfy isLiteralChar)
      , unquotedString
      ]
  where
    isBasicChar c =
      let code = ord c
       in case c of
            ' ' -> True
            '\t' -> True
            _ | 0x21 <= code && code <= 0x7E -> c `notElem` ['"', '\\']
            _ | isNonAscii c -> True
            -- TODO: escape sequences
            -- \", \\, \b, \f, \n, \r, \t, \uXXXX, \UXXXXXXXX
            _ -> False
    isLiteralChar c =
      let code = ord c
       in case c of
            ' ' -> True
            '\t' -> True
            _ | 0x21 <= code && code <= 0x7E -> c /= '\''
            _ | isNonAscii c -> True
            _ -> False
    unquotedString =
      some $
        label "[A-Za-z0-9_-]" . oneOf $
          ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ "-_"
    isNonAscii c =
      let code = ord c
       in (0x80 <= code && code <= 0xD7FF) || (0xE000 <= code && code <= 0x10FFFF)

parseValue :: Parser Value
parseValue = undefined

{--- Helpers ---}

hsymbol :: Text -> Parser ()
hsymbol s = L.symbol hspace s >> pure ()

-- | Parse trailing whitespace + newline
newline :: Parser ()
newline = hspace >> eol >> pure ()

-- | Parse spaces, newlines, and comments
emptyLines :: Parser ()
emptyLines = L.space space1 (L.skipLineComment "#") empty
