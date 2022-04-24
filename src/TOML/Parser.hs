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
import Data.Foldable (foldlM)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char hiding (newline)
import qualified Text.Megaparsec.Char.Lexer as L

import TOML.Internal (TOMLError (..), Table, Value (..))

parseTOML :: String -> Text -> Either TOMLError Value
parseTOML filename input =
  case runParser parseTOMLDocument filename input of
    Left e -> Left $ ParseError e
    Right result -> Table <$> normalize result

{--- Parse raw document ---}

type Parser = Parsec Void Text

type Key = [Text]
type RawTable = Map Key Value

data TOMLDoc = TOMLDoc
  { rootTable :: RawTable
  , subTables :: [TableSection]
  }

data TableSection = TableSection
  { tableSectionHeader :: TableSectionHeader
  , tableSectionTable :: RawTable
  }

data TableSectionHeader = SectionTable Key | SectionTableArray Key

parseTOMLDocument :: Parser TOMLDoc
parseTOMLDocument = do
  emptyLines
  rootTable <- parseRawTable
  emptyLines
  subTables <- many parseTableSection
  emptyLines
  eof
  return TOMLDoc{..}

parseRawTable :: Parser RawTable
parseRawTable =
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
  tableSectionTable <- parseRawTable
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
  (`sepBy` hsymbol ".") . choice $
    [ parseBasicString
    , parseLiteralString
    , parseUnquotedKey
    ]
  where
    parseUnquotedKey =
      takeWhile1P
        (Just "[A-Za-z0-9_-]")
        (`elem` ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ "-_")

parseValue :: Parser Value
parseValue =
  choice
    [ try $ Table <$> parseInlineTable
    , try $ Array <$> parseInlineArray
    , try $ String <$> parseString
    , try $ Integer <$> parseInteger
    , try $ Float <$> parseFloat
    , try $ Boolean <$> parseBoolean
    , try $ OffsetDateTime <$> parseOffsetDateTime
    , try $ LocalDateTime <$> parseLocalDateTime
    , try $ LocalDate <$> parseLocalDate
    , LocalTime <$> parseLocalTime
    ]
  where
    parseInlineTable = undefined
    parseInlineArray = undefined
    parseString =
      choice
        -- TODO: multiline basic+literal strings
        [ try parseBasicString
        , parseLiteralString
        ]
    parseInteger = undefined
    parseFloat = undefined
    parseBoolean = undefined
    parseOffsetDateTime = undefined
    parseLocalDateTime = undefined
    parseLocalDate = undefined
    parseLocalTime = undefined

-- | A string in double quotes.
parseBasicString :: Parser Text
parseBasicString = between (hsymbol "\"") (hsymbol "\"") $ takeWhileP (Just "basic-char") isBasicChar
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

-- | A string in single quotes.
parseLiteralString :: Parser Text
parseLiteralString = between (hsymbol "'") (hsymbol "'") $ takeWhileP (Just "literal-char") isLiteralChar
  where
    isLiteralChar c =
      let code = ord c
       in case c of
            ' ' -> True
            '\t' -> True
            _ | 0x21 <= code && code <= 0x7E -> c /= '\''
            _ | isNonAscii c -> True
            _ -> False

-- | https://github.com/toml-lang/toml/blob/1.0.0/toml.abnf#L38
isNonAscii :: Char -> Bool
isNonAscii c =
  let code = ord c
   in (0x80 <= code && code <= 0xD7FF) || (0xE000 <= code && code <= 0x10FFFF)

{--- Normalize into Value ---}

normalize :: TOMLDoc -> Either TOMLError Table
normalize TOMLDoc{..} = do
  root <- flattenTable rootTable
  foldlM (flip mergeTableSection) root subTables
  where
    flattenTable :: RawTable -> Either TOMLError Table
    flattenTable = undefined

    mergeTableSection :: TableSection -> Table -> Either TOMLError Table
    mergeTableSection TableSection{..} baseTable = do
      subTable <- flattenTable tableSectionTable
      case tableSectionHeader of
        SectionTable key -> mergeTableSectionTable key subTable baseTable
        SectionTableArray key -> mergeTableSectionArray key subTable baseTable

    mergeTableSectionTable :: Key -> Table -> Table -> Either TOMLError Table
    mergeTableSectionTable = undefined

    mergeTableSectionArray :: Key -> Table -> Table -> Either TOMLError Table
    mergeTableSectionArray = undefined

{--- Helpers ---}

hsymbol :: Text -> Parser ()
hsymbol s = L.symbol hspace s >> pure ()

-- | Parse trailing whitespace + newline
newline :: Parser ()
newline = hspace >> eol >> pure ()

-- | Parse spaces, newlines, and comments
emptyLines :: Parser ()
emptyLines = L.space space1 (L.skipLineComment "#") empty
