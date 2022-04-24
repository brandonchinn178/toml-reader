{-# LANGUAGE LambdaCase #-}
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

import Control.Monad.Combinators.NonEmpty (sepBy1)
import Data.Char (ord)
import Data.Foldable (foldlM)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Megaparsec hiding (sepBy1)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import TOML.Internal (TOMLError (..), Table, Value (..))

parseTOML :: String -> Text -> Either TOMLError Value
parseTOML filename input =
  case runParser parseTOMLDocument filename input of
    Left e -> Left $ ParseError $ Text.pack $ errorBundlePretty e
    Right result -> Table <$> normalize result

{--- Parse raw document ---}

type Parser = Parsec Void Text

type Key = NonEmpty Text
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
    endOfLine
    emptyLines
    return (key, value)

parseTableSection :: Parser TableSection
parseTableSection = do
  tableSectionHeader <-
    choice
      [ SectionTableArray <$> parseHeader "[[" "]]"
      , SectionTable <$> parseHeader "[" "]"
      ]
  endOfLine
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
  (`sepBy1` hsymbol ".") . choice $
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
    parseInlineTable = empty -- TODO
    parseInlineArray = empty -- TODO
    parseString =
      choice
        -- TODO: multiline basic+literal strings
        [ try parseBasicString
        , parseLiteralString
        ]
    parseInteger = empty -- TODO
    parseFloat = empty -- TODO
    parseBoolean = empty -- TODO
    parseOffsetDateTime = empty -- TODO
    parseLocalDateTime = empty -- TODO
    parseLocalDate = empty -- TODO
    parseLocalTime = empty -- TODO

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
    flattenTable = foldlM (\t (k, v) -> insertAt k v t) Map.empty . Map.toList

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

{- |
Insert Value into Table at Key.

e.g. `insertAt ["a", "b", "c"] v Map.empty` results in the JSON equivalent
of `{ "a": { "b": { "c": v } } }`.
-}
insertAt :: Key -> Value -> Table -> Either TOMLError Table
insertAt allKeys val = go [] allKeys
  where
    go history (k NonEmpty.:| ks) =
      let insert =
            case NonEmpty.nonEmpty ks of
              Nothing -> \case
                Nothing -> pure val
                Just v -> insertFail history k v
              Just ks' -> \curr -> do
                subTable <-
                  case curr of
                    Nothing -> pure Map.empty
                    Just (Table subTable) -> pure subTable
                    Just v -> insertFail history k v
                Table <$> go (k : history) ks' subTable
       in Map.alterF (fmap Just . insert) k

    insertFail history key currVal =
      normalizeErr . Text.unlines $
        [ "Could not add value to path \"" <> Text.intercalate "." (key : history) <> "\":"
        , "  Setting: " <> Text.intercalate "." (NonEmpty.toList allKeys) <> " = " <> Text.pack (show val)
        , "  Existing value: " <> Text.pack (show currVal)
        ]

normalizeErr :: Text -> Either TOMLError a
normalizeErr = Left . NormalizeError

{--- Helpers ---}

hsymbol :: Text -> Parser ()
hsymbol s = L.symbol hspace s >> pure ()

-- | Parse trailing whitespace/trailing comments + newline
endOfLine :: Parser ()
endOfLine = L.space hspace1 skipComments empty >> eol >> pure ()

-- | Parse spaces, newlines, and comments
emptyLines :: Parser ()
emptyLines = L.space space1 skipComments empty

skipComments :: Parser ()
skipComments = L.skipLineComment "#"
