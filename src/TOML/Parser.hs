{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

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
import Data.Char (chr, digitToInt, isDigit, ord)
import Data.Foldable (foldl', foldlM)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import qualified Numeric
import Text.Megaparsec hiding (sepBy1)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

#if !MIN_VERSION_megaparsec(9,0,0)
import Control.Monad (void)
import Data.Char (isSpace)
#endif

import TOML.Internal (TOMLError (..), Table, Value (..))

parseTOML :: String -> Text -> Either TOMLError Value
parseTOML filename input =
  case runParser parseTOMLDocument filename input of
    Left e -> Left $ ParseError $ Text.pack $ errorBundlePretty e
    Right result -> Table <$> normalize result

{--- Parse raw document ---}

type Parser = Parsec Void Text

type Key = NonEmpty Text
type RawTable = [(Key, Value)]

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
parseRawTable = many $ do
  key <- parseKey
  hspace
  _ <- char '='
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
    [ try $ Table <$> label "table" parseInlineTable
    , try $ Array <$> label "array" parseInlineArray
    , try $ String <$> label "string" parseString
    , try $ Integer <$> label "integer" parseInteger
    , try $ Float <$> label "float" parseFloat
    , try $ Boolean <$> label "boolean" parseBoolean
    , try $ OffsetDateTime <$> label "offset-datetime" parseOffsetDateTime
    , try $ LocalDateTime <$> label "local-datetime" parseLocalDateTime
    , try $ LocalDate <$> label "local-date" parseLocalDate
    , LocalTime <$> label "local-time" parseLocalTime
    ]
  where
    parseInlineTable = empty -- TODO
    parseInlineArray = empty -- TODO
    parseFloat = empty -- TODO
    parseOffsetDateTime = empty -- TODO
    parseLocalDateTime = empty -- TODO
    parseLocalDate = empty -- TODO
    parseLocalTime = empty -- TODO

parseString :: Parser Text
parseString =
  choice
    [ try parseMultilineBasicString
    , try parseMultilineLiteralString
    , try parseBasicString
    , parseLiteralString
    ]

-- | A string in double quotes.
parseBasicString :: Parser Text
parseBasicString =
  label "double-quoted string" $
    between (hsymbol "\"") (hsymbol "\"") $
      fmap Text.pack . many . choice $
        [ satisfy isBasicChar
        , parseEscaped
        ]

-- | A string in single quotes.
parseLiteralString :: Parser Text
parseLiteralString =
  label "single-quoted string" $
    between (hsymbol "'") (hsymbol "'") $ takeWhileP (Just "literal-char") isLiteralChar
  where
    isLiteralChar c =
      let code = ord c
       in case c of
            ' ' -> True
            '\t' -> True
            _ | 0x21 <= code && code <= 0x7E -> c /= '\''
            _ | isNonAscii c -> True
            _ -> False

-- | A multiline string with three double quotes.
parseMultilineBasicString :: Parser Text
parseMultilineBasicString =
  label "double-quoted multiline string" $ do
    hsymbol delim <* optional newline
    lineContinuation
    Text.pack <$> manyTill (mlBasicChar <* lineContinuation) (hsymbol delim)
  where
    delim = Text.replicate 3 "\""
    mlBasicChar =
      choice
        [ try parseEscaped
        , satisfy isBasicChar
        , newline
        ]
    lineContinuation = many (try $ char '\\' *> hspace *> newline *> space) *> pure ()

-- | A multiline string with three single quotes.
parseMultilineLiteralString :: Parser Text
parseMultilineLiteralString = empty

parseEscaped :: Parser Char
parseEscaped = char '\\' *> parseEscapedChar
  where
    parseEscapedChar =
      choice
        [ char '"'
        , char '\\'
        , char 'b' $> '\b'
        , char 'f' $> '\f'
        , char 'n' $> '\n'
        , char 'r' $> '\r'
        , char 't' $> '\t'
        , char 'u' *> unicodeHex 4
        , char 'U' *> unicodeHex 8
        ]

    unicodeHex n = do
      code <- readHex <$> count n hexDigitChar
      pure $ if code <= maxUnicode then chr code else '�'
    maxUnicode = ord (maxBound :: Char)

isBasicChar :: Char -> Bool
isBasicChar c =
  case c of
    ' ' -> True
    '\t' -> True
    _ | 0x21 <= code && code <= 0x7E -> c /= '"' && c /= '\\'
    _ | isNonAscii c -> True
    _ -> False
  where
    code = ord c

-- | https://github.com/toml-lang/toml/blob/1.0.0/toml.abnf#L38
isNonAscii :: Char -> Bool
isNonAscii c = (0x80 <= code && code <= 0xD7FF) || (0xE000 <= code && code <= 0x10FFFF)
  where
    code = ord c

parseInteger :: Parser Integer
parseInteger =
  choice
    [ try parseBinInt
    , try parseOctInt
    , try parseHexInt
    , parseSignedDecInt
    ]
  where
    parseSignedDecInt = do
      sign <- optional $ satisfy $ \c -> c == '-' || c == '+'
      num <-
        choice
          [ try parseUnsignedDecInt
          , fromIntegral . digitToInt <$> digitChar
          ]
      pure $ if sign == Just '-' then -num else num
    parseUnsignedDecInt =
      parseDigits readDec (satisfy $ \c -> isDigit c && c /= '0') digitChar
    parseHexInt =
      parsePrefixedInt readHex "0x" hexDigitChar
    parseOctInt =
      parsePrefixedInt readOct "0o" octDigitChar
    parseBinInt =
      parsePrefixedInt readBin "0b" binDigitChar

    parseDigits readInt parseLeadingDigit parseDigit = do
      leading <- parseLeadingDigit
      rest <- many $ optional (char '_') *> parseDigit
      pure $ readInt $ leading : rest
    parsePrefixedInt readInt prefix parseDigit =
      string prefix *> parseDigits readInt parseDigit parseDigit

parseBoolean :: Parser Bool
parseBoolean =
  choice
    [ True <$ string "true"
    , False <$ string "false"
    ]

{--- Normalize into Value ---}

normalize :: TOMLDoc -> Either TOMLError Table
normalize TOMLDoc{..} = do
  root <- flattenTable rootTable
  foldlM (flip mergeTableSection) root subTables
  where
    flattenTable :: RawTable -> Either TOMLError Table
    flattenTable = foldlM (\t (k, v) -> insertAt k v t) Map.empty

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
    go history (k NonEmpty.:| ks) table = do
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

      Map.alterF (fmap Just . insert) k table

    insertFail :: [Text] -> Text -> Value -> Either TOMLError a
    insertFail history key currVal =
      normalizeErr . Text.unlines $
        [ "Could not add value to path \"" <> Text.intercalate "." (key : history) <> "\":"
        , "  Setting: " <> Text.intercalate "." (NonEmpty.toList allKeys) <> " = " <> Text.pack (show val)
        , "  Existing value: " <> Text.pack (show currVal)
        ]

normalizeErr :: Text -> Either TOMLError a
normalizeErr = Left . NormalizeError

{--- Parser Helpers ---}

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

#if !MIN_VERSION_megaparsec(9,0,0)
hspace :: Parser ()
hspace = void $ takeWhileP (Just "white space") isHSpace

hspace1 :: Parser ()
hspace1 = void $ takeWhile1P (Just "white space") isHSpace

isHSpace :: Char -> Bool
isHSpace x = isSpace x && x /= '\n' && x /= '\r'
#endif

{--- Read Helpers ---}

-- | Assumes string satisfies @all isDigit@.
readDec :: (Show a, Num a, Eq a) => String -> a
readDec = runReader Numeric.readDec

-- | Assumes string satisfies @all isHexDigit@.
readHex :: (Show a, Num a, Eq a) => String -> a
readHex = runReader Numeric.readHex

-- | Assumes string satisfies @all isOctDigit@.
readOct :: (Show a, Num a, Eq a) => String -> a
readOct = runReader Numeric.readOct

-- | Assumes string satisfies @all (`elem` "01")@.
readBin :: (Show a, Num a) => String -> a
readBin s = foldl' go 0 s
  where
    go acc x =
      let digit
            | x == '0' = 0
            | x == '1' = 1
            | otherwise = error $ "readBin got unexpected digit: " <> show x
       in 2 * acc + digit

runReader :: Show a => ReadS a -> String -> a
runReader rdr digits =
  case rdr digits of
    [(x, "")] -> x
    result -> error $ "Unexpectedly unable to parse " <> show digits <> ": " <> show result
