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

import Control.Monad (guard)
import Control.Monad.Combinators.NonEmpty (sepBy1)
import Data.Char (chr, isDigit, ord)
import Data.Fixed (Fixed (..))
import Data.Foldable (foldl', foldlM)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (Day, LocalTime, TimeOfDay, UTCTime)
import qualified Data.Time as Time
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
  hsymbol "="
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
    parseHeader brackStart brackEnd = hsymbol brackStart *> parseKey <* hsymbol brackEnd

parseKey :: Parser Key
parseKey =
  (`sepBy1` try (hsymbol ".")) . choice $
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
    , try $ OffsetDateTime <$> label "offset-datetime" parseOffsetDateTime
    , try $ LocalDateTime <$> label "local-datetime" parseLocalDateTime
    , try $ LocalDate <$> label "local-date" parseLocalDate
    , try $ LocalTime <$> label "local-time" parseLocalTime
    , try $ Float <$> label "float" parseFloat
    , try $ Integer <$> label "integer" parseInteger
    , try $ Boolean <$> label "boolean" parseBoolean
    ]
  where
    parseInlineTable = empty -- TODO
    parseInlineArray = empty -- TODO

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
    between (char '"') (char '"') $
      fmap Text.pack . many . choice $
        [ satisfy isBasicChar
        , parseEscaped
        ]

-- | A string in single quotes.
parseLiteralString :: Parser Text
parseLiteralString =
  label "single-quoted string" $
    between (char '\'') (char '\'') $ takeWhileP (Just "literal-char") isLiteralChar

-- | A multiline string with three double quotes.
parseMultilineBasicString :: Parser Text
parseMultilineBasicString =
  label "double-quoted multiline string" $ do
    _ <- string "\"\"\"" *> optional eol
    lineContinuation
    Text.concat <$> manyTill (mlBasicContent <* lineContinuation) (exactly 3 '"')
  where
    mlBasicContent =
      choice
        [ Text.singleton <$> try parseEscaped
        , Text.singleton <$> satisfy isBasicChar
        , parseMultilineDelimiter '"'
        , eol
        ]
    lineContinuation = many (try $ char '\\' *> hspace *> eol *> space) *> pure ()

-- | A multiline string with three single quotes.
parseMultilineLiteralString :: Parser Text
parseMultilineLiteralString =
  label "single-quoted multiline string" $ do
    _ <- string "'''" *> optional eol
    Text.concat <$> manyTill mlLiteralContent (exactly 3 '\'')
  where
    mlLiteralContent =
      choice
        [ Text.singleton <$> satisfy isLiteralChar
        , parseMultilineDelimiter '\''
        , eol
        ]

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
      code <- readHex . Text.pack <$> count n hexDigitChar
      guard $ isUnicodeScalar code
      pure $ chr code

{- |
Parse the multiline delimiter (" in """ quotes, or ' in ''' quotes), unless
the delimiter indicates the end of the multiline string.

i.e. parse 1 or 2 delimiters, or 4 or 5, which is 1 or 2 delimiters at the
end of a multiline string (then backtrack 3 to mark the end).
-}
parseMultilineDelimiter :: Char -> Parser Text
parseMultilineDelimiter delim =
  choice
    [ exactly 1 delim
    , exactly 2 delim
    , do
        _ <- lookAhead (exactly 4 delim)
        Text.pack <$> count 1 (char delim)
    , do
        _ <- lookAhead (exactly 5 delim)
        Text.pack <$> count 2 (char delim)
    ]

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

isLiteralChar :: Char -> Bool
isLiteralChar c =
  case c of
    ' ' -> True
    '\t' -> True
    _ | 0x21 <= code && code <= 0x7E -> c /= '\''
    _ | isNonAscii c -> True
    _ -> False
  where
    code = ord c

parseOffsetDateTime :: Parser UTCTime
parseOffsetDateTime = do
  lt <- parseLocalDateTime
  tz <- parseTimezone
  return $ Time.localTimeToUTC tz lt
  where
    parseTimezone =
      choice
        [ char' 'Z' $> Time.utc
        , do
            applySign <- parseSign
            h <- parseHours
            _ <- char ':'
            m <- parseMinutes
            return $ Time.minutesToTimeZone $ applySign $ h * 60 + m
        ]

parseLocalDateTime :: Parser LocalTime
parseLocalDateTime = do
  d <- parseLocalDate
  _ <- char' 'T' <|> char ' '
  t <- parseLocalTime
  return $ Time.LocalTime d t

parseLocalDate :: Parser Day
parseLocalDate = do
  y <- parseDecDigits 4
  _ <- char '-'
  m <- parseDecDigits 2
  _ <- char '-'
  d <- parseDecDigits 2
  maybe empty return $ Time.fromGregorianValid y m d

parseLocalTime :: Parser TimeOfDay
parseLocalTime = do
  h <- parseHours
  _ <- char ':'
  m <- parseMinutes
  _ <- char ':'
  sInt <- parseSeconds
  sFracRaw <- optional $ fmap Text.pack $ char '.' >> some digitChar
  let sFrac = MkFixed $ maybe 0 (readDec . truncateText 12) sFracRaw
  return $ Time.TimeOfDay h m (fromIntegral sInt + sFrac)

parseHours :: Parser Int
parseHours = do
  h <- parseDecDigits 2
  guard $ 0 <= h && h < 24
  return h

parseMinutes :: Parser Int
parseMinutes = do
  m <- parseDecDigits 2
  guard $ 0 <= m && m < 60
  return m

parseSeconds :: Parser Int
parseSeconds = do
  s <- parseDecDigits 2
  guard $ 0 <= s && s <= 60 -- include 60 for leap seconds
  return s

parseFloat :: Parser Double
parseFloat = do
  applySign <- parseSign
  num <-
    choice
      [ try normalFloat
      , try $ string "inf" $> inf
      , try $ string "nan" $> nan
      ]
  pure $ applySign num
  where
    normalFloat = do
      intPart <- parseDecIntRaw
      (fracPart, expPart) <-
        choice
          [ try $ (,) <$> pure "" <*> parseExp
          , (,) <$> parseFrac <*> optionalOr "" parseExp
          ]
      pure $ readFloat $ intPart <> fracPart <> expPart

    parseExp =
      fmap Text.concat . sequence $
        [ string' "e"
        , parseSignRaw
        , parseNumRaw digitChar digitChar
        ]
    parseFrac =
      fmap Text.concat . sequence $
        [ string "."
        , parseNumRaw digitChar digitChar
        ]

    inf = read "Infinity"
    nan = read "NaN"

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
      applySign <- parseSign
      num <- readDec <$> parseDecIntRaw
      pure $ applySign num
    parseHexInt =
      parsePrefixedInt readHex "0x" hexDigitChar
    parseOctInt =
      parsePrefixedInt readOct "0o" octDigitChar
    parseBinInt =
      parsePrefixedInt readBin "0b" binDigitChar

    parsePrefixedInt readInt prefix parseDigit = do
      _ <- string prefix
      readInt <$> parseNumRaw parseDigit parseDigit

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

-- | https://github.com/toml-lang/toml/blob/1.0.0/toml.abnf#L38
isNonAscii :: Char -> Bool
isNonAscii c = (0x80 <= code && code <= 0xD7FF) || (0xE000 <= code && code <= 0x10FFFF)
  where
    code = ord c

-- | https://unicode.org/glossary/#unicode_scalar_value
isUnicodeScalar :: Int -> Bool
isUnicodeScalar code = (0x0 <= code && code <= 0xD7FF) || (0xE000 <= code && code <= 0x10FFFF)

-- | Returns "", "-", or "+"
parseSignRaw :: Parser Text
parseSignRaw = optionalOr "" (string "-" <|> string "+")

parseSign :: Num a => Parser (a -> a)
parseSign = do
  sign <- parseSignRaw
  pure $ if sign == "-" then negate else id

parseDecIntRaw :: Parser Text
parseDecIntRaw =
  choice
    [ try $ parseNumRaw (satisfy $ \c -> isDigit c && c /= '0') digitChar
    , Text.singleton <$> digitChar
    ]

parseDecDigits :: (Show a, Num a, Eq a) => Int -> Parser a
parseDecDigits n = readDec . Text.pack <$> count n digitChar

parseNumRaw :: Parser Char -> Parser Char -> Parser Text
parseNumRaw parseLeadingDigit parseDigit = do
  leading <- parseLeadingDigit
  rest <- many $ optional (char '_') *> parseDigit
  pure $ Text.pack $ leading : rest

{--- Parser Utilities ---}

hsymbol :: Text -> Parser ()
hsymbol s = hspace >> string s >> hspace >> pure ()

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

optionalOr :: a -> Parser a -> Parser a
optionalOr def = fmap (fromMaybe def) . optional

exactly :: Int -> Char -> Parser Text
exactly n c = try $ Text.pack <$> count n (char c) <* notFollowedBy (char c)

{--- Read Helpers ---}

truncateText :: Int -> Text -> Text
truncateText n t =
  case Text.chunksOf n t of
    [] -> ""
    t' : _ -> t'

-- | Assumes string satisfies @all isDigit@.
readFloat :: (Show a, RealFrac a) => Text -> a
readFloat = runReader Numeric.readFloat

-- | Assumes string satisfies @all isDigit@.
readDec :: (Show a, Num a, Eq a) => Text -> a
readDec = runReader Numeric.readDec

-- | Assumes string satisfies @all isHexDigit@.
readHex :: (Show a, Num a, Eq a) => Text -> a
readHex = runReader Numeric.readHex

-- | Assumes string satisfies @all isOctDigit@.
readOct :: (Show a, Num a, Eq a) => Text -> a
readOct = runReader Numeric.readOct

-- | Assumes string satisfies @all (`elem` "01")@.
readBin :: (Show a, Num a) => Text -> a
readBin = foldl' go 0 . Text.unpack
  where
    go acc x =
      let digit
            | x == '0' = 0
            | x == '1' = 1
            | otherwise = error $ "readBin got unexpected digit: " <> show x
       in 2 * acc + digit

runReader :: Show a => ReadS a -> Text -> a
runReader rdr digits =
  case rdr $ Text.unpack digits of
    [(x, "")] -> x
    result -> error $ "Unexpectedly unable to parse " <> show digits <> ": " <> show result
