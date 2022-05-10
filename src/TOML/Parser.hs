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

import Control.Monad (guard, void)
import Control.Monad.Combinators.NonEmpty (sepBy1)
import Data.Bifunctor (bimap)
import Data.Char (chr, isDigit, isSpace, ord)
import Data.Fixed (Fixed (..))
import Data.Foldable (foldl', foldlM)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (Day, LocalTime, TimeOfDay, UTCTime)
import qualified Data.Time as Time
import Data.Void (Void)
import qualified Numeric
import Text.Megaparsec hiding (sepBy1)
import Text.Megaparsec.Char hiding (space, space1)
import qualified Text.Megaparsec.Char.Lexer as L

import TOML.Internal (NormalizeError (..), TOMLError (..), Table, Value (..))

parseTOML :: String -> Text -> Either TOMLError Value
parseTOML filename input =
  case runParser parseTOMLDocument filename input of
    Left e -> Left $ ParseError $ Text.pack $ errorBundlePretty e
    Right result -> Table <$> normalize result

-- 'Value' generalized to allow for unnormalized + annotated Values.
data GenericValue map key tableMeta arrayMeta
  = GenericTable tableMeta (map key (GenericValue map key tableMeta arrayMeta))
  | GenericArray arrayMeta [GenericValue map key tableMeta arrayMeta]
  | GenericString Text
  | GenericInteger Integer
  | GenericFloat Double
  | GenericBoolean Bool
  | GenericOffsetDateTime UTCTime
  | GenericLocalDateTime LocalTime
  | GenericLocalDate Day
  | GenericLocalTime TimeOfDay

{--- Parse raw document ---}

type Parser = Parsec Void Text

-- | An unannotated, unnormalized value.
type RawValue = GenericValue LookupMap Key () ()

type Key = NonEmpty Text
type RawTable = LookupMap Key RawValue
newtype LookupMap k v = LookupMap {unLookupMap :: [(k, v)]}

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
parseRawTable = fmap LookupMap $ many $ parseKeyValue <* endOfLine <* emptyLines

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

parseKeyValue :: Parser (Key, RawValue)
parseKeyValue = do
  key <- parseKey
  hsymbol "="
  value <- parseValue
  pure (key, value)

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

parseValue :: Parser RawValue
parseValue =
  choice
    [ try $ GenericTable () <$> label "table" parseInlineTable
    , try $ GenericArray () <$> label "array" parseInlineArray
    , try $ GenericString <$> label "string" parseString
    , try $ GenericOffsetDateTime <$> label "offset-datetime" parseOffsetDateTime
    , try $ GenericLocalDateTime <$> label "local-datetime" parseLocalDateTime
    , try $ GenericLocalDate <$> label "local-date" parseLocalDate
    , try $ GenericLocalTime <$> label "local-time" parseLocalTime
    , try $ GenericFloat <$> label "float" parseFloat
    , try $ GenericInteger <$> label "integer" parseInteger
    , try $ GenericBoolean <$> label "boolean" parseBoolean
    ]

parseInlineTable :: Parser RawTable
parseInlineTable = do
  hsymbol "{"
  kvs <- parseKeyValue `sepBy` try (hsymbol ",")
  hsymbol "}"
  return $ LookupMap kvs

parseInlineArray :: Parser [RawValue]
parseInlineArray = do
  _ <- char '[' <* emptyLines
  vs <- (parseValue <* emptyLines) `sepEndBy` (char ',' <* emptyLines)
  _ <- char ']'
  return vs

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

-- | An annotated, normalized Value
type AnnValue = GenericValue Map Text TableMeta ArrayMeta

type AnnTable = Map Text AnnValue

unannotateTable :: AnnTable -> Table
unannotateTable = fmap unannotateValue

unannotateValue :: AnnValue -> Value
unannotateValue = \case
  GenericTable _ t -> Table $ unannotateTable t
  GenericArray _ vs -> Array $ map unannotateValue vs
  GenericString x -> String x
  GenericInteger x -> Integer x
  GenericFloat x -> Float x
  GenericBoolean x -> Boolean x
  GenericOffsetDateTime x -> OffsetDateTime x
  GenericLocalDateTime x -> LocalDateTime x
  GenericLocalDate x -> LocalDate x
  GenericLocalTime x -> LocalTime x

data TableType
  = -- | An inline table, e.g. "a.b" in:
    --
    -- @
    -- a.b = { c = 1 }
    -- @
    InlineTable
  | -- | A table created implicitly from a nested key, e.g. "a" in:
    --
    -- @
    -- a.b = 1
    -- @
    ImplicitKey
  | -- | An explicitly named section, e.g. "a.b.c" and "a.b" but not "a" in:
    --
    -- @
    -- [a.b.c]
    -- [a.b]
    -- @
    ExplicitSection
  | -- | An implicitly created section, e.g. "a" in:
    --
    -- @
    -- [a.b]
    -- @
    --
    -- Can later be converted into an explicit section
    ImplicitSection
  deriving (Eq)

data TableMeta = TableMeta
  { tableType :: TableType
  }

newtype ArrayMeta = ArrayMeta
  { isStaticArray :: Bool
  }

newtype NormalizeM a = NormalizeM
  { runNormalizeM :: Either NormalizeError a
  }

instance Functor NormalizeM where
  fmap f = NormalizeM . fmap f . runNormalizeM
instance Applicative NormalizeM where
  pure = NormalizeM . pure
  NormalizeM f <*> NormalizeM x = NormalizeM (f <*> x)
instance Monad NormalizeM where
  m >>= f = NormalizeM $ runNormalizeM . f =<< runNormalizeM m

normalizeError :: NormalizeError -> NormalizeM a
normalizeError = NormalizeM . Left

normalize :: TOMLDoc -> Either TOMLError Table
normalize = bimap NormalizeError unannotateTable . runNormalizeM . normalize'

normalize' :: TOMLDoc -> NormalizeM AnnTable
normalize' TOMLDoc{..} = do
  root <- flattenTable rootTable
  foldlM (flip mergeTableSection) root subTables
  where
    mergeTableSection :: TableSection -> AnnTable -> NormalizeM AnnTable
    mergeTableSection TableSection{..} baseTable = do
      case tableSectionHeader of
        SectionTable key ->
          mergeTableSectionTable key tableSectionTable baseTable
        SectionTableArray key -> do
          subTable <- flattenTable tableSectionTable
          mergeTableSectionArray key subTable baseTable

    mergeTableSectionTable sectionKey table baseTable = do
      -- prepend section key to all keys in section
      let table' = LookupMap . map (\(k, v) -> (sectionKey <> k, v)) . unLookupMap $ table
      -- ensure we initialize an empty table, if there's nothing in the table
      baseTable' <- initializePath sectionKey baseTable
      -- merge
      table' `mergeInto` baseTable'

    mergeTableSectionArray :: Key -> AnnTable -> AnnTable -> NormalizeM AnnTable
    mergeTableSectionArray sectionKey table baseTable = do
      let newTableMeta = TableMeta{tableType = ExplicitSection}
          callbacks =
            ModifyTableCallbacks
              { alterEnd = \case
                  -- if nothing exists, insert table into a new array
                  Nothing -> do
                    let meta = ArrayMeta{isStaticArray = False}
                    pure $ Just $ GenericArray meta [GenericTable newTableMeta table]
                  -- if an array exists, insert table to the end of the array
                  Just (GenericArray meta l)
                    | not (isStaticArray meta) ->
                        pure $ Just $ GenericArray meta $ l <> [GenericTable newTableMeta table]
                  -- otherwise, error
                  Just existingValue ->
                    normalizeError
                      ImplicitArrayForDefinedKeyError
                        { _path = NonEmpty.toList sectionKey
                        , _existingValue = unannotateValue existingValue
                        , _tableSection = unannotateTable table
                        }
              , onMidPathValue = \history existingValue ->
                  normalizeError
                    NonTableInNestedImplicitArrayError
                      { _path = history
                      , _existingValue = unannotateValue existingValue
                      , _sectionKey = NonEmpty.toList sectionKey
                      , _tableSection = unannotateTable table
                      }
              }
      modifyValueAtPathF callbacks sectionKey baseTable

flattenTable :: RawTable -> NormalizeM AnnTable
flattenTable = (`mergeInto` Map.empty)

mergeInto :: RawTable -> AnnTable -> NormalizeM AnnTable
table `mergeInto` baseTable = foldlM insertRawValue baseTable (unLookupMap table)
  where
    insertRawValue accTable (key, rawValue) = do
      value <- fromRawValue rawValue
      let callbacks =
            ModifyTableCallbacks
              { alterEnd = \case
                  Nothing -> pure $ Just value
                  Just existingValue ->
                    normalizeError
                      DuplicateKeyError
                        { _path = NonEmpty.toList key
                        , _existingValue = unannotateValue existingValue
                        , _valueToSet = unannotateValue value
                        }
              , onMidPathValue = \history existingValue ->
                  normalizeError
                    NonTableInNestedKeyError
                      { _path = history
                      , _existingValue = unannotateValue existingValue
                      , _originalKey = NonEmpty.toList key
                      , _originalValue = unannotateValue value
                      }
              }
      modifyValueAtPathF callbacks key accTable

    fromRawValue = \case
      GenericTable _ rawTable -> do
        let meta = TableMeta{tableType = InlineTable}
        GenericTable meta <$> flattenTable rawTable
      GenericArray _ rawValues -> do
        let meta = ArrayMeta{isStaticArray = True}
        GenericArray meta <$> mapM fromRawValue rawValues
      GenericString x -> pure (GenericString x)
      GenericInteger x -> pure (GenericInteger x)
      GenericFloat x -> pure (GenericFloat x)
      GenericBoolean x -> pure (GenericBoolean x)
      GenericOffsetDateTime x -> pure (GenericOffsetDateTime x)
      GenericLocalDateTime x -> pure (GenericLocalDateTime x)
      GenericLocalDate x -> pure (GenericLocalDate x)
      GenericLocalTime x -> pure (GenericLocalTime x)

-- | Initialize a table at the given path.
initializePath :: Key -> AnnTable -> NormalizeM AnnTable
initializePath sectionKey table =
  modifyValueAtPathF
    ModifyTableCallbacks
      { alterEnd = \case
          Just (GenericTable meta existingTable) ->
            case tableType meta of
              -- error if we're redefining an inline table
              InlineTable ->
                normalizeError
                  DuplicateKeyError
                    { _path = NonEmpty.toList sectionKey
                    , _existingValue = Table $ unannotateTable existingTable
                    , _valueToSet = Table $ unannotateTable table
                    }
              -- error if we're redefining an explicit table
              ExplicitSection ->
                normalizeError
                  DuplicateSectionError
                    { _sectionKey = NonEmpty.toList sectionKey
                    }
              -- otherwise, mark table as explicit, e.g. in the config:
              --   [a.b.c.d]
              --   [a.b]
              _ -> pure $ Just $ GenericTable meta{tableType = ExplicitSection} existingTable
          -- insert an empty Map if a value doesn't already exist
          Nothing -> do
            let meta = TableMeta{tableType = ExplicitSection}
            pure $ Just $ GenericTable meta Map.empty
          -- if some other value exists, leave it untouched, let mergeInto handle it
          Just existingValue -> pure $ Just existingValue
      , onMidPathValue = \_ -> pure
      }
    sectionKey
    table

type PathHistory = [Text] -- The log of keys traversed so far
data ModifyTableCallbacks = ModifyTableCallbacks
  { alterEnd :: Maybe AnnValue -> NormalizeM (Maybe AnnValue)
  -- ^ Alter the (possibly missing) Annvalue at the end of the path.
  , onMidPathValue :: PathHistory -> AnnValue -> NormalizeM AnnValue
  -- ^ Alter a value in the middle of the path, when not recursing
  }

-- | A helper for recursing through a Table.
modifyValueAtPathF :: ModifyTableCallbacks -> Key -> AnnTable -> NormalizeM AnnTable
modifyValueAtPathF ModifyTableCallbacks{..} sectionKey = go [] sectionKey
  where
    go history (k NonEmpty.:| ks) table = Map.alterF (handle (history ++ [k]) ks) k table
    handle history ks mVal =
      case NonEmpty.nonEmpty ks of
        -- when we're at the last key
        Nothing -> alterEnd mVal
        -- when we want to keep recursing ...
        Just ks' ->
          let go' meta = fmap (GenericTable meta) . go history ks'
           in fmap Just $
                case mVal of
                  -- ... and nothing exists, recurse into a new empty Map
                  Nothing -> go' TableMeta{tableType = ImplicitSection} Map.empty
                  -- ... and a Table exists ...
                  Just (GenericTable meta subTable)
                    -- ... and the Table is an inline table, error
                    | InlineTable <- tableType meta ->
                        normalizeError
                          ExtendTableError
                            { _path = history
                            , _originalKey = NonEmpty.toList sectionKey
                            }
                    -- ... otherwise recurse into it
                    | otherwise -> go' meta subTable
                  -- ... and Array exists, recurse into the last Table, per spec:
                  --   Any reference to an array of tables points to the
                  --   most recently defined table element of the array.
                  Just (GenericArray aMeta vs)
                    | Just vs' <- NonEmpty.nonEmpty vs
                    , GenericTable tMeta subTable <- NonEmpty.last vs' ->
                        GenericArray aMeta . snoc (NonEmpty.init vs') <$> go' tMeta subTable
                  -- ... and something else exists, call onMidPathValue callback
                  Just v -> onMidPathValue history v

    snoc xs x = xs <> [x]

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
skipComments = do
  _ <- string "#"
  void . many $ do
    c <- satisfy (/= '\n')
    let code = ord c
    case c of
      '\r' -> void $ lookAhead (char '\n')
      _
        | (0x00 <= code && code <= 0x08) || (0x0A <= code && code <= 0x1F) || code == 0x7F ->
            fail $ "Comment has invalid character: \\" <> show code
      _ -> pure ()

space, space1 :: Parser ()
space = void $ many parseSpace
space1 = void $ some parseSpace

-- | TOML does not support bare '\r' without '\n'.
parseSpace :: Parser ()
parseSpace = void (satisfy (\c -> isSpace c && c /= '\r')) <|> void (string "\r\n")

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
