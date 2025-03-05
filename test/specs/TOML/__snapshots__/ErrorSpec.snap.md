# TOML.Error

## renderTOMLError / renders DecodeError.InvalidValue

```
Decode error at '.a.b': Invalid value: bad value: true
```

## renderTOMLError / renders DecodeError.MissingField

```
Decode error at '.a.b': Field does not exist
```

## renderTOMLError / renders DecodeError.OtherDecodeError

```
Decode error at '.a.b': decode failure
```

## renderTOMLError / renders DecodeError.TypeMismatch

```
Decode error at '.a.b': Type mismatch, got: true
```

## renderTOMLError / renders NormalizeError.DuplicateKeyError

```
Could not add value to path "a.b":
  Existing value: true
  Value to set: 1
```

## renderTOMLError / renders NormalizeError.DuplicateSectionError

```
Found duplicate section: "a.b"
```

## renderTOMLError / renders NormalizeError.ExtendTableError

```
Invalid table key: "a.b"
  Table already statically defined at "a"
```

## renderTOMLError / renders NormalizeError.ExtendTableInInlineArrayError

```
Invalid table key: "a.b"
  Table defined in inline array at "a"
```

## renderTOMLError / renders NormalizeError.ImplicitArrayForDefinedKeyError

```
Could not create implicit array at path "a.b":
  Existing value: [true]
  Array table section: {"a": 1}
```

## renderTOMLError / renders NormalizeError.NonTableInNestedImplicitArrayError

```
Found non-Table at path "a" when initializing implicit array at path "a.b":
  Existing value: true
  Array table section: {"a": 1}
```

## renderTOMLError / renders NormalizeError.NonTableInNestedKeyError

```
Found non-Table at path "a" when defining nested key "a.b":
  Existing value: true
  Original value: 1
```

## renderTOMLError / renders ParseError

```
megaparsec error
```
