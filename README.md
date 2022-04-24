# toml-reader

TOML format parser compliant with v1.0.0.

## Usage

TODO

## Design decisions

* Only supports reading, not writing, since TOML is lossy. For example, a simple `a.b.c = 1` line could be written in a number of ways:

    ```toml
    a.b.c = 1
    a.b = { c = 1 }
    a = { b.c = 1 }

    [a]
    b.c = 1
    b = { c = 1 }

    [a.b]
    c = 1
    ```

    Since reading/writing isn't an idempotent operation, this library won't even pretend to provide `FromTOML`/`ToTOML` typeclasses that imply that they're inverses of each other.

    Hopefully some other `toml-writer` library may come along to make it easy to specify how to format your data in TOML (e.g. a combinator for `table` vs `inlineTable`), or you could use [`tomland`](https://github.com/kowainik/tomland).
