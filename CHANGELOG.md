# Changelog

## 1.0.0

Initial release

### 1.0.1

Fix stack overflow for large byte sequences.
[Thanks, @edkv](https://discourse.elm-lang.org/t/base64-encoding-decoding/2693/2)

### 1.0.2

[Tons of optimizations](https://discourse.elm-lang.org/t/optimizing-base64-encoding-decoding/3603)
by @folkertdev ([PR #1](https://github.com/danfishgold/base64-bytes/pull/1))

### 1.0.3

Even more decoder optimizations (2x speedup) by @folkertdev
([PR #3](https://github.com/danfishgold/base64-bytes/pull/3))

### 1.1.0

Add `toString` and `fromString`, which are wrappers around `toBytes` and
`fromBytes`, to encode/decode Base64 strings from/into strings, so that projects
that use this package for these use cases don't need to directly import `Bytes`
as well.
