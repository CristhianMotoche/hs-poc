# contratos

A CLI tool for generating rental contract DOCX documents from Mustache templates and YAML context files.

## How it works

1. A Mustache template (Markdown) defines the contract structure.
2. A YAML file provides the context data (parties, amounts, dates, etc.).
3. The tool renders the template, converts the result to DOCX via Pandoc.

## Usage

```bash
contratos <template.md> <context.yml> [output.docx]
```

If `output.docx` is omitted, the output file is named `<template.md>.docx`.

### Example

```bash
contratos PLANTILLA.md data/dep2.yml contrato.docx
```

## Context schema

The YAML context file must conform to the following structure:

```yaml
street_one: <string>
street_two: <string>
tenant:
  name: <string>
  cid: <integer>
canon_price: <number>
canon_price_words: <string>
landlords:
  - name: <string>
    cid: <string>   # quoted to preserve leading zeros
banking:
  name: <string>
  cid: <string>
  number: <string>
  account: <string>
payment_day: <integer>
dates:
  from:
    d: <integer>
    m: <integer>
    y: <integer>
  to:
    d: <integer>
    m: <integer>
    y: <integer>
  sign:
    d: <integer>
    m: <integer>
    y: <integer>
```

> **Note:** `cid` fields for landlords and banking should be quoted strings in YAML (e.g., `"0702138983"`) to preserve leading zeros.

## Encrypted files

Sensitive files (`PLANTILLA.md` and `data/dep2.yml`) are stored encrypted as `.gpg` files. The password is in LastPass.

### Decrypt

```bash
gpg --output PLANTILLA.md --decrypt PLANTILLA.md.gpg
gpg --output data/dep2.yml --decrypt data/dep2.yml.gpg
```

### Encrypt

```bash
gpg --symmetric --cipher-algo AES256 --output PLANTILLA.md.gpg PLANTILLA.md
gpg --symmetric --cipher-algo AES256 --output data/dep2.yml.gpg data/dep2.yml
```

## Building

Requires [GHC](https://www.haskell.org/ghc/) and [Cabal](https://www.haskell.org/cabal/).

```bash
cabal build
cabal run contratos -- PLANTILLA.md data/dep2.yml
```

## Dependencies

- [`aeson`](https://hackage.haskell.org/package/aeson) — JSON/YAML value handling
- [`pandoc`](https://hackage.haskell.org/package/pandoc) — Markdown to DOCX conversion
- [`stache`](https://hackage.haskell.org/package/stache) — Mustache template rendering
- [`yaml`](https://hackage.haskell.org/package/yaml) — YAML parsing

## License

WTFPL
