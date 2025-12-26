# Data Analysis with Haskell

A Haskell project for analyzing the Titanic dataset.

## Prerequisites

- Haskell (GHC 9.10.3 or later)
- Cabal build tool
- devenv (for Nix environment)
- curl and unzip utilities

## Setup

### 1. Download the Data

Run the following script to download the Titanic dataset:

```bash
#!/bin/bash
curl -L -o data/titanic-dataset.zip \
  https://www.kaggle.com/api/v1/datasets/download/yasserh/titanic-dataset
unzip data/titanic-dataset.zip -d data/
```

### 2. Environment Setup

If using devenv (recommended):

```bash
devenv shell
```

This will set up the required system dependencies including the snappy library.

### 3. Build the Project

```bash
cabal build
```

### 4. Run the Project

```bash
cabal exec da-exe
```

## Project Structure

- `src/` - Library source code
- `app/` - Executable source code
- `data/` - Dataset files
- `test/` - Test files

## Dependencies

- `dataframe` - For DataFrame operations
- `snappy` - System library for compression (installed via devenv)
