# SLIP Package Function Reference

## General Purpose Functions

| Function | Arguments | Description |
|----------|-----------|-------------|
| `hello` | none | Prints welcome message |
| `blank-line` | none | Prints a blank line |
| `report` | `label`, `var` | Formats and prints a labeled variable |
| `disp` | `label`, `&rest objs` | Displays labeled objects with formatting |
| `typewriter-string` | `s`, `n` | Prints string with typewriter effect using delay `n` |
| `dot-display` | `lab`, `val`, `n` | Displays label padded with dots to length `n`, followed by value |
| `dot-display-type` | `lab`, `val`, `n` | Similar to dot-display but with typewriter effect |

## Character Functions

| Function | Arguments | Description |
|----------|-----------|-------------|
| `char-upperp` | `c` | Tests if character is uppercase |
| `rand-char` | none | Generates random character |

## String Functions

| Function | Arguments | Description |
|----------|-----------|-------------|
| `rtrim` | `s` | Removes spaces from right end of string |
| `startswith` | `sub`, `str` | Tests if string starts with substring |
| `endswith` | `sub`, `str` | Tests if string ends with substring |
| `split-line` | `line`, `delim` | Splits line by delimiter |

## List Functions

| Function | Arguments | Description |
|----------|-----------|-------------|
| `foreach-i` | `lst`, `&body b` | Macro for iterating over list |
| `generate` | `start`, `end`, `&optional increment` | Generates list of numbers |
| `sort-list` | `l` | Sorts list of strings alphabetically |
| `random-choice` | `a` | Returns random element from list or array |
| `random-elt` | `choices` | Returns random element from list |
| `shuffle` | `l` | Returns randomized copy of list |
| `one-of` | `set` | Returns single-element list with random choice |
| `list->array` | `l` | Converts list to array |

## File Functions

| Function | Arguments | Description |
|----------|-----------|-------------|
| `file-loop` | `filename`, `func` | Applies function to each line in file |
| `process-file` | `filename`, `func`, `delim` | Processes delimited file with function |
| `file-to-list` | `filename` | Reads file into list |
| `cut-file` | `f`, `delim`, `col` | Extracts column from delimited file |
| `list-to-file` | `lst`, `filename` | Writes list to file |
| `with-file-lines` | `fname`, `line`, `&body b` | Macro for processing file lines |
| `file-matrix` | `fname`, `delim` | Parses delimited file into 2D array |
| `matrix-to-file` | `array`, `fname` | Writes 2D array to file |

## Array Functions

| Function | Arguments | Description |
|----------|-----------|-------------|
| `extract-column` | `col-n`, `matrix` | Extracts column from 2D array as list |

## Hash Table Functions

| Function | Arguments | Description |
|----------|-----------|-------------|
| `store-hash` | `h`, `k`, `v` | Stores value in hash table |
| `assoc-to-hash` | `l` | Converts association list to hash table |
| `list-to-hash` | `lst` | Converts flat list to hash table |
| `show-hash` | `h` | Displays hash table contents |
| `show-hash-type` | `h` | Displays hash table contents with typewriter effect |
| `sum-hash` | `h` | Sums numeric values in hash table |
| `stats-hash` | `h` | Displays statistical analysis of hash values |

## Test Data Functions

| Function | Arguments | Description |
|----------|-----------|-------------|
| `creature-list` | none | Returns list of mythical creatures for testing |
