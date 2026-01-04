# Map and Clean JSON Alt Files

This repository provides R utilities to **map**, **compare**, and **clean** `alt` JSON files by checking consistency between:

- Main country JSON files (dict)
- `alt/` folders in the dict directory
- `alt/` folders (or root) in the results directory

The core function identifies `alt` files that are **not referenced** in the main JSON and can either **list** them or **delete** them safely.

---

## Usage

Source the script in R:

```r
source("altClean.R")
altClean(300, delete = FALSE)
altClean(300, delete = TRUE)


