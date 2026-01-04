# Mapping alt JSON

This repository provides R utilities to **map**, **compare**, and **clean** `alt` JSON files.

---
```r
source("altClean.R")

# # 1) Run full mapping and export CSVs
# out <- run_alt_mapping(cfg$dict_dir, cfg$result_dir, export_csv = TRUE)
View(out$dict_wide)
View(out$dict_long)
View(out$result_compare)
# 
# # 2) Single-country report + dry-run delete listing
altClean(630, dict_dir = cfg$dict_dir, result_dir = cfg$result_dir, delete = FALSE)
# 
# # 3) Actually delete (be careful)
altClean(630, dict_dir = cfg$dict_dir, result_dir = cfg$result_dir, delete = TRUE)
```

# JSON extraction + country PDF reporting (R)

This script extracts model metadata and data-source information

---
