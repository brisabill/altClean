suppressPackageStartupMessages({
  library(jsonlite)
  library(purrr)
  library(stringr)
  library(data.table)
})

# -------------------------
# SETTINGS
# -------------------------
dict_dir   = normalizePath("C:/Users/mirandaa/Desktop/estican/estican/dict",   winslash = "/", mustWork = FALSE),
result_dir = normalizePath("C:/Users/mirandaa/Desktop/estican/estican/result", winslash = "/", mustWork = FALSE)

dry_run <- TRUE  # set to FALSE to actually delete

# -------------------------
# HELPERS
# -------------------------

safe_fromJSON <- function(path) {
  tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) {
      warning(sprintf("Failed to parse JSON: %s | %s", path, e$message), call. = FALSE)
      NULL
    }
  )
}

# Extract "alt" anywhere in nested JSON lists; alt can be string OR list
extract_alts_from_main <- function(j) {
  if (is.null(j)) return(character(0))
  
  walk_collect <- function(x) {
    if (!is.list(x)) return(character(0))
    
    found <- character(0)
    nms <- names(x)
    
    if (!is.null(nms) && "alt" %in% nms) {
      v <- x[["alt"]]
      v <- unlist(v, recursive = TRUE, use.names = FALSE)
      v <- as.character(v)
      v <- str_trim(v)
      v <- v[!is.na(v) & v != ""]
      found <- c(found, v)
    }
    
    kids <- purrr::map(x, walk_collect)
    c(found, unlist(kids, use.names = FALSE))
  }
  
  unique(walk_collect(j))
}

# List alt JSON files for a country (dict alt folder); drops "xx_" prefix and ".json"
list_alt_files_for_country <- function(alt_dir, country_code) {
  if (!dir.exists(alt_dir)) return(character(0))
  
  cc <- as.character(country_code)
  files <- list.files(alt_dir, pattern = "\\.json$", full.names = FALSE)
  
  # keep ONLY files that belong to this same country code (cc)
  files <- files[startsWith(files, paste0(cc, "_")) | files == paste0(cc, ".json")]
  if (!length(files)) return(character(0))
  
  stems <- str_remove(files, "\\.json$")
  stems <- str_remove(stems, paste0("^", cc, "_"))
  stems <- stems[!is.na(stems) & stems != ""]
  unique(stems)
}

# Region dirs are numeric names, including "06", "09", etc.
get_region_dirs <- function(dict_dir) {
  dirs <- list.dirs(dict_dir, recursive = FALSE, full.names = TRUE)
  dirs[str_detect(basename(dirs), "^\\d+$")]
}

# Find main JSON file for a country in dict_dir regions
find_main_json <- function(dict_dir, countrynumber) {
  cc <- as.character(countrynumber)
  region_dirs <- get_region_dirs(dict_dir)
  
  for (rdir in region_dirs) {
    candidate <- file.path(rdir, paste0(cc, ".json"))
    if (file.exists(candidate)) {
      return(list(main_path = candidate, region_hit = basename(rdir)))
    }
  }
  
  stop("Could not find main JSON for country ", cc, call. = FALSE)
}

# Parse results filenames (json/csv) and extract BASE alt (everything before _i/_m/_p block)
parse_result_file_alt <- function(files, cc) {
  if (!length(files)) return(data.table())
  
  dt <- data.table(file = files)
  dt[, file_noext := str_remove(file, "\\.(json|csv)$")]
  dt[, after_cc   := str_remove(file_noext, paste0("^", cc, "_"))]
  
  # base alt = everything before _i/_m/_p (and rest)
  dt[, alt := str_replace(after_cc, "(_[imp](?:_|$).*)$", "")]
  dt[, alt := str_trim(alt)]
  dt <- dt[!is.na(alt) & alt != ""]
  
  dt
}

# Extract alts/types from results folder (json only) to build compare table
extract_result_alts_for_country <- function(region_dir_result, country_code) {
  cc <- as.character(country_code)
  
  alt_dir <- file.path(region_dir_result, "alt")
  scan_dir <- if (dir.exists(alt_dir)) alt_dir else region_dir_result
  if (!dir.exists(scan_dir)) return(data.table())
  
  files <- list.files(scan_dir, pattern = "\\.json$", full.names = FALSE)
  files <- files[startsWith(files, paste0(cc, "_"))]
  if (!length(files)) return(data.table())
  
  dt <- data.table(file = files)
  dt[, file_noext := str_remove(file, "\\.json$")]
  
  dt[, type := fifelse(str_detect(file_noext, "_i_"), "i",
                       fifelse(str_detect(file_noext, "_m_"), "m", NA_character_))]
  dt <- dt[!is.na(type)]
  
  dt[, after_cc := str_remove(file_noext, paste0("^", cc, "_"))]
  dt[, alt := str_replace(after_cc, "(_[im]_.*)$", "")]
  dt[, alt := str_trim(alt)]
  dt <- dt[alt != "" & !is.na(alt)]
  
  unique(dt[, .(alt, type)])
}

# -------------------------
# MAIN TABLES (DICT)
# -------------------------

region_dirs <- get_region_dirs(dict_dir)

res_list <- lapply(region_dirs, function(rdir) {
  region  <- basename(rdir)              # can be "06"
  alt_dir <- file.path(rdir, "alt")
  
  main_jsons <- list.files(rdir, pattern = "\\.json$", full.names = TRUE)
  
  rbindlist(lapply(main_jsons, function(f) {
    cc <- str_remove(basename(f), "\\.json$")
    j  <- safe_fromJSON(f)
    
    country_label <- if (!is.null(j) && !is.null(j$country_label)) j$country_label else NA_character_
    
    alts_main_vec     <- extract_alts_from_main(j)
    alts_folder_vec   <- list_alt_files_for_country(alt_dir, cc)
    only_in_main_vec  <- setdiff(alts_main_vec, alts_folder_vec)
    only_in_fold_vec  <- setdiff(alts_folder_vec, alts_main_vec)
    
    data.table(
      region_folder = region,
      country_code  = suppressWarnings(as.integer(cc)),
      country_label = country_label,
      
      alts_in_main       = paste(sort(alts_main_vec), collapse = ", "),
      alt_files          = paste(sort(alts_folder_vec), collapse = ", "),
      only_in_main       = paste(sort(only_in_main_vec), collapse = ", "),
      only_in_alt_folder = paste(sort(only_in_fold_vec), collapse = ", "),
      
      n_alts_in_main       = length(alts_main_vec),
      n_alt_files          = length(alts_folder_vec),
      n_only_in_main       = length(only_in_main_vec),
      n_only_in_alt_folder = length(only_in_fold_vec)
    )
  }), fill = TRUE)
})

res <- rbindlist(res_list, fill = TRUE)

# export
fwrite(res, file.path(dict_dir, "alt_mapping_comparison.csv"))

# Long format (one alt per row)
res_long <- rbindlist(lapply(region_dirs, function(rdir) {
  region  <- basename(rdir)
  alt_dir <- file.path(rdir, "alt")
  
  main_jsons <- list.files(rdir, pattern = "\\.json$", full.names = TRUE)
  
  rbindlist(lapply(main_jsons, function(f) {
    cc <- str_remove(basename(f), "\\.json$")
    j  <- safe_fromJSON(f)
    
    country_label <- if (!is.null(j) && !is.null(j$country_label)) j$country_label else NA_character_
    
    alts_main_vec   <- extract_alts_from_main(j)
    alts_folder_vec <- list_alt_files_for_country(alt_dir, cc)
    
    all_alts <- sort(unique(c(alts_main_vec, alts_folder_vec)))
    if (!length(all_alts)) return(NULL)
    
    dt <- data.table(
      region_folder = region,
      country_code  = suppressWarnings(as.integer(cc)),
      country_label = country_label,
      alt = all_alts,
      in_main   = all_alts %in% alts_main_vec,
      in_folder = all_alts %in% alts_folder_vec
    )
    
    dt[, status := fifelse(in_main & in_folder, "both",
                           fifelse(in_main & !in_folder, "only_in_main",
                                   fifelse(!in_main & in_folder, "only_in_alt_folder", "neither")))]
    dt
  }), fill = TRUE)
}), fill = TRUE)

# -------------------------
# jsonclean (console report)
# -------------------------
jsonclean <- function(countrynumber, dict_dir) {
  hit <- find_main_json(dict_dir, countrynumber)
  cc <- as.character(countrynumber)
  
  main_path <- hit$main_path
  region_hit <- hit$region_hit
  
  alt_dir <- file.path(dirname(main_path), "alt")
  
  j <- safe_fromJSON(main_path)
  country_label <- if (!is.null(j) && !is.null(j$country_label)) j$country_label else NA_character_
  
  alts_main   <- sort(extract_alts_from_main(j))
  alts_folder <- sort(list_alt_files_for_country(alt_dir, cc))
  
  only_in_main   <- sort(setdiff(alts_main, alts_folder))
  only_in_folder <- sort(setdiff(alts_folder, alts_main))
  
  cat("\n============================\n")
  cat("Country:", ifelse(is.na(country_label), cc, paste0(country_label, " (", cc, ")")), "\n")
  cat("Region folder:", region_hit, "\n")
  cat("Main JSON:", main_path, "\n")
  cat("Alt folder:", alt_dir, "\n")
  cat("============================\n\n")
  
  cat("Alts referenced in MAIN JSON (", length(alts_main), "):\n", sep = "")
  if (!length(alts_main)) cat("  - none -\n") else cat("  - ", paste(alts_main, collapse = "\n  - "), "\n", sep = "")
  cat("\n")
  
  cat("Alt files present in ALT folder (", length(alts_folder), "):\n", sep = "")
  if (!length(alts_folder)) cat("  - none -\n") else cat("  - ", paste(alts_folder, collapse = "\n  - "), "\n", sep = "")
  cat("\n")
  
  cat("ONLY in MAIN JSON (missing files) (", length(only_in_main), "):\n", sep = "")
  if (!length(only_in_main)) cat("  - none -\n") else cat("  - ", paste(only_in_main, collapse = "\n  - "), "\n", sep = "")
  cat("\n")
  
  cat("ONLY in ALT folder (not referenced) (", length(only_in_folder), "):\n", sep = "")
  if (!length(only_in_folder)) cat("  - none -\n") else cat("  - ", paste(only_in_folder, collapse = "\n  - "), "\n", sep = "")
  cat("\n")
  
  invisible(list(
    country_code = suppressWarnings(as.integer(cc)),
    country_label = country_label,
    region_folder = region_hit,
    main_json_path = main_path,
    alt_dir = alt_dir,
    alts_main = alts_main,
    alts_folder = alts_folder,
    only_in_main = only_in_main,
    only_in_folder = only_in_folder
  ))
}

# -------------------------
# RESULTS: build table + compare to dict main JSON
# -------------------------

res_result_long <- rbindlist(lapply(region_dirs, function(rdir_dict) {
  region <- basename(rdir_dict)
  
  main_jsons <- list.files(rdir_dict, pattern = "^\\d+\\.json$", full.names = TRUE)
  
  rdir_result <- file.path(result_dir, region)
  if (!dir.exists(rdir_result)) return(NULL)
  
  rbindlist(lapply(main_jsons, function(f) {
    cc <- str_remove(basename(f), "\\.json$")
    
    dt <- extract_result_alts_for_country(rdir_result, cc)
    if (!nrow(dt)) return(NULL)
    
    data.table(
      region_folder = region,
      country_code  = suppressWarnings(as.integer(cc)),
      alt = dt$alt,
      type = dt$type
    )
  }), fill = TRUE)
}), fill = TRUE)

dict_main_alts <- unique(res_long[in_main == TRUE, .(region_folder, country_code, alt)])
dict_main_alts[, in_dict_main := TRUE]
setkey(dict_main_alts, region_folder, country_code, alt)

setkey(res_result_long, region_folder, country_code, alt)

res_result_compare <- merge(
  res_result_long,
  dict_main_alts,
  by = c("region_folder", "country_code", "alt"),
  all.x = TRUE
)

res_result_compare[, in_dict_main := !is.na(in_dict_main)]
res_result_compare[, status := fifelse(in_dict_main, "present_in_dict_main_json", "only_in_result")]
res_result_compare[, in_dict_main := NULL]

# add country label
country_labels <- unique(res[, .(region_folder, country_code, country_label)])
setkey(country_labels, region_folder, country_code)
setkey(res_result_compare, region_folder, country_code)
res_result_compare <- country_labels[res_result_compare]

# -------------------------
# DELETIONS (DICT + RESULT)
# -------------------------

# DICT deletions: only those not referenced in main json
to_delete_dict <- unique(res_long[status == "only_in_alt_folder", .(region_folder, country_code, alt)])

cat("\n--- DICT deletions ---\n")
if (nrow(to_delete_dict)) {
  # IMPORTANT: use region_folder AS-IS ("06" stays "06")
  to_delete_dict[, file_path := file.path(
    dict_dir,
    region_folder,
    "alt",
    paste0(country_code, "_", alt, ".json")
  )]
  
  to_delete_dict[, exists := file.exists(file_path)]
  del_dict_files <- to_delete_dict[exists == TRUE, file_path]
  
  cat("Candidates:", nrow(to_delete_dict), "\n")
  cat("Existing files:", length(del_dict_files), "\n")
  
  if (length(del_dict_files)) {
    if (dry_run) {
      cat("DRY RUN (no deletion). Example paths:\n")
      cat(paste(head(del_dict_files, 20), collapse = "\n"), "\n")
    } else {
      ok <- file.remove(del_dict_files)
      cat("Deleted:", sum(ok), " / Failed:", sum(!ok), "\n")
    }
  }
} else {
  cat("No files to delete.\n")
}

# RESULT deletions: any csv/json whose BASE alt is not in dict main json
to_delete_result <- unique(res_result_compare[status == "only_in_result", .(region_folder, country_code, alt)])

cat("\n--- RESULT deletions ---\n")
if (nrow(to_delete_result)) {
  del_result_dt <- rbindlist(lapply(unique(to_delete_result$region_folder), function(region) {
    region <- as.character(region)
    
    rdir_result <- file.path(result_dir, region)
    if (!dir.exists(rdir_result)) return(NULL)
    
    alt_dir <- file.path(rdir_result, "alt")
    scan_dir <- if (dir.exists(alt_dir)) alt_dir else rdir_result
    
    targets_region <- to_delete_result[as.character(region_folder) == region]
    if (!nrow(targets_region)) return(NULL)
    
    rbindlist(lapply(unique(targets_region$country_code), function(cc_num) {
      cc <- as.character(cc_num)
      
      files <- list.files(scan_dir, pattern = "\\.(json|csv)$", full.names = FALSE)
      files <- files[startsWith(files, paste0(cc, "_"))]
      if (!length(files)) return(NULL)
      
      parsed <- parse_result_file_alt(files, cc)
      if (!nrow(parsed)) return(NULL)
      
      targets_cc <- targets_region[country_code == cc_num, alt]
      parsed <- parsed[alt %in% targets_cc]
      if (!nrow(parsed)) return(NULL)
      
      parsed[, `:=`(
        region_folder = region,
        country_code  = cc_num,
        scan_dir      = scan_dir,
        file_path     = file.path(scan_dir, file)
      )]
      parsed
    }), fill = TRUE)
  }), fill = TRUE)
  
  cat("Candidates (alts):", nrow(to_delete_result), "\n")
  cat("Matching files found:", nrow(del_result_dt), "\n")
  
  if (nrow(del_result_dt)) {
    del_result_dt[, exists := file.exists(file_path)]
    del_files <- unique(del_result_dt[exists == TRUE, file_path])
    
    cat("Existing files:", length(del_files), "\n")
    
    if (length(del_files)) {
      if (dry_run) {
        cat("DRY RUN (no deletion). Example paths:\n")
        cat(paste(head(del_files, 50), collapse = "\n"), "\n")
      } else {
        ok <- file.remove(del_files)
        cat("Deleted:", sum(ok), " / Failed:", sum(!ok), "\n")
      }
    }
  }
} else {
  cat("No files to delete.\n")
}

cat("\nDONE. If you want to actually delete, set dry_run <- FALSE.\n")

# -------------------------
# altClean function
# -------------------------
altClean <- function(countrycode,
                     delete = FALSE,
                     dict_dir = "C:/Users/brisa/Documents/alt_code/dict",
                     result_dir = "C:/Users/brisa/Documents/alt_code/result") {
  
  info <- jsonclean(countrycode, dict_dir)  # prints jsonclean block
  
  cc <- as.character(countrycode)
  region_hit <- as.character(info$region_folder)
  main_path <- info$main_json_path
  alts_main <- info$alts_main
  
  # dict candidates
  dict_candidates <- data.table()
  if (length(info$only_in_folder)) {
    dict_candidates <- data.table(
      where = "dict",
      region_folder = region_hit,
      country_code = suppressWarnings(as.integer(cc)),
      alt = info$only_in_folder,
      file_path = file.path(info$alt_dir, paste0(cc, "_", info$only_in_folder, ".json"))
    )
    dict_candidates[, exists := file.exists(file_path)]
  }
  
  # result candidates
  result_candidates <- data.table()
  rdir_result <- file.path(result_dir, region_hit)
  if (dir.exists(rdir_result)) {
    alt_dir_result <- file.path(rdir_result, "alt")
    scan_dir <- if (dir.exists(alt_dir_result)) alt_dir_result else rdir_result
    
    files <- list.files(scan_dir, pattern = "\\.(json|csv)$", full.names = FALSE)
    files <- files[startsWith(files, paste0(cc, "_"))]
    
    parsed <- parse_result_file_alt(files, cc)
    if (nrow(parsed)) {
      parsed <- parsed[!(alt %in% alts_main)]
      if (nrow(parsed)) {
        parsed[, `:=`(
          where = "result",
          region_folder = region_hit,
          country_code = suppressWarnings(as.integer(cc)),
          file_path = file.path(scan_dir, file),
          exists = file.exists(file.path(scan_dir, file))
        )]
        result_candidates <- parsed[, .(where, region_folder, country_code, alt, file_path, exists)]
      }
    }
  }
  
  all_candidates <- rbindlist(
    list(
      dict_candidates[, .(where, region_folder, country_code, alt, file_path, exists)],
      result_candidates
    ),
    fill = TRUE
  )
  
  cat("\n============================\n")
  cat("altClean\n")
  cat("Country:", ifelse(is.na(info$country_label), cc, paste0(info$country_label, " (", cc, ")")), "\n")
  cat("Region folder:", region_hit, "\n")
  cat("Main JSON:", main_path, "\n")
  cat("Delete mode:", ifelse(isTRUE(delete), "YES (will delete)", "NO (dry run - list only)"), "\n")
  cat("============================\n\n")
  
  cat("DICT candidates:", nrow(dict_candidates), " | existing:", sum(dict_candidates$exists %in% TRUE), "\n")
  cat("RESULT candidates:", nrow(result_candidates), " | existing:", sum(result_candidates$exists %in% TRUE), "\n")
  cat("TOTAL candidates:", nrow(all_candidates), " | existing:", sum(all_candidates$exists %in% TRUE), "\n\n")
  
  existing_files <- unique(all_candidates[exists == TRUE, file_path])
  
  if (!length(existing_files)) {
    cat("Nothing to delete.\n")
  } else if (!isTRUE(delete)) {
    cat("FILES THAT WOULD BE DELETED:\n")
    cat(paste(existing_files, collapse = "\n"), "\n")
  } else {
    ok <- file.remove(existing_files)
    cat("Deleted:", sum(ok), " / Failed:", sum(!ok), "\n")
    if (any(!ok)) {
      cat("Failed paths:\n")
      cat(paste(existing_files[!ok], collapse = "\n"), "\n")
    }
  }
  
  invisible(list(
    country_code = suppressWarnings(as.integer(cc)),
    country_label = info$country_label,
    region_folder = region_hit,
    main_json_path = main_path,
    alts_main = alts_main,
    jsonclean = info,
    dict_to_delete = dict_candidates,
    result_to_delete = result_candidates,
    to_delete = all_candidates
  ))
}

