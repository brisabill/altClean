#Maping alts in the the dict folder and comparing to al folder both in dict and in results
library(jsonlite)
library(purrr)
library(stringr)
library(data.table)

dict_dir <- "C:/Users/brisa/Documents/alt_code/dict"  # <- change if needed

safe_fromJSON <- function(path) {
  tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
}

# Extract alt in the main .json files
extract_alts_from_main <- function(j) {
  if (is.null(j)) return(character(0))
  out <- character(0)
  
  walk_rec <- function(x) {
    if (!is.list(x)) return()
    
    nms <- names(x)
    if (!is.null(nms) && "alt" %in% nms) {
      v <- x[["alt"]]
      
      # alt can be string OR list
      v <- unlist(v, recursive = TRUE, use.names = FALSE)
      v <- as.character(v)
      v <- str_trim(v)
      v <- v[!is.na(v) & v != ""]
      
      out <<- c(out, v)
    }
    
    purrr::walk(x, walk_rec)
  }
  
  walk_rec(j)
  unique(out)
}

# List alt JSON files for a country, drops "xx_" prefix and ".json"; cleaned text

list_alt_files_for_country <- function(alt_dir, country_code) {
  if (!dir.exists(alt_dir)) return(character(0))
  
  cc <- as.character(country_code)
  files <- list.files(alt_dir, pattern = "\\.json$", full.names = FALSE)
  
  # keep ONLY files that belong to this same country code (cc)
  files <- files[startsWith(files, paste0(cc, "_")) | files == paste0(cc, ".json")]
  
  stems <- str_remove(files, "\\.json$")
  stems <- str_remove(stems, paste0("^", cc, "_"))  #removes the xx_ country code from the start of the file name
  stems <- stems[!is.na(stems) & stems != ""]
  unique(stems)
}

# gets regions of folders as numeric
region_dirs <- list.dirs(dict_dir, recursive = FALSE, full.names = TRUE)
region_dirs <- region_dirs[str_detect(basename(region_dirs), "^\\d+$")]

# collects the extracted info into a datatable
res_list <- lapply(region_dirs, function(rdir) {
  region  <- basename(rdir)
  alt_dir <- file.path(rdir, "alt")
  
  main_jsons <- list.files(rdir, pattern = "\\.json$", full.names = TRUE)
  
  rbindlist(lapply(main_jsons, function(f) {
    cc <- str_remove(basename(f), "\\.json$")
    j  <- safe_fromJSON(f)
    
    country_label <- if (!is.null(j) && !is.null(j$country_label)) j$country_label else NA_character_
    
    # collects info into vectors
    alts_main_vec     <- extract_alts_from_main(j)
    alts_folder_vec   <- list_alt_files_for_country(alt_dir, cc)
    only_in_main_vec  <- setdiff(alts_main_vec, alts_folder_vec)
    only_in_fold_vec  <- setdiff(alts_folder_vec, alts_main_vec)
    
    data.table(
      region_folder = region,
      country_code  = suppressWarnings(as.integer(cc)),
      country_label = country_label,
      
      alts_in_main        = paste(sort(alts_main_vec), collapse = ", "),
      alt_files           = paste(sort(alts_folder_vec), collapse = ", "),
      only_in_main        = paste(sort(only_in_main_vec), collapse = ", "),
      only_in_alt_folder  = paste(sort(only_in_fold_vec), collapse = ", "),
      
      n_alts_in_main        = length(alts_main_vec),
      n_alt_files           = length(alts_folder_vec),
      n_only_in_main        = length(only_in_main_vec),
      n_only_in_alt_folder  = length(only_in_fold_vec)
    )
  }), fill = TRUE)
})

res <- rbindlist(res_list, fill = TRUE)

# save and export in .csv
View(res)
fwrite(res, file.path(dict_dir, "alt_mapping_comparison.csv"))

# Now create the table in the long format; one alt per row
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
    
    data.table(
      region_folder = region,
      country_code  = suppressWarnings(as.integer(cc)),
      country_label = country_label,
      alt = all_alts,
      in_main   = all_alts %in% alts_main_vec,
      in_folder = all_alts %in% alts_folder_vec
    )[, status := fifelse(in_main & in_folder, "both",
                          fifelse(in_main & !in_folder, "only_in_main",
                                  fifelse(!in_main & in_folder, "only_in_alt_folder", "neither")))]
  }), fill = TRUE)
}), fill = TRUE)

View(res_long)

# -------------------------
# code to call the country and get files info
# -------------------------
jsonclean <- function(countrynumber, dict_dir) {
  cc <- as.character(countrynumber)
  
  region_dirs <- list.dirs(dict_dir, recursive = FALSE, full.names = TRUE)
  region_dirs <- region_dirs[str_detect(basename(region_dirs), "^\\d+$")]
  
  main_path <- NULL
  region_hit <- NULL
  for (rdir in region_dirs) {
    candidate <- file.path(rdir, paste0(cc, ".json"))
    if (file.exists(candidate)) {
      main_path <- candidate
      region_hit <- basename(rdir)
      break
    }
  }
  if (is.null(main_path)) stop("Could not find main JSON for country ", cc)
  
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
    country_code = as.integer(countrynumber),
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

# Example:
#you have to list from which directory you wasnt to search
jsonclean(8, dict_dir)

#--------------------------------
#Mapping alts in the results folder
#--------------------------------

# ============================================================
#NEXT: Mapping the alt's in the results folder
# ============================================================

result_dir <- "C:/Users/brisa/Documents/alt_code/result"  # <- change if needed

# Extracts and cleans the alt file name so that it matches the dict folder; drops the _i_ or _m_
extract_result_alts_for_country <- function(region_dir_result, country_code) {
  cc <- as.character(country_code)
  
  alt_dir <- file.path(region_dir_result, "alt")
  scan_dir <- if (dir.exists(alt_dir)) alt_dir else region_dir_result
  if (!dir.exists(scan_dir)) return(data.table())
  
  files <- list.files(scan_dir, pattern = "\\.json$", full.names = FALSE)
  
  # makes sure it stops reading the country code at the "_"
  files <- files[startsWith(files, paste0(cc, "_"))]
  if (!length(files)) return(data.table())
  
  dt <- data.table(file = files)
  dt[, file_noext := str_remove(file, "\\.json$")]
  
  # keep only incidence/mortality files (must contain _i_ or _m_)
  dt[, type := fifelse(str_detect(file_noext, "_i_"), "i",
                       fifelse(str_detect(file_noext, "_m_"), "m", NA_character_))]
  dt <- dt[!is.na(type)]
  
  # remove cc_ prefix
  dt[, after_cc := str_remove(file_noext, paste0("^", cc, "_"))]
  
  # clean alt name = everything before _i_ or _m_
  dt[, alt := str_replace(after_cc, "(_[im]_.*)$", "")]
  dt[, alt := str_trim(alt)]
  dt <- dt[alt != "" & !is.na(alt)]
  
  unique(dt[, .(alt, type)])
}

# Build res_result_long aligned with regions and country code
res_result_long <- rbindlist(lapply(region_dirs, function(rdir_dict) {
  region <- basename(rdir_dict)
  
  # country codes come from dict region main json files
  main_jsons <- list.files(rdir_dict, pattern = "^\\d+\\.json$", full.names = TRUE)
  
  # corresponding region folder in result
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

View(res_result_long)

# Compare RESULT alts to DICT MAIN JSON alts (from your existing res_long)
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

# adds country label to the existing table
country_labels <- unique(res[, .(region_folder, country_code, country_label)])
setkey(country_labels, region_folder, country_code)
setkey(res_result_compare, region_folder, country_code)
res_result_compare <- country_labels[res_result_compare]

View(res_result_compare)

#-------------------------------
# code to delete files based on what was extracted
#---------------------------------
dry_run <- TRUE  # <- set to FALSE to actually delete

to_delete_dict <- unique(res_long[status == "only_in_alt_folder",
                                  .(region_folder, country_code, alt)])

if (nrow(to_delete_dict)) {
  to_delete_dict[, file_path := file.path(
    dict_dir,
    sprintf("%02d", as.integer(region_folder)),   
    "alt",
    paste0(country_code, "_", alt, ".json")
  )]
  
  # keep only existing files
  to_delete_dict[, exists := file.exists(file_path)]
  del_dict_files <- to_delete_dict[exists == TRUE, file_path]
  
  cat("\n--- DICT deletions ---\n")
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
  cat("\n--- DICT deletions ---\nNo files to delete.\n")
}

# deletes all the .json files included in the previous search

to_delete_result <- unique(res_result_compare[status == "only_in_result",
                                              .(region_folder, country_code, alt)])

if (nrow(to_delete_result)) {
  
  
  parse_result_file_alt <- function(files, cc) {
    # only .json files will be deleted
    file_noext <- str_remove(files, "\\.json$")
    # keep only _i_/_m_
    type <- fifelse(str_detect(file_noext, "_i_"), "i",
                    fifelse(str_detect(file_noext, "_m_"), "m", NA_character_))
    keep <- !is.na(type)
    if (!any(keep)) return(data.table())
    
    file_noext <- file_noext[keep]
    files <- files[keep]
    type <- type[keep]
    
    after_cc <- str_remove(file_noext, paste0("^", cc, "_"))
    alt_clean <- str_replace(after_cc, "(_[im]_.*)$", "")
    alt_clean <- str_trim(alt_clean)
    
    data.table(file = files, alt = alt_clean, type = type)
  }
  
  # scan and build file list that are going to be deleted
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
      
      files <- list.files(scan_dir, pattern = "\\.json$", full.names = FALSE)
      files <- files[startsWith(files, paste0(cc, "_"))]
      if (!length(files)) return(NULL)
      
      parsed <- parse_result_file_alt(files, cc)
      if (!nrow(parsed)) return(NULL)
      
      # keep only files whose alt matches one of the alts in the main.json file
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
  
  cat("\n--- RESULT deletions ---\n")
  cat("Candidates (alts):", nrow(to_delete_result), "\n")
  cat("Matching files found:", nrow(del_result_dt), "\n")
  
  if (nrow(del_result_dt)) {
    del_result_dt[, exists := file.exists(file_path)]
    del_files <- del_result_dt[exists == TRUE, file_path]
    
    cat("Existing files:", length(del_files), "\n")
    
    if (length(del_files)) {
      if (dry_run) {
        cat("DRY RUN (no deletion). Example paths:\n")
        cat(paste(head(del_files, 20), collapse = "\n"), "\n")
      } else {
        ok <- file.remove(del_files)
        cat("Deleted:", sum(ok), " / Failed:", sum(!ok), "\n")
      }
    }
  }
} else {
  cat("\n--- RESULT deletions ---\nNo files to delete.\n")
}

cat("\nDONE. If you want to actually delete, set dry_run <- FALSE.\n")


####Creates cleaning/deleting function
altClean <- function(countrycode,
                     delete = FALSE,
                     dict_dir = "C:/Users/brisa/Documents/alt_code/dict",
                     result_dir = "C:/Users/brisa/Documents/alt_code/result") {
  
  # -------------------------
  # local helper: jsonclean (prints exactly like yours)
  # -------------------------
  jsonclean_local <- function(countrynumber, dict_dir) {
    cc <- as.character(countrynumber)
    
    region_dirs <- list.dirs(dict_dir, recursive = FALSE, full.names = TRUE)
    region_dirs <- region_dirs[stringr::str_detect(basename(region_dirs), "^\\d+$")]
    
    main_path <- NULL
    region_hit <- NULL
    for (rdir in region_dirs) {
      candidate <- file.path(rdir, paste0(cc, ".json"))
      if (file.exists(candidate)) {
        main_path <- candidate
        region_hit <- basename(rdir)
        break
      }
    }
    if (is.null(main_path)) stop("Could not find main JSON for country ", cc)
    
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
  # local helper: parse result filenames into alt + type
  # -------------------------
  parse_result_files <- function(files, cc) {
    if (!length(files)) return(data.table::data.table())
    file_noext <- stringr::str_remove(files, "\\.json$")
    
    type <- data.table::fifelse(stringr::str_detect(file_noext, "_i_"), "i",
                                data.table::fifelse(stringr::str_detect(file_noext, "_m_"), "m", NA_character_))
    keep <- !is.na(type)
    if (!any(keep)) return(data.table::data.table())
    
    files <- files[keep]
    file_noext <- file_noext[keep]
    type <- type[keep]
    
    after_cc <- stringr::str_remove(file_noext, paste0("^", cc, "_"))
    alt_clean <- stringr::str_replace(after_cc, "(_[im]_.*)$", "")
    alt_clean <- stringr::str_trim(alt_clean)
    alt_clean <- alt_clean[!is.na(alt_clean) & alt_clean != ""]
    
    data.table::data.table(file = files, alt = alt_clean, type = type)
  }
  
  # -------------------------
  # 1) run jsonclean INSIDE altClean to print the console block
  # -------------------------
  info <- jsonclean_local(countrycode, dict_dir)  # prints like jsonclean()
  
  cc <- as.character(countrycode)
  region_hit <- as.character(info$region_folder)
  main_path <- info$main_json_path
  alts_main <- info$alts_main
  
  # -------------------------
  # 2) build deletion candidates (dict + result), based on "NOT in main json"
  # -------------------------
  # dict candidates = only_in_folder
  dict_candidates <- data.table::data.table()
  if (length(info$only_in_folder)) {
    dict_candidates <- data.table::data.table(
      where = "dict",
      region_folder = region_hit,
      country_code = suppressWarnings(as.integer(cc)),
      alt = info$only_in_folder,
      file_path = file.path(info$alt_dir, paste0(cc, "_", info$only_in_folder, ".json"))
    )
    dict_candidates[, exists := file.exists(file_path)]
  }
  
  # result candidates = parsed files whose alt NOT in main json
  result_candidates <- data.table::data.table()
  rdir_result <- file.path(result_dir, region_hit)
  if (dir.exists(rdir_result)) {
    alt_dir_result <- file.path(rdir_result, "alt")
    scan_dir <- if (dir.exists(alt_dir_result)) alt_dir_result else rdir_result
    
    files <- list.files(scan_dir, pattern = "\\.json$", full.names = FALSE)
    files <- files[startsWith(files, paste0(cc, "_"))]
    
    parsed <- parse_result_files(files, cc)
    if (nrow(parsed)) {
      parsed <- parsed[!(alt %in% alts_main)]
      if (nrow(parsed)) {
        parsed[, `:=`(
          where = "result",
          region_folder = region_hit,
          country_code = suppressWarnings(as.integer(cc)),
          file_path = file.path(scan_dir, file)
        )]
        parsed[, exists := file.exists(file_path)]
        result_candidates <- parsed[, .(where, region_folder, country_code, alt, type, file_path, exists)]
      }
    }
  }
  
  all_candidates <- data.table::rbindlist(
    list(
      dict_candidates[, .(where, region_folder, country_code, alt, type = NA_character_, file_path, exists)],
      result_candidates
    ),
    fill = TRUE
  )
  
  # -------------------------
  # 3) altClean header + delete/list
  # -------------------------
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
  
  existing_files <- all_candidates[exists == TRUE, file_path]
  
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

# Examples:
altClean(300, delete = FALSE)  # prints jsonclean block + altClean block, lists files
altClean(300, delete = TRUE)   # prints blocks, then deletes
