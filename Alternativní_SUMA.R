  # install.packages(c("readr", "dplyr"))
  # install.packages("openxlsx")  # volitelné
  
  suppressPackageStartupMessages({
    library(readr)
    library(dplyr)
  })
  
  # ============================================================
  # NASTAVENÍ
  # ============================================================
  INPUT_CSV   <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/REG_MOD_X20/LM_X20_pixels.csv"
  OUTPUT_CSV  <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/REG_MOD_X20/MOD_summary_weighted_p995.csv"
  OUTPUT_XLSX <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/REG_MOD_X20/MOD_summary_weighted_p995.xlsx"
  
  DIAG_CSV    <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/REG_MOD_X20/prediction_limits_diagnostic_p995.csv"
  DIAG_XLSX   <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/REG_MOD_X20/prediction_limits_diagnostic_p995.xlsx"
  
  PIXEL_AREA_COL <- "Shape_Area"
  
  LOWER_BOUND <- 0
  UPPER_PROB  <- 0.995   # 99.5. percentil
  
  # ============================================================
  # FUNKCE
  # ============================================================
  to_num <- function(x) {
    if (is.numeric(x)) return(x)
    x <- trimws(as.character(x))
    x[x %in% c("", "NA", "NaN", "NULL")] <- NA
    x <- gsub("\u00A0", "", x, fixed = TRUE)
    x <- gsub(" ", "", x, fixed = TRUE)
    x <- gsub(",", ".", x, fixed = TRUE)
    suppressWarnings(as.numeric(x))
  }
  
  wmean_safe <- function(x, w) {
    ok <- is.finite(x) & !is.na(x) & is.finite(w) & !is.na(w) & w > 0
    if (!any(ok)) return(NA_real_)
    sum(x[ok] * w[ok], na.rm = TRUE) / sum(w[ok], na.rm = TRUE)
  }
  
  clean_names <- function(nm) {
    nm <- gsub('^"|"$', '', nm)
    nm <- gsub("^\ufeff", "", nm)
    nm <- gsub("^ï»¿", "", nm)
    nm <- gsub("^ď»ż", "", nm)
    nm <- gsub('"', "", nm, fixed = TRUE)
    nm
  }
  
  has_any_finite <- function(data, cols) {
    if (length(cols) == 0) return(rep(FALSE, nrow(data)))
    mat <- as.matrix(data[, cols, drop = FALSE])
    rowSums(is.finite(mat) & !is.na(mat)) > 0
  }
  
  clip_by_bounds <- function(x, lower = 0, upper_prob = 0.995) {
    x_num <- to_num(x)
    x_out <- x_num
    x_out[!is.finite(x_out)] <- NA_real_
    
    idx_lower <- !is.na(x_out) & x_out < lower
    n_lower <- sum(idx_lower, na.rm = TRUE)
    x_out[idx_lower] <- NA_real_
    
    ref <- x_out[!is.na(x_out) & is.finite(x_out)]
    
    if (length(ref) == 0) {
      return(list(
        x = x_out,
        cap = NA_real_,
        n_valid_before = sum(!is.na(x_num) & is.finite(x_num)),
        n_below_lower_removed = n_lower,
        n_above_upper_clipped = 0L,
        n_valid_after = 0L
      ))
    }
    
    cap <- as.numeric(quantile(ref, probs = upper_prob, na.rm = TRUE, type = 7))
    
    idx_upper <- !is.na(x_out) & is.finite(x_out) & x_out > cap
    n_upper <- sum(idx_upper, na.rm = TRUE)
    x_out[idx_upper] <- cap
    
    list(
      x = x_out,
      cap = cap,
      n_valid_before = sum(!is.na(x_num) & is.finite(x_num)),
      n_below_lower_removed = n_lower,
      n_above_upper_clipped = n_upper,
      n_valid_after = sum(!is.na(x_out) & is.finite(x_out))
    )
  }
  
  calc_one_mod <- function(mod_name, data, pixel_area_col, height_cols, sum_cols, pred_cols_any) {
    share <- data[[mod_name]]
    pixel_area_m2 <- data[[pixel_area_col]]
    
    area_in_mod_m2 <- pixel_area_m2 * share
    area_in_mod_ha <- area_in_mod_m2 / 10000
    
    ok_area <- is.finite(share) & !is.na(share) & share > 0 &
      is.finite(pixel_area_m2) & !is.na(pixel_area_m2) & pixel_area_m2 > 0 &
      is.finite(area_in_mod_m2) & !is.na(area_in_mod_m2) & area_in_mod_m2 > 0
    
    ok_pred <- ok_area & data$has_pred_any
    ok_nopred <- ok_area & !data$has_pred_any
    
    area_total_m2 <- sum(area_in_mod_m2[ok_area], na.rm = TRUE)
    area_total_ha <- sum(area_in_mod_ha[ok_area], na.rm = TRUE)
    
    area_pred_m2 <- sum(area_in_mod_m2[ok_pred], na.rm = TRUE)
    area_pred_ha <- sum(area_in_mod_ha[ok_pred], na.rm = TRUE)
    area_nopred_m2 <- sum(area_in_mod_m2[ok_nopred], na.rm = TRUE)
    area_nopred_ha <- sum(area_in_mod_ha[ok_nopred], na.rm = TRUE)
    
    out <- list(
      MOD = mod_name,
      n_parts = sum(ok_area, na.rm = TRUE),
      area_m2 = area_total_m2,
      area_ha = area_total_ha,
      area_pred_m2 = area_pred_m2,
      area_pred_ha = area_pred_ha,
      area_nopred_m2 = area_nopred_m2,
      area_nopred_ha = area_nopred_ha,
      pred_share = ifelse(area_total_m2 > 0, area_pred_m2 / area_total_m2, NA_real_)
    )
    
    # výškové veličiny = vážený průměr
    for (col in height_cols) {
      out[[col]] <- wmean_safe(data[[col]], area_in_mod_m2)
    }
    
    # G, V, N = přepočet na dílčí plochu v ha a následná suma
    for (col in sum_cols) {
      ok <- ok_area & is.finite(data[[col]]) & !is.na(data[[col]])
      out[[col]] <- if (any(ok)) {
        sum(data[[col]][ok] * area_in_mod_ha[ok], na.rm = TRUE)
      } else {
        NA_real_
      }
    }
    
    as.data.frame(out, check.names = FALSE)
  }
  
  # ============================================================
  # 1) NAČTENÍ DAT
  # ============================================================
  df <- read_delim(
    file = INPUT_CSV,
    delim = ";",
    locale = locale(decimal_mark = ","),
    show_col_types = FALSE,
    guess_max = 100000
  )
  
  names(df) <- clean_names(names(df))
  
  if (!(PIXEL_AREA_COL %in% names(df))) {
    stop("Chybí sloupec: ", PIXEL_AREA_COL)
  }
  
  # ============================================================
  # 2) DEFINICE SLOUPCŮ PRO TVŮJ SOUBOR
  # ============================================================
  mod_cols <- grep("^MOD_([0-9]+|NA)$", names(df), value = TRUE)
  
  height_cols <- intersect(c("H_DOM", "H_LOREY"), names(df))
  sum_cols    <- intersect(c("G_per_ha", "V_per_ha", "N_per_ha"), names(df))
  pred_cols_any <- unique(c(height_cols, sum_cols))
  
  if (length(mod_cols) == 0) {
    stop("Nebyly nalezeny žádné sloupce MOD_*.")
  }
  
  if (length(pred_cols_any) == 0) {
    stop("Nebyly nalezeny žádné predikční sloupce (H_DOM, H_LOREY, G_per_ha, V_per_ha, N_per_ha).")
  }
  
  # numerický převod
  num_cols <- unique(c(PIXEL_AREA_COL, mod_cols, pred_cols_any))
  df[num_cols] <- lapply(df[num_cols], to_num)
  
  cat("Nalezené MOD sloupce:\n")
  print(mod_cols)
  cat("\n")
  
  cat("Výškové sloupce:\n")
  print(height_cols)
  cat("\n")
  
  cat("Sumované sloupce:\n")
  print(sum_cols)
  cat("\n")
  
  # ============================================================
  # 3) OMEZENÍ HODNOT: <0 -> NA ; >p99.5 -> cap
  # ============================================================
  diag_list <- list()
  
  for (col in pred_cols_any) {
    clipped <- clip_by_bounds(df[[col]], lower = LOWER_BOUND, upper_prob = UPPER_PROB)
    df[[col]] <- clipped$x
    
    diag_list[[col]] <- data.frame(
      column = col,
      lower_bound = LOWER_BOUND,
      upper_prob = UPPER_PROB,
      upper_percentile_value = clipped$cap,
      n_valid_before = clipped$n_valid_before,
      n_below_lower_removed = clipped$n_below_lower_removed,
      n_above_upper_clipped = clipped$n_above_upper_clipped,
      n_valid_after = clipped$n_valid_after,
      stringsAsFactors = FALSE
    )
  }
  
  diag_tbl <- bind_rows(diag_list)
  
  cat("Diagnostika omezení:\n")
  print(diag_tbl)
  cat("\n")
  
  # po omezení hodnot znovu označíme řádky, kde existuje aspoň jedna predikce
  df$has_pred_any <- has_any_finite(df, pred_cols_any)
  
  # ============================================================
  # 4) AGREGACE PRO VŠECHNY MOD
  # ============================================================
  result_list <- lapply(
    mod_cols,
    calc_one_mod,
    data = df,
    pixel_area_col = PIXEL_AREA_COL,
    height_cols = height_cols,
    sum_cols = sum_cols,
    pred_cols_any = pred_cols_any
  )
  
  result <- bind_rows(result_list)
  
  # seřazení MOD
  result <- result %>%
    mutate(
      MOD_order = case_when(
        MOD == "MOD_NA" ~ 999,
        grepl("^MOD_[0-9]+$", MOD) ~ as.numeric(sub("^MOD_", "", MOD)),
        TRUE ~ 1000
      )
    ) %>%
    arrange(MOD_order, MOD) %>%
    select(-MOD_order)
  
  # ============================================================
  # 5) ULOŽENÍ
  # ============================================================
  dir.create(dirname(OUTPUT_CSV), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(DIAG_CSV), recursive = TRUE, showWarnings = FALSE)
  
  write.csv2(result, OUTPUT_CSV, row.names = FALSE)
  write.csv2(diag_tbl, DIAG_CSV, row.names = FALSE)
  
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    openxlsx::write.xlsx(result, OUTPUT_XLSX, overwrite = TRUE)
    openxlsx::write.xlsx(diag_tbl, DIAG_XLSX, overwrite = TRUE)
  }
  
  # ============================================================
  # 6) KONTROLA
  # ============================================================
  cat("Počet řádků výsledku:", nrow(result), "\n\n")
  print(result)
  
  cat("\nDiagnostická tabulka limitů:\n")
  print(diag_tbl)
  
  cat("\nUloženo do:\n")
  cat("OUTPUT_CSV: ", OUTPUT_CSV, "\n", sep = "")
  cat("DIAG_CSV:   ", DIAG_CSV, "\n", sep = "")