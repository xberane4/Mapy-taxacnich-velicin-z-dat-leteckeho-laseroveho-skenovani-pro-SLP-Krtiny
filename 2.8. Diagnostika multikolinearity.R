# ============================================================
# ÚČEL SKRIPTU
# ============================================================
# Skript slouží k diagnostice multikolinearity u finálně vybraných
# regresních modelů. Pro každý model uvedený ve vstupní tabulce
# vybere odpovídající podmnožinu zkusných ploch podle dřevinné skupiny
# a výškového intervalu, následně vypočítá základní ukazatele
# multikolinearity a numerické stability modelu. Výstupem je rozšířená
# tabulka modelů doplněná o souhrnné charakteristiky a samostatná
# tabulka s hodnotami VIF pro jednotlivé prediktory.
# ============================================================

# ============================================================
# DIAGNOSTIKA MULTIKOLINEARITY PRO VYBRANÉ MODELY
# (Vyber_modelu_PIL.xlsx)
# + data ze zkusných ploch (sample_plots_REPLACED_BY_PIL.csv)
#
# Pro každý řádek tabulky modelů jsou dopočítány:
# - faktor inflace rozptylu (VIF; ruční výpočet bez balíčku car)
#   včetně tolerance,
# - číslo podmíněnosti (kappa) odvozené z model.matrix,
# - korelace mezi prediktory (maximální a medián absolutních hodnot),
# - informace o případné singulárnosti modelu
#   (rank deficiency / alias).
#
# DŮLEŽITÉ:
# - intervaly jsou filtrovány podle proměnné definované v COL_HEIGHT_VAR
#   (např. z_p95),
# - proměnná Hclass může mít např. tyto tvary:
#     A_z_p95_[0;17.117)
#     A_z_p95_[0;17,117)
#     A_z_p95_[30;+)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(stringr)
  library(purrr)
  library(tibble)
})

# Balíček writexl je volitelný; pokud není dostupný, výstup bude uložen ve formátu CSV.
has_writexl <- requireNamespace("writexl", quietly = TRUE)

# ------------------------------------------------------------
# 0) Nastavení vstupních a výstupních cest
# ------------------------------------------------------------
DATA_PATH   <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/REG_MOD_PIL/sample_plots_REPLACED_BY_PIL.csv"
MODELS_PATH <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/LIN_REG_MOD_PIL/Vyber_modelu_PIL.xlsx"

OUT_DIR <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/REG_MOD_PIL/multicollinearity_check_final_balik"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

OUT_MODELS_ENRICHED_XLSX <- file.path(OUT_DIR, "models_with_collinearity.xlsx")
OUT_VIF_LONG_XLSX        <- file.path(OUT_DIR, "vif_by_predictor.xlsx")
OUT_MODELS_ENRICHED_CSV  <- file.path(OUT_DIR, "models_with_collinearity.csv")
OUT_VIF_LONG_CSV         <- file.path(OUT_DIR, "vif_by_predictor.csv")

# Názvy sloupců v tabulce modelů
COL_TARGET     <- "target"
COL_GROUP      <- "skupina"
COL_HCLASS     <- "Hclass"       # např. "A_z_p95_[0;17.117)"
COL_PREDICTORS <- "predictors"   # např. "bin_14_16, bin_34_36, bin_38_40"

# Název proměnné v datové tabulce, podle které je filtrován výškový interval
COL_HEIGHT_VAR <- "z_p95"

# Regulární výraz pro rozdělení seznamu prediktorů oddělených čárkou
PRED_SEP_REGEX <- "\\s*,\\s*"

# Targetové proměnné, u nichž byly při konstrukci modelů vyloučeny nulové hodnoty
NONZERO_TARGETS <- c("G_per_ha", "V_per_ha", "N_per_ha", "H_LOREY", "H_DOM")

# Minimální počet úplných pozorování po filtraci
MIN_N_COMPLETE <- 5

# ------------------------------------------------------------
# 1) Načtení vstupních dat
# ------------------------------------------------------------
df_data   <- read_csv(DATA_PATH, show_col_types = FALSE)
df_models <- readxl::read_excel(MODELS_PATH)

stopifnot(all(c(COL_TARGET, COL_GROUP, COL_HCLASS, COL_PREDICTORS) %in% names(df_models)))
stopifnot(all(c(COL_GROUP, COL_HEIGHT_VAR) %in% names(df_data)))

# ------------------------------------------------------------
# 2) Pomocné funkce
# ------------------------------------------------------------

# Funkce pro rozdělení textového seznamu prediktorů do vektoru názvů proměnných
parse_predictors <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  if (is.na(x) || x == "") return(character(0))
  str_split(x, pattern = PRED_SEP_REGEX, simplify = FALSE)[[1]] |>
    str_trim() |>
    discard(~ .x == "")
}

# Robustní převod textové hodnoty na numerický formát
# - akceptuje desetinnou tečku i čárku,
# - ignoruje okolní mezery.
to_num <- function(x) {
  x <- str_trim(as.character(x))
  x <- str_replace_all(x, ",", ".")
  suppressWarnings(as.numeric(x))
}

# Funkce pro parsování hranic intervalu z textu Hclass
# Podporované formáty:
# - [low;high)
# - [low;+)
# Hranice mohou být desetinné, s tečkou i čárkou, včetně záporných hodnot.
parse_hclass_bounds <- function(hclass) {
  hclass <- as.character(hclass)
  
  # Obecný vzor pro číselnou hodnotu:
  # celé číslo nebo desetinné číslo s tečkou či čárkou
  num_pat <- "(-?[0-9]+(?:[\\.,][0-9]+)?)"
  
  # Varianta 1: [low;high)
  m1 <- str_match(hclass, paste0("\\[", num_pat, "\\s*;\\s*", num_pat, "\\s*\\)"))
  if (!is.na(m1[1, 1])) {
    return(list(low = to_num(m1[1, 2]), high = to_num(m1[1, 3])))
  }
  
  # Varianta 2: [low;+)
  m2 <- str_match(hclass, paste0("\\[", num_pat, "\\s*;\\s*\\+\\)"))
  if (!is.na(m2[1, 1])) {
    return(list(low = to_num(m2[1, 2]), high = Inf))
  }
  
  list(low = NA_real_, high = NA_real_)
}

# Souhrn korelací mezi prediktory:
# - maximální absolutní korelace,
# - medián absolutních korelací.
pairwise_cor_summary <- function(X) {
  if (ncol(X) < 2) {
    return(tibble(cor_max_abs = NA_real_, cor_median_abs = NA_real_))
  }
  
  # Odstranění konstantních prediktorů
  keep <- map_lgl(as.data.frame(X), ~ is.finite(sd(.x, na.rm = TRUE)) && sd(.x, na.rm = TRUE) > 0)
  X2 <- X[, keep, drop = FALSE]
  if (ncol(X2) < 2) {
    return(tibble(cor_max_abs = NA_real_, cor_median_abs = NA_real_))
  }
  
  C <- suppressWarnings(cor(X2, use = "pairwise.complete.obs"))
  vals <- abs(C[upper.tri(C)])
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0) {
    tibble(cor_max_abs = NA_real_, cor_median_abs = NA_real_)
  } else {
    tibble(cor_max_abs = max(vals), cor_median_abs = median(vals))
  }
}

# Bezpečný výpočet čísla podmíněnosti
safe_kappa <- function(mm) {
  tryCatch(as.numeric(kappa(mm, exact = TRUE)), error = function(e) NA_real_)
}

# Identifikace případné singulárnosti modelu
rank_deficiency_flag <- function(fit) {
  ali <- tryCatch(alias(fit)$Complete, error = function(e) NULL)
  !is.null(ali) && nrow(ali) > 0
}

# ------------------------------------------------------------
# Výpočet VIF bez balíčku car (na bázi funkcí base R)
#
# VIF_j = 1 / (1 - R^2),
# kde R^2 pochází z pomocné regrese Xj ~ ostatní prediktory.
# ------------------------------------------------------------
vif_manual <- function(df_X) {
  p <- ncol(df_X)
  
  if (p == 0) {
    return(tibble(predictor = character(0), vif = numeric(0)))
  }
  
  if (p < 2) {
    return(tibble(predictor = colnames(df_X), vif = NA_real_))
  }
  
  map_dfr(seq_len(p), function(j) {
    y <- df_X[[j]]
    X <- df_X[, -j, drop = FALSE]
    
    if (!is.finite(sd(y, na.rm = TRUE)) || sd(y, na.rm = TRUE) == 0) {
      return(tibble(predictor = colnames(df_X)[j], vif = Inf))
    }
    
    fit <- tryCatch(lm(y ~ ., data = as.data.frame(X)), error = function(e) NULL)
    if (is.null(fit)) {
      return(tibble(predictor = colnames(df_X)[j], vif = NA_real_))
    }
    
    r2 <- summary(fit)$r.squared
    if (!is.finite(r2)) return(tibble(predictor = colnames(df_X)[j], vif = NA_real_))
    tibble(predictor = colnames(df_X)[j], vif = 1 / (1 - r2))
  })
}

# ------------------------------------------------------------
# 3) Výpočet diagnostik pro jeden řádek tabulky modelů
# ------------------------------------------------------------
compute_for_row <- function(row, df_data) {
  target <- row[[COL_TARGET]]
  group  <- row[[COL_GROUP]]
  hclass <- row[[COL_HCLASS]]
  preds  <- parse_predictors(row[[COL_PREDICTORS]])
  
  # --- kontrola existence targetové proměnné
  if (!target %in% names(df_data)) {
    return(list(
      summary = tibble(
        collin_ok = FALSE,
        collin_note = paste0("target not in data: ", target),
        n_used = NA_integer_, p_used = length(preds),
        vif_max = NA_real_, vif_mean = NA_real_, vif_median = NA_real_,
        tol_min = NA_real_,
        n_vif_gt5 = NA_integer_, n_vif_gt10 = NA_integer_,
        kappa = NA_real_,
        cor_max_abs = NA_real_, cor_median_abs = NA_real_,
        rank_deficient = NA
      ),
      vif_long = tibble()
    ))
  }
  
  if (length(preds) == 0) {
    return(list(
      summary = tibble(
        collin_ok = FALSE,
        collin_note = "no predictors",
        n_used = NA_integer_, p_used = 0L,
        vif_max = NA_real_, vif_mean = NA_real_, vif_median = NA_real_,
        tol_min = NA_real_,
        n_vif_gt5 = NA_integer_, n_vif_gt10 = NA_integer_,
        kappa = NA_real_,
        cor_max_abs = NA_real_, cor_median_abs = NA_real_,
        rank_deficient = NA
      ),
      vif_long = tibble()
    ))
  }
  
  missing_preds <- preds[!preds %in% names(df_data)]
  if (length(missing_preds) > 0) {
    return(list(
      summary = tibble(
        collin_ok = FALSE,
        collin_note = paste0("missing predictors in data: ", paste(missing_preds, collapse = ", ")),
        n_used = NA_integer_, p_used = length(preds),
        vif_max = NA_real_, vif_mean = NA_real_, vif_median = NA_real_,
        tol_min = NA_real_,
        n_vif_gt5 = NA_integer_, n_vif_gt10 = NA_integer_,
        kappa = NA_real_,
        cor_max_abs = NA_real_, cor_median_abs = NA_real_,
        rank_deficient = NA
      ),
      vif_long = tibble()
    ))
  }
  
  # --- parsování intervalu z proměnné Hclass
  b <- parse_hclass_bounds(hclass)
  if (is.na(b$low) || is.na(b$high)) {
    return(list(
      summary = tibble(
        collin_ok = FALSE,
        collin_note = paste0("cannot parse Hclass bounds: ", hclass),
        n_used = NA_integer_, p_used = length(preds),
        vif_max = NA_real_, vif_mean = NA_real_, vif_median = NA_real_,
        tol_min = NA_real_,
        n_vif_gt5 = NA_integer_, n_vif_gt10 = NA_integer_,
        kappa = NA_real_,
        cor_max_abs = NA_real_, cor_median_abs = NA_real_,
        rank_deficient = NA
      ),
      vif_long = tibble()
    ))
  }
  
  # --- filtrace dat podle skupiny a výškového intervalu
  d <- df_data %>%
    filter(.data[[COL_GROUP]] == group) %>%
    filter(.data[[COL_HEIGHT_VAR]] >= b$low) %>%
    { if (is.finite(b$high)) filter(., .data[[COL_HEIGHT_VAR]] < b$high) else . }
  
  # --- vytvoření modelové datové sady
  d2 <- d %>%
    select(all_of(c(target, preds))) %>%
    na.omit()
  
  if (target %in% NONZERO_TARGETS) {
    d2 <- d2 %>% filter(.data[[target]] != 0)
  }
  
  n_used <- nrow(d2)
  if (n_used < max(MIN_N_COMPLETE, (length(preds) + 2))) {
    return(list(
      summary = tibble(
        collin_ok = FALSE,
        collin_note = "too few complete cases (after filtering + na.omit + optional target!=0)",
        n_used = n_used, p_used = length(preds),
        vif_max = NA_real_, vif_mean = NA_real_, vif_median = NA_real_,
        tol_min = NA_real_,
        n_vif_gt5 = NA_integer_, n_vif_gt10 = NA_integer_,
        kappa = NA_real_,
        cor_max_abs = NA_real_, cor_median_abs = NA_real_,
        rank_deficient = NA
      ),
      vif_long = tibble()
    ))
  }
  
  # --- fit lineárního modelu
  fml <- as.formula(paste(target, "~", paste(preds, collapse = " + ")))
  fit <- tryCatch(lm(fml, data = d2), error = function(e) NULL)
  if (is.null(fit)) {
    return(list(
      summary = tibble(
        collin_ok = FALSE,
        collin_note = "lm() failed",
        n_used = n_used, p_used = length(preds),
        vif_max = NA_real_, vif_mean = NA_real_, vif_median = NA_real_,
        tol_min = NA_real_,
        n_vif_gt5 = NA_integer_, n_vif_gt10 = NA_integer_,
        kappa = NA_real_,
        cor_max_abs = NA_real_, cor_median_abs = NA_real_,
        rank_deficient = NA
      ),
      vif_long = tibble()
    ))
  }
  
  mm  <- model.matrix(fit)
  kap <- safe_kappa(mm)
  rd  <- rank_deficiency_flag(fit)
  
  X <- d2 %>% select(all_of(preds))
  
  vif_tbl <- vif_manual(X) %>%
    mutate(
      tolerance = 1 / vif,
      tolerance = ifelse(is.infinite(vif), 0, tolerance)
    )
  
  cor_sum <- pairwise_cor_summary(as.matrix(X))
  
  out_sum <- tibble(
    collin_ok = TRUE,
    collin_note = "",
    n_used = n_used,
    p_used = length(preds),
    vif_max = ifelse(nrow(vif_tbl) > 0, suppressWarnings(max(vif_tbl$vif, na.rm = TRUE)), NA_real_),
    vif_mean = ifelse(nrow(vif_tbl) > 0, suppressWarnings(mean(vif_tbl$vif, na.rm = TRUE)), NA_real_),
    vif_median = ifelse(nrow(vif_tbl) > 0, suppressWarnings(median(vif_tbl$vif, na.rm = TRUE)), NA_real_),
    tol_min = ifelse(nrow(vif_tbl) > 0, suppressWarnings(min(vif_tbl$tolerance, na.rm = TRUE)), NA_real_),
    n_vif_gt5 = ifelse(nrow(vif_tbl) > 0, sum(vif_tbl$vif > 5, na.rm = TRUE), NA_integer_),
    n_vif_gt10 = ifelse(nrow(vif_tbl) > 0, sum(vif_tbl$vif > 10, na.rm = TRUE), NA_integer_),
    kappa = kap,
    cor_max_abs = cor_sum$cor_max_abs,
    cor_median_abs = cor_sum$cor_median_abs,
    rank_deficient = rd
  )
  
  # --- dlouhý formát tabulky VIF doplněný o identifikátory modelu
  id_cols <- intersect(names(row), c("set_id","target","skupina","Hclass","Hclass_id","subset","n_pred","CV_R2"))
  
  vif_long <- vif_tbl
  if (nrow(vif_long) > 0) {
    for (cc in rev(id_cols)) {
      vif_long <- vif_long %>% mutate(!!cc := row[[cc]], .before = 1)
    }
  }
  
  list(summary = out_sum, vif_long = vif_long)
}

# ------------------------------------------------------------
# 4) Výpočet diagnostik pro všechny modely
# ------------------------------------------------------------
rows <- split(df_models, seq_len(nrow(df_models)))
res  <- map(rows, compute_for_row, df_data = df_data)

df_sum  <- bind_rows(map(res, "summary"))
df_vifs <- bind_rows(map(res, "vif_long"))

df_models_enriched <- bind_cols(df_models, df_sum)

# ------------------------------------------------------------
# 5) Export výstupů
# ------------------------------------------------------------
if (has_writexl) {
  writexl::write_xlsx(df_models_enriched, OUT_MODELS_ENRICHED_XLSX)
  writexl::write_xlsx(df_vifs, OUT_VIF_LONG_XLSX)
  message("HOTOVO (XLSX):")
  message(" - ", OUT_MODELS_ENRICHED_XLSX)
  message(" - ", OUT_VIF_LONG_XLSX)
} else {
  write_csv(df_models_enriched, OUT_MODELS_ENRICHED_CSV)
  write_csv(df_vifs, OUT_VIF_LONG_CSV)
  message("HOTOVO (CSV) – balíček writexl není nainstalován:")
  message(" - ", OUT_MODELS_ENRICHED_CSV)
  message(" - ", OUT_VIF_LONG_CSV)
}

# ------------------------------------------------------------
# 6) Kontrolní souhrn do konzole
# ------------------------------------------------------------
message("\nShrnutí:")
message(" - modelů celkem: ", nrow(df_models))
message(" - úspěšně vyhodnoceno: ", sum(df_models_enriched$collin_ok, na.rm = TRUE))
message(" - neúspěšně vyhodnoceno: ", sum(!df_models_enriched$collin_ok, na.rm = TRUE))
message(" - výstupní složka: ", OUT_DIR)
