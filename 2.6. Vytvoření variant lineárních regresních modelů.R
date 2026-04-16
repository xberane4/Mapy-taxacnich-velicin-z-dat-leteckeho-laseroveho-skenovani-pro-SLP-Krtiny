# ============================================================
# ÚČEL SCRIPTU
# ============================================================
# Tento script slouží k výběru regresních modelů na základě
# metody exhaustive subset selection s využitím funkce regsubsets
# z balíčku leaps. Modely jsou vytvářeny samostatně pro zvolené
# cílové porostní veličiny, skupiny dřevin a výškové intervaly.
#
# Pro každou kombinaci cílové proměnné, dřevinné skupiny
# a výškové třídy jsou:
# - vybrány všechny možné kombinace prediktorů do zadaného
#   maximálního počtu proměnných,
# - vypočteny základní statistiky výběru modelu,
# - vyhodnoceny křížově validační metriky pomocí 10-fold CV,
# - sestaveny rovnice modelů fitovaných na úplných datech,
# - uloženy pouze nejlepší modely podle zvoleného kritéria.
#
# Výstupem scriptu je souhrnná tabulka obsahující nejlepší
# kandidátní modely pro jednotlivé kombinace cílové proměnné,
# dřevinné skupiny a výškové třídy, včetně informací o použitých
# prediktorech, kvalitě modelu a křížově validačních metrikách.
# ============================================================

library(dplyr)
library(leaps)

# ------------------------------------------------------------
# 0) DEFINICE PARAMETRŮ
# ------------------------------------------------------------
DATA_PATH <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/REG_MOD_PIL/sample_plots_REPLACED_BY_PIL.csv"
OUT_PATH  <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/REG_MOD_PIL/INT_A_MOD_PIL.csv"

TARGETS <- c("V_per_ha", "H_DOM", "H_LOREY", "G_per_ha", "N_per_ha")

# Proměnná použitá pro přiřazení výškových tříd
HEIGHT_CLASS_VAR <- "z_p95"

# Maximální počet prediktorů v modelu
NV_MAX <- 3

# Parametry křížové validace
K_FOLDS <- 10
SEED    <- 2025

# Volby filtrace a exportu
DROP_ZERO_TARGET <- TRUE
TOP_N <- 4  # počet nejlepších modelů ukládaných pro každou kombinaci

# Povolené skupiny dřevin
SPECIES_LEVELS <- c(
  "DBZ", "BK", "SM", "MD", "BO",
  "ostatní listnaté", "ostatní jehličnaté"
)

# ------------------------------------------------------------
# 1) DEFINICE VÝŠKOVÝCH INTERVALŮ PRO JEDNOTLIVÉ DŘEVINY
# ------------------------------------------------------------
# Intervaly jsou definovány individuálně pro každou skupinu dřevin.
# Dolní hranice je včetně, horní hranice je exkluzivní.
# Poslední interval může být otevřený směrem k nekonečnu.
# ------------------------------------------------------------
HEIGHT_BINS <- tribble(
  ~skupina,             ~class_id, ~h_min,     ~h_max,     ~label,
  "DBZ",                1,         0,          17.117,     "A_z_p95_[0;17.117)",
  "DBZ",                2,         17.117,     20.558,     "A_z_p95_[17.117;20.558)",
  "DBZ",                3,         20.558,     Inf,        "A_z_p95_[20.558;+)",
  
  "BK",                 1,         0,          19.0685,    "A_z_p95_[0;19.0685)",
  "BK",                 2,         19.0685,    24.7646,    "A_z_p95_[19.0685;24.7646)",
  "BK",                 3,         24.7646,    29.269975,  "A_z_p95_[24.7646;29.269975)",
  "BK",                 4,         29.269975,  Inf,        "A_z_p95_[29.269975;+)",
  
  "SM",                 1,         0,          15.567333,  "A_z_p95_[0;15.567333)",
  "SM",                 2,         15.567333,  22.43,      "A_z_p95_[15.567333;22.43)",
  "SM",                 3,         22.43,      Inf,        "A_z_p95_[22.43;+)",
  
  "MD",                 1,         0,          21.256725,  "A_z_p95_[0;21.256725)",
  "MD",                 2,         21.256725,  Inf,        "A_z_p95_[21.256725;+)",
  
  "BO",                 1,         0,          17.19965,   "A_z_p95_[0;17.19965)",
  "BO",                 2,         17.19965,   Inf,        "A_z_p95_[17.19965;+)",
  
  "ostatní listnaté",   1,         0,          17.06666,   "A_z_p95_[0;17.06666)",
  "ostatní listnaté",   2,         17.06666,   20.67675,   "A_z_p95_[17.06666;20.67675)",
  "ostatní listnaté",   3,         20.67675,   23.96732,   "A_z_p95_[20.67675;23.96732)",
  "ostatní listnaté",   4,         23.96732,   28.01321,   "A_z_p95_[23.96732;28.01321)",
  "ostatní listnaté",   5,         28.01321,   Inf,        "A_z_p95_[28.01321;+)",
  
  "ostatní jehličnaté", 1,         0,          19.47715,   "A_z_p95_[0;19.47715)",
  "ostatní jehličnaté", 2,         19.47715,   23.64325,   "A_z_p95_[19.47715;23.64325)",
  "ostatní jehličnaté", 3,         23.64325,   27.8142,    "A_z_p95_[23.64325;27.8142)",
  "ostatní jehličnaté", 4,         27.8142,    31.10385,   "A_z_p95_[27.8142;31.10385)",
  "ostatní jehličnaté", 5,         31.10385,   Inf,        "A_z_p95_[31.10385;+)"
)

# ------------------------------------------------------------
# 2) POMOCNÉ FUNKCE PRO VÝPOČET METRIK A TVORBU ROVNIC
# ------------------------------------------------------------

rmse_fun <- function(y, yhat) sqrt(mean((y - yhat)^2))

mae_fun <- function(y, yhat) mean(abs(y - yhat))

r2_fun <- function(y, yhat) {
  ss_res <- sum((y - yhat)^2)
  ss_tot <- sum((y - mean(y))^2)
  1 - ss_res / ss_tot
}

nrmse_range_fun <- function(rmse, y) {
  r <- max(y) - min(y)
  if (is.finite(r) && r > 0) rmse / r else NA_real_
}

nrmse_mean_fun <- function(rmse, y) {
  m <- mean(y)
  if (is.finite(m) && m != 0) rmse / abs(m) else NA_real_
}

# ------------------------------------------------------------
# Funkce pro výpočet křížově validačních metrik lineárního modelu
# ------------------------------------------------------------
cv_metrics_lm <- function(df_mod, target, preds, k = 10, seed = 2025) {
  n <- nrow(df_mod)
  set.seed(seed)
  fold_id <- sample(rep(1:k, length.out = n))
  
  y_all <- df_mod[[target]]
  yhat_oof <- rep(NA_real_, n)
  
  for (fold in 1:k) {
    test_idx  <- which(fold_id == fold)
    train_idx <- which(fold_id != fold)
    
    train <- df_mod[train_idx, , drop = FALSE]
    test  <- df_mod[test_idx, , drop = FALSE]
    
    fml <- as.formula(paste(target, "~", paste(preds, collapse = " + ")))
    m <- lm(fml, data = train)
    
    yhat_oof[test_idx] <- predict(m, newdata = test)
  }
  
  rmse <- rmse_fun(y_all, yhat_oof)
  mae  <- mae_fun(y_all, yhat_oof)
  r2   <- r2_fun(y_all, yhat_oof)
  
  list(
    CV_R2 = r2,
    CV_RMSE = rmse,
    CV_MAE = mae,
    CV_nRMSE_range = nrmse_range_fun(rmse, y_all),
    CV_nRMSE_mean  = nrmse_mean_fun(rmse, y_all)
  )
}

# ------------------------------------------------------------
# Funkce pro sestavení rovnice lineárního modelu
# ------------------------------------------------------------
equation_from_lm <- function(df_mod, target, preds) {
  fml <- as.formula(paste(target, "~", paste(preds, collapse = " + ")))
  m <- lm(fml, data = df_mod)
  co <- coef(m)
  
  eq <- paste0(
    target, " = ",
    formatC(co[1], format = "f", digits = 4),
    if (length(co) > 1) paste0(
      " + ",
      paste(
        paste0(formatC(co[-1], format = "f", digits = 4), " * ", names(co)[-1]),
        collapse = " + "
      )
    ) else ""
  )
  
  list(formula = deparse(fml), equation = eq)
}

# ------------------------------------------------------------
# 3) FUNKCE PRO PŘIŘAZENÍ VÝŠKOVÉ TŘÍDY
# ------------------------------------------------------------
# Funkce přiřazuje jednotlivým plochám výškovou třídu podle
# definice v tabulce HEIGHT_BINS, a to samostatně pro každou
# skupinu dřevin.
# ------------------------------------------------------------
assign_height_class <- function(df, height_var, bins_tbl) {
  
  stopifnot("skupina" %in% names(df))
  stopifnot(height_var %in% names(df))
  
  df$Hclass_id <- NA_integer_
  df$Hclass    <- NA_character_
  
  for (sp in unique(bins_tbl$skupina)) {
    bins_sp <- bins_tbl %>%
      filter(skupina == sp) %>%
      arrange(class_id)
    
    idx_sp <- which(df$skupina == sp & !is.na(df[[height_var]]))
    if (length(idx_sp) == 0) next
    
    h <- df[[height_var]][idx_sp]
    
    for (j in seq_len(nrow(bins_sp))) {
      lo  <- bins_sp$h_min[j]
      hi  <- bins_sp$h_max[j]
      cid <- bins_sp$class_id[j]
      lab <- bins_sp$label[j]
      
      in_bin <- (h >= lo) & (h < hi)
      if (any(in_bin)) {
        df$Hclass_id[idx_sp[in_bin]] <- cid
        df$Hclass[idx_sp[in_bin]]    <- lab
      }
    }
  }
  
  df
}

# ------------------------------------------------------------
# 4) NAČTENÍ DAT, FILTRACE A PŘIŘAZENÍ VÝŠKOVÝCH TŘÍD
# ------------------------------------------------------------
df <- read.csv(DATA_PATH)

stopifnot("skupina" %in% names(df))
stopifnot(HEIGHT_CLASS_VAR %in% names(df))

missing_targets <- setdiff(TARGETS, names(df))
if (length(missing_targets) > 0) {
  stop("V datech chybí target sloupce: ", paste(missing_targets, collapse = ", "))
}

df <- df[df$skupina %in% SPECIES_LEVELS, , drop = FALSE]

# Přiřazení výškových tříd podle definovaných intervalů
df <- assign_height_class(df, HEIGHT_CLASS_VAR, HEIGHT_BINS)

# Vyřazení řádků bez přiřazené výškové třídy
df <- df[!is.na(df$Hclass_id) & !is.na(df$Hclass), , drop = FALSE]

cat("Celkem ploch po omezení na zadané skupiny a ne-NA Hclass:", nrow(df), "\n")

# ------------------------------------------------------------
# 5) DEFINICE SADY PREDIKTORŮ
# ------------------------------------------------------------
# Z výběru prediktorů jsou vyloučeny identifikátory, souřadnice,
# klasifikační proměnné a terénní veličiny, které představují
# cílové proměnné nebo z nich přímo vycházejí.
# ------------------------------------------------------------
exclude_vars <- c(
  "IP_FKEY", "X", "Y", "skupina", "Hclass", "Hclass_id",
  "H_TOP", "H_DOM", "BA", "H_LOREY", "VOL_SUM",
  "N_TREES", "G_per_ha", "N_per_ha", "V_per_ha"
)

predictor_vars_all <- setdiff(names(df), exclude_vars)

# Zachování pouze numerických prediktorů
is_num_all <- sapply(df[, predictor_vars_all, drop = FALSE], is.numeric)
predictor_vars_all <- predictor_vars_all[is_num_all]

cat("Počet numerických LiDAR metrik:", length(predictor_vars_all), "\n")

# ------------------------------------------------------------
# 6) HLAVNÍ SMYČKA: TARGET × SKUPINA × Hclass_id
# ------------------------------------------------------------
all_results <- list()
row_counter <- 0

combos <- expand.grid(
  target    = TARGETS,
  skupina   = SPECIES_LEVELS,
  Hclass_id = 1:5,
  stringsAsFactors = FALSE
)

for (ii in 1:nrow(combos)) {
  
  tgt <- combos$target[ii]
  sp  <- combos$skupina[ii]
  cid <- combos$Hclass_id[ii]
  
  sub <- df[df$skupina == sp & df$Hclass_id == cid, , drop = FALSE]
  n_plots_raw <- nrow(sub)
  
  # Volitelná filtrace nulových hodnot cílové proměnné
  n_zero_target <- 0
  if (DROP_ZERO_TARGET && tgt %in% names(sub)) {
    n_zero_target <- sum(!is.na(sub[[tgt]]) & sub[[tgt]] == 0)
    sub <- sub[!( !is.na(sub[[tgt]]) & sub[[tgt]] == 0 ), , drop = FALSE]
  }
  
  n_plots_raw_after_zero <- nrow(sub)
  
  # Získání textového popisu výškového intervalu
  lab <- HEIGHT_BINS %>%
    filter(skupina == sp, class_id == cid) %>%
    distinct(label) %>%
    pull(label)
  
  if (length(lab) == 0) lab <- paste0("class_", cid)
  
  cat("\n--- target=", tgt, " | ", sp, " | ", lab, " | n=", n_plots_raw, " ---\n", sep = "")
  
  if (n_plots_raw == 0) {
    cat("  -> SKIP: žádná plocha\n")
    next
  }
  
  if (n_plots_raw < K_FOLDS) {
    cat("  -> SKIP: méně ploch než K_FOLDS (", K_FOLDS, ")\n", sep = "")
    next
  }
  
  if (n_zero_target > 0) {
    cat("  vyhozeno nulových target hodnot:", n_zero_target,
        " | n po filtru:", n_plots_raw_after_zero, "\n", sep = "")
  }
  
  model_df <- sub[, c(tgt, predictor_vars_all), drop = FALSE]
  model_df <- na.omit(model_df)
  
  n_after <- nrow(model_df)
  cat("  po na.omit: n=", n_after, "\n", sep = "")
  
  if (n_after < K_FOLDS) {
    cat("  -> SKIP: po na.omit méně než K_FOLDS\n")
    next
  }
  
  # Exhaustive subset selection pomocí funkce regsubsets
  fit_leaps <- regsubsets(
    as.formula(paste(tgt, "~ .")),
    data = model_df,
    nvmax = NV_MAX,
    method = "exhaustive",
    really.big = TRUE
  )
  
  summary_leaps <- summary(fit_leaps)
  which_mat <- summary_leaps$which
  rownames(which_mat) <- 1:nrow(which_mat)
  
  extract_predictors <- function(row) names(row)[row == TRUE & names(row) != "(Intercept)"]
  predictor_lists <- apply(which_mat, 1, extract_predictors)
  
  k <- nrow(which_mat)
  cat("  subsetů:", k, "\n")
  
  base_table <- data.frame(
    target = tgt,
    skupina = sp,
    Hclass_id = cid,
    Hclass = lab,
    n_plots_raw = n_plots_raw,
    n_zero_target = n_zero_target,
    n_plots_raw_used = n_plots_raw_after_zero,
    n_plots = n_after,
    subset = 1:k,
    R2_leaps = summary_leaps$rsq,
    Adj_R2_leaps = summary_leaps$adjr2,
    Cp = summary_leaps$cp,
    BIC = summary_leaps$bic,
    predictors = sapply(predictor_lists, function(x) paste(x, collapse = ", ")),
    stringsAsFactors = FALSE
  )
  
  cv_list <- vector("list", k)
  eq_list <- vector("list", k)
  
  for (s in 1:k) {
    preds <- predictor_lists[[s]]
    
    if (length(preds) == 0) {
      cv_list[[s]] <- list(CV_R2 = NA, CV_RMSE = NA, CV_MAE = NA, CV_nRMSE_range = NA, CV_nRMSE_mean = NA)
      eq_list[[s]] <- list(formula = NA, equation = NA)
      next
    }
    
    cv_list[[s]] <- cv_metrics_lm(model_df, tgt, preds, k = K_FOLDS, seed = SEED)
    eq_list[[s]] <- equation_from_lm(model_df, tgt, preds)
  }
  
  cv_df <- do.call(rbind, lapply(cv_list, as.data.frame))
  eq_df <- do.call(rbind, lapply(eq_list, as.data.frame))
  
  out_table <- cbind(base_table, cv_df, eq_df)
  
  # Výběr nejlepších modelů podle hodnoty CV_R2
  out_table <- out_table %>%
    arrange(desc(CV_R2)) %>%
    slice_head(n = TOP_N)
  
  row_counter <- row_counter + 1
  all_results[[row_counter]] <- out_table
}

# ------------------------------------------------------------
# 7) SPOJENÍ VÝSLEDKŮ A EXPORT
# ------------------------------------------------------------
if (length(all_results) == 0) {
  stop("Nevznikly žádné výsledky (všechny kombinace byly skipnuté).")
}

final_out <- bind_rows(all_results) %>%
  arrange(target, skupina, Hclass_id, desc(CV_R2))

write.csv(final_out, OUT_PATH, row.names = FALSE)

cat("\n✔ Hotovo — uložený výstup:\n", OUT_PATH, "\n", sep = "")
