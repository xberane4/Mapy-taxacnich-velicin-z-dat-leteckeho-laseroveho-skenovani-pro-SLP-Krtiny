# ============================================================
# ÚČEL SKRIPTU
# Skript slouží k hromadnému testování více scénářů hierarchického
# regresního modelování nad daty z ALS/LiDAR a zkusných ploch.
# Pro každou kombinaci zvolených parametrů vytváří intervalové
# rozdělení výšky porostu, provádí výběr prediktorů, odhaduje
# smíšené nebo lineární modely, vyhodnocuje jejich přesnost pomocí
# křížové validace a ukládá souhrnné výsledky pro následné porovnání.
# ============================================================

# ============================================================
# ABA LiDAR: dávkové testování scénářů modelování
# - spouští více variant scénářů definovaných kombinací:
#   COR_CUTOFF × N_HCLASS × NV_MAX_DEFAULT × LEAPS_CRITERION × HCLASS_SOURCE
# - pro každý scénář:
#   -> ukládá souhrnnou tabulku CV metrik ve formátu CSV
#   -> ukládá detailní výsledky ve formátu RDS
# - na závěr vytváří jednu společnou tabulku napříč všemi scénáři
#
# HLAVNÍ ÚPRAVA:
# - intervalová příslušnost (Hclass) může být vytvořena podle:
#     * "H_DOM"   = dominantní výška porostu na zkusných plochách
#     * "H_PROXY" = výšková proxy veličina z LiDAR dat dostupná i pro pixely
#                   (doporučená varianta pro následnou plošnou aplikaci)
#
# DŮLEŽITÁ ÚPRAVA:
# - pro logaritmicky transformované cílové veličiny
#   (G_per_ha, V_per_ha, N_per_ha) je zpětná transformace provedena jako:
#     exp(eta + 0.5 * sigma^2)
#   kde sigma představuje reziduální směrodatnou odchylku modelu
#   odhadnutou vždy v rámci příslušného trénovacího foldu
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(leaps)
  library(lme4)
})

# --------------------------
# 0) CESTY K SOUBORŮM
# --------------------------
CSV_PATH  <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/LLM_X20/sample_plots_REPLACED_BY_X20.csv"
OUT_ROOT  <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/LLM_X20/SMISENY_MODEL_SCENARIOS_X20_test_TRANSFORM"
dir.create(OUT_ROOT, showWarnings = FALSE, recursive = TRUE)

# --------------------------
# 1) GLOBÁLNÍ NASTAVENÍ
# --------------------------
TARGETS <- c("H_DOM","H_LOREY","G_per_ha","V_per_ha","N_per_ha")

LEAPS_METHOD <- "seqrep"
MIN_N_FOLD <- 60

MIN_N_PER_SPECIES <- 20
K_FOLDS <- 10
SEED <- 2025

# Minimální počty úrovní pro zahrnutí náhodných efektů
MIN_LEVELS_SKUPINA <- 3
MIN_LEVELS_HCLASS  <- 4

# >>> DŮLEŽITÉ: nastav sloupec s LiDAR výškovou proxy veličinou,
# který bude použit pro tvorbu intervalů při plošné aplikaci.
# Tento sloupec musí existovat jak v tabulce ploch, tak později i v pixelech.
H_PROXY_COL <- "z_p95"

# --------------------------
# 1A) TRANSFORMACE PROMĚNNÝCH
# --------------------------
LOG_TARGETS <- c("G_per_ha","V_per_ha","N_per_ha")

log_safe <- function(y) {
  out <- rep(NA_real_, length(y))
  ok <- is.finite(y) & y > 0
  out[ok] <- log(y[ok])
  out
}

TRANSFORM <- list(
  H_DOM    = function(y) y,
  H_LOREY  = function(y) y,
  G_per_ha = log_safe,
  V_per_ha = log_safe,
  N_per_ha = log_safe
)

get_sigma <- function(model) {
  if (inherits(model, "merMod")) return(as.numeric(sigma(model)))
  if (inherits(model, "lm"))     return(as.numeric(summary(model)$sigma))
  NA_real_
}

backtransform_pred <- function(target_name, eta, sigma_hat) {
  eta <- as.numeric(eta)
  out <- rep(NA_real_, length(eta))
  ok <- is.finite(eta)
  
  if (!(target_name %in% LOG_TARGETS)) {
    out[ok] <- eta[ok]
    return(out)
  }
  
  s2 <- as.numeric(sigma_hat)^2
  if (!is.finite(s2)) return(out)
  
  # Bias-korigovaná zpětná transformace pro lognormální rozdělení:
  # E(Y) = exp(eta + 0.5 * sigma^2)
  out[ok] <- exp(eta[ok] + 0.5 * s2)
  out
}

# --------------------------
# 2) POMOCNÉ FUNKCE
# --------------------------
is_numeric_col <- function(x) is.numeric(x) && !is.logical(x)

filter_predictors <- function(X, cor_cutoff = 0.99) {
  X <- as.data.frame(X)
  
  sds <- suppressWarnings(sapply(X, sd, na.rm = TRUE))
  keep <- is.finite(sds) & sds > 0
  X <- X[, keep, drop = FALSE]
  if (ncol(X) < 2) return(X)
  
  hashes <- vapply(X, function(col) paste(col, collapse = "|"), character(1))
  dup <- duplicated(hashes)
  if (any(dup)) X <- X[, !dup, drop = FALSE]
  if (ncol(X) < 2) return(X)
  
  C <- suppressWarnings(cor(X, use = "pairwise.complete.obs"))
  if (!all(is.finite(C))) return(X)
  
  to_drop <- rep(FALSE, ncol(X))
  for (i in 1:(ncol(X) - 1)) {
    if (to_drop[i]) next
    high <- which(abs(C[i, (i + 1):ncol(X)]) >= cor_cutoff) + i
    if (length(high) > 0) to_drop[high] <- TRUE
  }
  
  X[, !to_drop, drop = FALSE]
}

safe_quantile_breaks <- function(x, n_class) {
  x <- x[is.finite(x)]
  if (length(x) < 3) return(NULL)
  br <- unique(quantile(x, probs = seq(0, 1, length.out = n_class + 1), na.rm = TRUE))
  if (length(br) < 3) return(NULL)
  br
}

# Výpočet hranic intervalů z vybraného zdrojového sloupce
# (H_DOM nebo H_PROXY_COL) pouze na trénovacích datech
compute_breaks_train <- function(train_df, n_class = 4, min_n_sp = 20, source_col) {
  xg <- train_df[[source_col]]
  global_br <- safe_quantile_breaks(xg, n_class)
  
  sp_list <- list()
  spp <- unique(as.character(train_df$skupina))
  for (sp in spp) {
    x <- train_df[[source_col]][as.character(train_df$skupina) == sp]
    x <- x[is.finite(x)]
    if (length(x) < min_n_sp) {
      sp_list[[sp]] <- NULL
      next
    }
    sp_list[[sp]] <- safe_quantile_breaks(x, n_class)
  }
  
  list(global = global_br, by_species = sp_list, source_col = source_col)
}

# Přiřazení výškové třídy Hclass podle stejného zdrojového sloupce
apply_Hclass <- function(df_any, breaks_obj) {
  out <- df_any %>% mutate(skupina = factor(skupina))
  out$Hclass_q <- NA_character_
  
  global_br  <- breaks_obj$global
  sp_brks    <- breaks_obj$by_species
  source_col <- breaks_obj$source_col
  
  if (!source_col %in% names(out)) {
    stop("Missing interval source column in data: ", source_col)
  }
  
  for (sp in levels(out$skupina)) {
    idx <- which(out$skupina == sp)
    if (length(idx) == 0) next
    
    br <- sp_brks[[as.character(sp)]]
    if (is.null(br)) br <- global_br
    
    if (is.null(br)) {
      out$Hclass_q[idx] <- "ALL"
    } else {
      labs <- paste0("Q", seq_len(length(br) - 1))
      out$Hclass_q[idx] <- as.character(
        cut(out[[source_col]][idx],
            breaks = br,
            include.lowest = TRUE,
            labels = labs,
            right = TRUE)
      )
      out$Hclass_q[idx][is.na(out$Hclass_q[idx])] <- "ALL"
    }
  }
  
  out %>%
    mutate(
      Hclass_q  = factor(Hclass_q),
      Hclass_sp = interaction(skupina, Hclass_q, drop = TRUE, sep = "__")
    )
}

pick_best_exact_size <- function(regfit, size_k, criterion = c("bic","adjr2","cp")) {
  criterion <- match.arg(criterion)
  s <- summary(regfit)
  
  if (size_k < 1 || size_k > nrow(s$which)) return(character(0))
  
  row <- size_k
  vars <- names(s$which[row, ])[s$which[row, ]]
  vars <- setdiff(vars, "(Intercept)")
  
  if (length(vars) != size_k) {
    ok_sizes <- which(rowSums(s$which[, -1, drop = FALSE]) > 0)
    ok_sizes <- ok_sizes[ok_sizes <= size_k]
    if (length(ok_sizes) == 0) return(character(0))
    
    score <- switch(
      criterion,
      bic   = s$bic,
      cp    = s$cp,
      adjr2 = -s$adjr2
    )
    
    best_row <- ok_sizes[which.min(score[ok_sizes])]
    vars <- names(s$which[best_row, ])[s$which[best_row, ]]
    vars <- setdiff(vars, "(Intercept)")
  }
  
  vars
}

choose_random_terms <- function(dtr) {
  n_sp <- nlevels(dtr$skupina)
  n_hc <- nlevels(dtr$Hclass_sp)
  
  use_sp <- is.finite(n_sp) && n_sp >= MIN_LEVELS_SKUPINA
  use_hc <- is.finite(n_hc) && n_hc >= MIN_LEVELS_HCLASS
  
  if (use_sp && use_hc) return("(1|skupina) + (1|Hclass_sp)")
  if (use_sp) return("(1|skupina)")
  if (use_hc) return("(1|Hclass_sp)")
  "0"
}

fit_lmm_safe <- function(dtr, fixed_part) {
  rand <- choose_random_terms(dtr)
  
  if (identical(rand, "0")) {
    f <- as.formula(paste0("y_trans ~ ", fixed_part))
    m <- lm(f, data = dtr)
    return(list(model = m, formula = f, used_random = "LM"))
  }
  
  f <- as.formula(paste0("y_trans ~ ", fixed_part, " + ", rand))
  m <- suppressWarnings(lmer(f, data = dtr, REML = FALSE))
  
  if (inherits(m, "merMod") && isSingular(m, tol = 1e-5)) {
    if (grepl("Hclass_sp", rand) && grepl("skupina", rand)) {
      f2 <- as.formula(paste0("y_trans ~ ", fixed_part, " + (1|skupina)"))
      m2 <- suppressWarnings(lmer(f2, data = dtr, REML = FALSE))
      if (!isSingular(m2, tol = 1e-5)) {
        return(list(model = m2, formula = f2, used_random = "skupina"))
      }
    }
  }
  
  list(model = m, formula = f, used_random = rand)
}

predict_any <- function(model, newdata) {
  if (inherits(model, "merMod")) {
    return(predict(model, newdata = newdata, re.form = NULL, allow.new.levels = TRUE))
  }
  predict(model, newdata = newdata)
}

cv_metrics <- function(y_true, y_pred) {
  ok <- is.finite(y_true) & is.finite(y_pred)
  y_true <- y_true[ok]
  y_pred <- y_pred[ok]
  n <- length(y_true)
  
  if (n < 2) {
    return(data.frame(n=n, R2=NA, RMSE=NA, MAE=NA, nRMSE_range=NA, nRMSE_mean=NA))
  }
  
  resid <- y_true - y_pred
  rmse <- sqrt(mean(resid^2))
  mae  <- mean(abs(resid))
  
  ss_res <- sum(resid^2)
  ss_tot <- sum((y_true - mean(y_true))^2)
  r2 <- if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_
  
  rng <- max(y_true) - min(y_true)
  nrmse_range <- if (rng > 0) rmse / rng else NA_real_
  
  mu <- mean(y_true)
  nrmse_mean <- if (mu != 0) rmse / mu else NA_real_
  
  data.frame(
    n = n,
    R2 = r2,
    RMSE = rmse,
    MAE = mae,
    nRMSE_range = nrmse_range,
    nRMSE_mean = nrmse_mean
  )
}

# --------------------------
# 3) JEDNORÁZOVÉ NAČTENÍ DAT
# --------------------------
df <- read.csv(CSV_PATH, stringsAsFactors = FALSE, check.names = FALSE)

need_cols <- c("skupina", "H_DOM", H_PROXY_COL)
miss <- setdiff(need_cols, names(df))
if (length(miss) > 0) {
  stop("Missing required columns in CSV: ", paste(miss, collapse = ", "))
}

targets_present <- TARGETS[TARGETS %in% names(df)]
if (length(targets_present) == 0) stop("None of TARGETS found in data.")

# Nahrazení nulových hodnot cílových veličin za NA
df[targets_present] <- lapply(df[targets_present], function(x) {
  x[x == 0] <- NA
  x
})

df <- df %>% mutate(skupina = factor(skupina))

EXCLUDE_ALWAYS <- c(
  "IP_FKEY","X","Y","skupina",
  "H_TOP","H_DOM","H_LOREY","BA","VOL_SUM","N_TREES","G_per_ha","N_per_ha","V_per_ha"
)

predictor_cols_all <- names(df)[sapply(df, is_numeric_col)]
predictor_cols_all <- setdiff(predictor_cols_all, intersect(predictor_cols_all, EXCLUDE_ALWAYS))

if (length(predictor_cols_all) < 2) {
  stop("Not enough numeric predictors after exclusions.")
}

# Stratifikované foldy vytvořené pouze jednou,
# aby byly scénáře mezi sebou spravedlivě porovnatelné
set.seed(SEED)
df$.row_id <- seq_len(nrow(df))
fold_id <- rep(NA_integer_, nrow(df))

for (sp in levels(df$skupina)) {
  idx <- which(df$skupina == sp)
  idx <- sample(idx)
  fold_id[idx] <- rep(1:K_FOLDS, length.out = length(idx))
}

df$.fold <- fold_id

cat("Loaded rows:", nrow(df), "\n")
cat("Predictor candidates:", length(predictor_cols_all), "\n")
cat("Targets:", paste(targets_present, collapse = ", "), "\n")
cat("H_PROXY_COL for intervals:", H_PROXY_COL, "\n")

# --------------------------
# 4) FUNKCE PRO SPUŠTĚNÍ JEDNOHO SCÉNÁŘE
# --------------------------
run_scenario <- function(df, scenario, predictor_cols_all) {
  
  COR_CUTOFF      <- scenario$COR_CUTOFF
  N_HCLASS        <- scenario$N_HCLASS
  NV_MAX_DEFAULT  <- scenario$NV_MAX_DEFAULT
  LEAPS_CRITERION <- scenario$LEAPS_CRITERION
  HCLASS_SOURCE   <- scenario$HCLASS_SOURCE
  
  source_col <- if (identical(HCLASS_SOURCE, "H_DOM")) "H_DOM" else H_PROXY_COL
  
  scen_id <- paste0(
    "cap", NV_MAX_DEFAULT,
    "_h", N_HCLASS,
    "_cor", gsub("\\.", "", format(COR_CUTOFF, nsmall = 2)),
    "_", LEAPS_CRITERION,
    "_", HCLASS_SOURCE
  )
  
  out_dir <- file.path(OUT_ROOT, scen_id)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  results <- list()
  cv_rows <- list()
  
  for (target_name in targets_present) {
    
    oof <- data.frame(
      .row_id = df$.row_id,
      y_true  = df[[target_name]],
      y_pred  = NA_real_
    )
    
    fold_selected <- vector("list", K_FOLDS)
    fold_used_re  <- rep(NA_character_, K_FOLDS)
    
    for (k in 1:K_FOLDS) {
      
      train_idx <- which(df$.fold != k)
      test_idx  <- which(df$.fold == k)
      
      train_raw <- df[train_idx, , drop = FALSE]
      test_raw  <- df[test_idx,  , drop = FALSE]
      
      brks <- compute_breaks_train(
        train_raw,
        n_class   = N_HCLASS,
        min_n_sp  = MIN_N_PER_SPECIES,
        source_col = source_col
      )
      
      train <- apply_Hclass(train_raw, brks)
      test  <- apply_Hclass(test_raw,  brks)
      
      dtr <- train %>%
        select(.row_id, all_of(c(target_name, "skupina", "Hclass_sp", predictor_cols_all))) %>%
        filter(!is.na(.data[[target_name]]), !is.na(skupina), !is.na(Hclass_sp))
      
      ytr <- TRANSFORM[[target_name]](dtr[[target_name]])
      oky <- is.finite(ytr)
      dtr <- dtr[oky, , drop = FALSE]
      ytr <- ytr[oky]
      
      if (nrow(dtr) < MIN_N_FOLD) next
      
      Xtr0 <- dtr[, predictor_cols_all, drop = FALSE]
      cc <- complete.cases(Xtr0, ytr)
      dtr <- dtr[cc, , drop = FALSE]
      ytr <- ytr[cc]
      Xtr0 <- Xtr0[cc, , drop = FALSE]
      
      if (nrow(dtr) < MIN_N_FOLD) next
      
      Xtr <- filter_predictors(Xtr0, cor_cutoff = COR_CUTOFF)
      if (ncol(Xtr) < 2) next
      
      d_leaps <- cbind(ytr = ytr, Xtr)
      names(d_leaps) <- make.names(names(d_leaps), unique = TRUE)
      
      pred_names <- setdiff(names(d_leaps), "ytr")
      form_leaps <- as.formula(paste0("ytr ~ ", paste(pred_names, collapse = " + ")))
      
      reg <- regsubsets(
        form_leaps,
        data   = d_leaps,
        nvmax  = min(NV_MAX_DEFAULT, length(pred_names)),
        method = LEAPS_METHOD
      )
      
      selected <- pick_best_exact_size(
        reg,
        size_k    = min(NV_MAX_DEFAULT, length(pred_names)),
        criterion = LEAPS_CRITERION
      )
      
      if (length(selected) == 0) next
      selected <- intersect(selected, pred_names)
      if (length(selected) == 0) next
      
      # Příprava trénovacích dat pro odhad lmer/lm modelu
      dtr$y_trans <- ytr
      dtr2 <- cbind(
        dtr[, c("skupina","Hclass_sp","y_trans"), drop = FALSE],
        d_leaps[, selected, drop = FALSE]
      )
      
      fixed_part <- paste(selected, collapse = " + ")
      fit <- tryCatch(fit_lmm_safe(dtr2, fixed_part), error = function(e) NULL)
      if (is.null(fit)) next
      
      fold_selected[[k]] <- selected
      fold_used_re[k] <- fit$used_random
      
      # Testovací data
      dte <- test %>%
        select(.row_id, all_of(c(target_name, "skupina", "Hclass_sp", predictor_cols_all))) %>%
        filter(!is.na(.data[[target_name]]), !is.na(skupina), !is.na(Hclass_sp))
      
      if (nrow(dte) == 0) next
      
      # Zachování stejné množiny prediktorů jako po korelační filtraci v tréninku
      Xte0 <- dte[, colnames(Xtr), drop = FALSE]
      
      cc_te <- complete.cases(Xte0[, selected, drop = FALSE], dte$skupina, dte$Hclass_sp)
      dte2 <- dte[cc_te, , drop = FALSE]
      if (nrow(dte2) == 0) next
      
      Xte <- Xte0[cc_te, , drop = FALSE]
      dte_leaps <- as.data.frame(Xte)
      names(dte_leaps) <- make.names(names(dte_leaps), unique = TRUE)
      
      newdata <- cbind(
        dte2[, c("skupina","Hclass_sp"), drop = FALSE],
        dte_leaps[, selected, drop = FALSE]
      )
      
      eta_hat <- tryCatch(
        predict_any(fit$model, newdata),
        error = function(e) rep(NA_real_, nrow(newdata))
      )
      
      sig_hat <- get_sigma(fit$model)
      y_hat   <- backtransform_pred(target_name, eta_hat, sig_hat)
      
      oof_idx <- match(dte2$.row_id, oof$.row_id)
      oof$y_pred[oof_idx] <- y_hat
    }
    
    met <- cv_metrics(oof$y_true, oof$y_pred)
    met$n_pred <- sum(is.finite(oof$y_true) & is.finite(oof$y_pred))
    met$target <- target_name
    
    sel_all <- unlist(fold_selected, recursive = TRUE, use.names = FALSE)
    sel_freq <- sort(table(sel_all), decreasing = TRUE)
    
    top_sel <- if (length(sel_freq) > 0) {
      paste(names(sel_freq)[1:min(10, length(sel_freq))], collapse = ", ")
    } else {
      ""
    }
    
    results[[target_name]] <- list(
      target = target_name,
      cv = met,
      selected_freq = sel_freq,
      used_random = table(fold_used_re, useNA = "ifany"),
      notes = list(
        scenario_id = scen_id,
        cor_cutoff = COR_CUTOFF,
        n_hclass = N_HCLASS,
        hard_cap = NV_MAX_DEFAULT,
        leaps_criterion = LEAPS_CRITERION,
        hclass_source = HCLASS_SOURCE,
        hclass_source_col = source_col,
        top_selected = top_sel,
        backtransform = "identity for heights; exp(eta + 0.5*sigma^2) for log-targets"
      )
    )
    
    cv_rows[[target_name]] <- data.frame(
      target = target_name,
      n = met$n,
      R2 = met$R2,
      RMSE = met$RMSE,
      MAE = met$MAE,
      nRMSE_range = met$nRMSE_range,
      nRMSE_mean = met$nRMSE_mean,
      n_pred = met$n_pred,
      top10_predictors = top_sel,
      scenario_id = scen_id,
      COR_CUTOFF = COR_CUTOFF,
      N_HCLASS = N_HCLASS,
      NV_MAX_DEFAULT = NV_MAX_DEFAULT,
      LEAPS_CRITERION = LEAPS_CRITERION,
      HCLASS_SOURCE = HCLASS_SOURCE,
      HCLASS_SOURCE_COL = source_col,
      BACKTRANSFORM = ifelse(
        target_name %in% LOG_TARGETS,
        "exp(eta + 0.5*sigma^2)",
        "identity"
      ),
      stringsAsFactors = FALSE
    )
    
    cat("[", scen_id, "] ", target_name,
        ": R2=", round(met$R2, 3),
        " RMSE=", round(met$RMSE, 3),
        " n_pred=", met$n_pred,
        " (Hclass=", HCLASS_SOURCE, "->", source_col, ")\n",
        sep = "")
  }
  
  cv_table <- do.call(rbind, cv_rows)
  
  write.csv(cv_table, file.path(out_dir, "CV_summary.csv"), row.names = FALSE)
  saveRDS(results, file.path(out_dir, "CV_results.rds"))
  
  return(cv_table)
}

# --------------------------
# 5) DEFINICE TESTOVANÝCH SCÉNÁŘŮ
# --------------------------
scenarios <- expand.grid(
  COR_CUTOFF = c(0.99, 0.95),
  N_HCLASS = c(3, 4),
  NV_MAX_DEFAULT = c(3, 5),
  LEAPS_CRITERION = c("bic", "adjr2"),
  HCLASS_SOURCE = c("H_DOM", "H_PROXY"),
  stringsAsFactors = FALSE
)

cat("Total scenarios:", nrow(scenarios), "\n")

# --------------------------
# 6) SPUŠTĚNÍ VŠECH SCÉNÁŘŮ A SLOUČENÍ VÝSLEDKŮ
# --------------------------
all_tables <- vector("list", nrow(scenarios))

for (i in seq_len(nrow(scenarios))) {
  scen <- as.list(scenarios[i, ])
  all_tables[[i]] <- run_scenario(df, scen, predictor_cols_all)
}

summary_all <- do.call(rbind, all_tables)
summary_all <- summary_all %>%
  arrange(target, desc(R2))

write.csv(summary_all, file.path(OUT_ROOT, "ALL_SCENARIOS_CV_SUMMARY.csv"), row.names = FALSE)

cat("\nDONE. Combined summary saved to:\n",
    file.path(OUT_ROOT, "ALL_SCENARIOS_CV_SUMMARY.csv"), "\n", sep = "")
