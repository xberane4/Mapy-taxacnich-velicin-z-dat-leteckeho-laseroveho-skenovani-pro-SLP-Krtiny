# ============================================================
# SKRIPT: Evaluace struktur náhodných efektů v lineárních
# smíšených modelech pro předem definované fixní prediktory
#
# Účel skriptu:
# Skript slouží k vyhodnocení různých struktur náhodných efektů
# v lineárních smíšených modelech (LMM) při použití fixních
# prediktorů převzatých z externího souboru MS Excel. Hodnocení
# probíhá jak na celé trénovací množině pomocí informačních
# kritérií a diagnostiky modelu, tak prostřednictvím 10násobné
# křížové validace. Současně je zajištěna korektní zpětná
# transformace predikcí pro logaritmicky transformované cílové
# proměnné.
#
# Hlavní výstupy:
# - souhrnná tabulka in-sample hodnocení modelů,
# - souhrnná tabulka výsledků 10-fold cross-validace,
# - RDS objekt s plně odhadnutými modely a metadaty analýzy.
#
# Poznámky k implementaci:
# 1) Validační foldy jsou generovány náhodně, avšak stratifikovaně
#    podle proměnné 'skupina', a to vždy pouze na řádcích, které
#    jsou skutečně použity v daném scénáři modelu.
# 2) Pro logaritmicky transformované cílové proměnné je použita
#    bias-korigovaná zpětná transformace ve tvaru:
#       y_hat = exp(eta + 0.5 * sigma^2)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(lme4)
  library(readxl)
  library(stringr)
  library(tidyr)
})

# --------------------------
# 0) Nastavení vstupních a výstupních cest
# --------------------------
CSV_PATH  <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/LLM_X20/sample_plots_REPLACED_BY_X20.csv"
XLSX_PATH <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/LLM_X20/SMIS_MOD_FIN_X20.xlsx"

OUT_DIR   <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/LLM_X20/X20_random_eval_FINAL_FIN"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

TARGETS <- c("H_DOM","H_LOREY","G_per_ha","V_per_ha","N_per_ha")

K_FOLDS <- 10
SEED    <- 2025

# --------------------------
# Nastavení výškových tříd podle proxy výšky
# --------------------------
H_PROXY_COL <- "z_p95"          # proxy výška použitá pro konstrukci tříd
N_HCLASS <- 4
MIN_N_PER_SPECIES <- 20         # minimální počet řádků pro druhově specifické hranice; jinak se použije globální varianta

MIN_N_FOLD <- 60
MIN_LEVELS_FACTOR <- 3
MIN_LEVELS_HCLASS <- 4          # minimální počet úrovní Hclass_sp pro zařazení náhodného efektu

RAND_CANDIDATES <- c(
  "LM",
  "(1|skupina)",
  "(1|Hclass_sp)",
  "(1|skupina) + (1|Hclass_sp)",
  "(1|skupina:Hclass_q)"        # volitelná varianta; často se chová podobně jako (1|Hclass_sp)
)

# --------------------------
# 1) Definice transformací cílových proměnných
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

# --------------------------
# 2) Pomocné funkce
# --------------------------
safe_quantile_breaks <- function(x, n_class) {
  x <- x[is.finite(x)]
  if (length(x) < 3) return(NULL)
  br <- unique(quantile(x, probs = seq(0, 1, length.out = n_class + 1), na.rm = TRUE))
  if (length(br) < 3) return(NULL)
  br
}

# Výpočet hranic tříd z trénovacích dat:
# - globálně,
# - samostatně pro jednotlivé dřeviny (při dostatečném počtu pozorování),
# přičemž jako proxy proměnná je použita z_p95.
compute_breaks_train <- function(train_df, proxy_col, n_class = 4, min_n_sp = 20) {
  stopifnot(proxy_col %in% names(train_df))
  
  global_br <- safe_quantile_breaks(train_df[[proxy_col]], n_class)
  
  sp_list <- list()
  spp <- unique(as.character(train_df$skupina))
  for (sp in spp) {
    x <- train_df[[proxy_col]][as.character(train_df$skupina) == sp]
    x <- x[is.finite(x)]
    if (length(x) < min_n_sp) {
      sp_list[[sp]] <- NULL
      next
    }
    sp_list[[sp]] <- safe_quantile_breaks(x, n_class)
  }
  
  list(global = global_br, by_species = sp_list)
}

# Aplikace výškových tříd:
# - třídy Hclass_q jsou určeny po dřevinách,
# - při nedostatku dat se použijí globální hranice,
# - současně je vytvořena interakční proměnná Hclass_sp.
apply_Hclass <- function(df_any, proxy_col, breaks_obj) {
  out <- df_any %>% mutate(skupina = factor(skupina))
  stopifnot(proxy_col %in% names(out))
  
  out$Hclass_q <- NA_character_
  
  global_br <- breaks_obj$global
  sp_brks   <- breaks_obj$by_species
  
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
        cut(out[[proxy_col]][idx], breaks = br, include.lowest = TRUE, labels = labs, right = TRUE)
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

# Výpočet validačních metrik predikce
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
  r2 <- if (ss_tot > 0) 1 - ss_res/ss_tot else NA_real_
  
  rng <- max(y_true) - min(y_true)
  nrmse_range <- if (rng > 0) rmse / rng else NA_real_
  mu <- mean(y_true)
  nrmse_mean <- if (mu != 0) rmse / mu else NA_real_
  
  data.frame(n=n, R2=r2, RMSE=rmse, MAE=mae, nRMSE_range=nrmse_range, nRMSE_mean=nrmse_mean)
}

# Generování náhodných stratifikovaných foldů podle proměnné 'skupina'
make_folds_stratified <- function(group, k = 10, seed = 42) {
  set.seed(seed)
  group <- as.factor(group)
  fold <- rep(NA_integer_, length(group))
  
  for (g in levels(group)) {
    idx <- which(group == g)
    if (length(idx) == 0) next
    idx <- sample(idx)
    fold[idx] <- rep(1:k, length.out = length(idx))
  }
  
  # Záložní řešení pro případ neočekávaných chybějících přiřazení
  if (anyNA(fold)) {
    na_idx <- which(is.na(fold))
    fold[na_idx] <- sample(rep(1:k, length.out = length(na_idx)))
  }
  
  fold
}

# Extrakce reziduální směrodatné odchylky modelu
get_sigma <- function(model) {
  if (inherits(model, "merMod")) return(as.numeric(sigma(model)))
  if (inherits(model, "lm")) return(as.numeric(summary(model)$sigma))
  NA_real_
}

# Bias-korigovaná zpětná transformace predikcí pro logaritmické modely
backtransform_pred <- function(target_name, eta, sigma_hat) {
  eta <- as.numeric(eta)
  out <- rep(NA_real_, length(eta))
  ok <- is.finite(eta)
  
  if (!(target_name %in% LOG_TARGETS)) {
    out[ok] <- eta[ok]
    return(out)
  }
  
  # exp(eta + 0.5*sigma^2)
  s2 <- as.numeric(sigma_hat)^2
  out[ok] <- exp(eta[ok] + 0.5 * s2)
  out
}

# Predikce lineárního prediktoru
predict_eta <- function(model, newdata) {
  if (inherits(model, "merMod")) {
    return(predict(model, newdata = newdata, re.form = NULL, allow.new.levels = TRUE))
  }
  predict(model, newdata = newdata)
}

# Filtrování kandidátních struktur náhodných efektů podle počtu dostupných úrovní
filter_random_candidates <- function(dtr, candidates) {
  ok <- c()
  n_sp <- nlevels(dtr$skupina)
  n_hc <- nlevels(dtr$Hclass_sp)
  
  for (rc in candidates) {
    if (rc == "LM") { ok <- c(ok, rc); next }
    if (grepl("skupina", rc)   && is.finite(n_sp) && n_sp < MIN_LEVELS_FACTOR) next
    if (grepl("Hclass_sp", rc) && is.finite(n_hc) && n_hc < MIN_LEVELS_HCLASS) next
    ok <- c(ok, rc)
  }
  ok
}

# Odhad modelu pro danou kombinaci fixních prediktorů a náhodné struktury
fit_model_candidate <- function(dtr, fixed_vars, rand_term) {
  fixed_part <- paste(fixed_vars, collapse = " + ")
  
  if (rand_term == "LM") {
    f <- as.formula(paste0("y_trans ~ ", fixed_part))
    m <- lm(f, data = dtr)
    return(list(model=m, formula=f, type="LM",
                singular=NA, varcorr=NA, aic=AIC(m), bic=BIC(m), logLik=as.numeric(logLik(m))))
  } else {
    f <- as.formula(paste0("y_trans ~ ", fixed_part, " + ", rand_term))
    m <- suppressWarnings(lmer(f, data = dtr, REML = FALSE))
    sing <- isSingular(m, tol = 1e-5)
    vc_txt <- paste(capture.output(print(VarCorr(m), comp = c("Variance","Std.Dev."))), collapse = "\n")
    return(list(model=m, formula=f, type="LMM",
                singular=sing, varcorr=vc_txt, aic=AIC(m), bic=BIC(m),
                logLik=as.numeric(logLik(m))))
  }
}

# --------------------------
# 3) Načtení datového souboru CSV a nahrazení nulových hodnot NA
# --------------------------
df <- read.csv(CSV_PATH, stringsAsFactors = FALSE, check.names = FALSE)
stopifnot(all(c("skupina", H_PROXY_COL) %in% names(df)))

df <- df %>% mutate(skupina = factor(skupina))
df$.row_id <- seq_len(nrow(df))

targets_present <- intersect(TARGETS, names(df))
if (length(targets_present) == 0) stop("V CSV nejsou žádné TARGETS, které očekávám.")

# U všech cílových proměnných jsou nulové hodnoty považovány za nepoužitelné a nahrazeny hodnotou NA
for (t in targets_present) {
  df[[t]][is.finite(df[[t]]) & df[[t]] == 0] <- NA
}

# --------------------------
# 4) Načtení fixních prediktorů z XLSX a parsování sloupce top10_predictors
# --------------------------
sheets <- excel_sheets(XLSX_PATH)

read_sheet_safe <- function(sh) {
  tryCatch(read_excel(XLSX_PATH, sheet = sh), error = function(e) NULL)
}

tabs <- lapply(sheets, read_sheet_safe)
tabs <- tabs[!vapply(tabs, is.null, logical(1))]

pick_table <- NULL
for (tab in tabs) {
  nms <- tolower(names(tab))
  has_target <- any(nms == "target")
  has_top10  <- any(nms == "top10_predictors")
  has_cap    <- any(nms %in% c("cap","hard_cap"))
  if (has_target && has_top10 && has_cap) { pick_table <- tab; break }
}

if (is.null(pick_table)) {
  stop("V XLSX jsem nenašel tabulku se sloupci: target + (cap/hard_cap) + top10_predictors.")
}

names(pick_table) <- tolower(names(pick_table))
cap_col <- if ("cap" %in% names(pick_table)) "cap" else "hard_cap"

fixed_map <- pick_table %>%
  transmute(
    target = as.character(target),
    cap    = as.integer(.data[[cap_col]]),
    top10_predictors = as.character(top10_predictors)
  ) %>%
  filter(!is.na(target), !is.na(cap), !is.na(top10_predictors)) %>%
  mutate(pred_list = str_split(top10_predictors, "\\s*,\\s*")) %>%
  unnest_longer(pred_list, values_to = "predictor") %>%
  mutate(predictor = str_trim(as.character(predictor))) %>%
  filter(predictor != "") %>%
  group_by(target, cap) %>%
  group_modify(~{
    k <- .y$cap[[1]]
    .x <- .x %>% distinct(predictor)
    head(.x, k)
  }) %>%
  ungroup() %>%
  select(target, cap, predictor)

if (nrow(fixed_map) == 0) {
  stop("fixed_map je prázdné – zkontroluj, že top10_predictors obsahuje seznam oddělený čárkami.")
}

get_fixed_vars <- function(target_name, cap_k) {
  fixed_map %>%
    filter(target == target_name, cap == cap_k) %>%
    pull(predictor) %>%
    unique()
}

caps_present <- sort(unique(fixed_map$cap))
message("Caps nalezené v XLSX: ", paste(caps_present, collapse = ", "))

# --------------------------
# 5) Vlastní evaluace modelových struktur
# --------------------------
summary_rows <- list()
cv_rows <- list()
all_results <- list()

targets_to_run <- intersect(targets_present, unique(fixed_map$target))

for (target_name in targets_to_run) {
  for (cap_k in caps_present) {
    
    fixed_vars <- get_fixed_vars(target_name, cap_k)
    if (length(fixed_vars) != cap_k) {
      message("SKIP: ", target_name, " cap=", cap_k, " (našlo se ", length(fixed_vars), " prediktorů)")
      next
    }
    
    missing <- setdiff(fixed_vars, names(df))
    if (length(missing) > 0) {
      message("SKIP: ", target_name, " cap=", cap_k,
              " chybí prediktory v CSV: ", paste(missing, collapse = ", "))
      next
    }
    
    message("\n==============================")
    message("TARGET: ", target_name, " | CAP: ", cap_k)
    message("FIXED:  ", paste(fixed_vars, collapse = " + "))
    message("Hclass proxy: ", H_PROXY_COL)
    message("==============================")
    
    # ---------- A) Odhad na celé množině (in-sample) ----------
    d_full <- df %>%
      select(.row_id, skupina, all_of(H_PROXY_COL), all_of(target_name), all_of(fixed_vars)) %>%
      filter(!is.na(.data[[target_name]]), !is.na(skupina))
    
    br_full <- compute_breaks_train(d_full, proxy_col = H_PROXY_COL, n_class = N_HCLASS, min_n_sp = MIN_N_PER_SPECIES)
    d_full <- apply_Hclass(d_full, proxy_col = H_PROXY_COL, breaks_obj = br_full)
    
    y_full <- TRANSFORM[[target_name]](d_full[[target_name]])
    oky <- is.finite(y_full)
    d_full <- d_full[oky, , drop=FALSE]
    d_full$y_trans <- y_full[oky]
    
    cc <- complete.cases(d_full[, fixed_vars, drop=FALSE], d_full$Hclass_sp, d_full$skupina, d_full$y_trans)
    d_full <- d_full[cc, , drop=FALSE]
    
    if (nrow(d_full) < MIN_N_FOLD) {
      message("SKIP full-fit: málo dat po čištění: ", nrow(d_full))
      next
    }
    
    cand_full <- filter_random_candidates(d_full, RAND_CANDIDATES)
    
    fits_full <- list()
    for (rc in cand_full) {
      fits_full[[rc]] <- tryCatch(fit_model_candidate(d_full, fixed_vars, rc), error = function(e) NULL)
    }
    fits_full <- fits_full[!vapply(fits_full, is.null, logical(1))]
    
    for (rc in names(fits_full)) {
      fi <- fits_full[[rc]]
      summary_rows[[length(summary_rows)+1]] <- data.frame(
        target = target_name,
        cap = cap_k,
        fixed = paste(fixed_vars, collapse = " + "),
        random = rc,
        type = fi$type,
        n_full = nrow(d_full),
        aic = fi$aic,
        bic = fi$bic,
        logLik = fi$logLik,
        singular = fi$singular,
        varcorr = ifelse(is.na(fi$varcorr), "", fi$varcorr),
        stringsAsFactors = FALSE
      )
    }
    
    # ---------- B) 10-fold cross-validace pro jednotlivé kandidátní struktury ----------
    # Foldy jsou vytvářeny pouze nad skutečně použitelnými řádky
    # po základní filtraci dle cílové proměnné, prediktorů, proxy výšky a proměnné skupina.
    base_cv <- df %>%
      select(.row_id, skupina, all_of(H_PROXY_COL), all_of(target_name), all_of(fixed_vars)) %>%
      filter(!is.na(.data[[target_name]]), !is.na(skupina), !is.na(.data[[H_PROXY_COL]])) %>%
      filter(if_all(all_of(fixed_vars), ~ !is.na(.x)))
    
    if (nrow(base_cv) < MIN_N_FOLD || nrow(base_cv) < K_FOLDS) {
      message("SKIP CV: málo dat po základní filtraci: ", nrow(base_cv))
      next
    }
    
    folds_cv <- make_folds_stratified(base_cv$skupina, k = K_FOLDS, seed = SEED)
    fold_lookup <- data.frame(.row_id = base_cv$.row_id, .fold = folds_cv)
    
    cv_for_candidate <- function(rand_term) {
      
      # Out-of-fold predikce jsou ukládány pouze pro řádky přítomné v base_cv
      oof <- base_cv %>%
        transmute(.row_id, y_true = .data[[target_name]], y_pred = NA_real_) %>%
        left_join(fold_lookup, by = ".row_id")
      
      for (k in 1:K_FOLDS) {
        
        test_ids  <- oof$.row_id[oof$.fold == k]
        train_ids <- oof$.row_id[oof$.fold != k]
        
        train_raw <- df[df$.row_id %in% train_ids, , drop=FALSE]
        test_raw  <- df[df$.row_id %in% test_ids,  , drop=FALSE]
        
        # Hranice tříd jsou vždy odvozeny pouze z trénovacích dat
        brks <- compute_breaks_train(train_raw, proxy_col = H_PROXY_COL, n_class = N_HCLASS, min_n_sp = MIN_N_PER_SPECIES)
        train <- apply_Hclass(train_raw, proxy_col = H_PROXY_COL, breaks_obj = brks)
        test  <- apply_Hclass(test_raw,  proxy_col = H_PROXY_COL, breaks_obj = brks)
        
        dtr <- train %>%
          select(.row_id, skupina, all_of(H_PROXY_COL), Hclass_q, Hclass_sp, all_of(target_name), all_of(fixed_vars)) %>%
          filter(!is.na(.data[[target_name]]), !is.na(skupina), !is.na(Hclass_sp))
        
        ytr <- TRANSFORM[[target_name]](dtr[[target_name]])
        oky <- is.finite(ytr)
        dtr <- dtr[oky, , drop=FALSE]
        dtr$y_trans <- ytr[oky]
        
        cc <- complete.cases(dtr[, fixed_vars, drop=FALSE], dtr$Hclass_sp, dtr$skupina, dtr$y_trans)
        dtr <- dtr[cc, , drop=FALSE]
        if (nrow(dtr) < MIN_N_FOLD) next
        
        cand_fold <- filter_random_candidates(dtr, c(rand_term))
        if (!(rand_term %in% cand_fold)) next
        
        fit <- tryCatch(fit_model_candidate(dtr, fixed_vars, rand_term), error = function(e) NULL)
        if (is.null(fit)) next
        
        dte <- test %>%
          select(.row_id, skupina, all_of(H_PROXY_COL), Hclass_q, Hclass_sp, all_of(target_name), all_of(fixed_vars)) %>%
          filter(!is.na(.data[[target_name]]), !is.na(skupina), !is.na(Hclass_sp))
        
        if (nrow(dte) == 0) next
        cc_te <- complete.cases(dte[, fixed_vars, drop=FALSE], dte$Hclass_sp, dte$skupina)
        dte <- dte[cc_te, , drop=FALSE]
        if (nrow(dte) == 0) next
        
        eta_hat <- tryCatch(predict_eta(fit$model, dte), error = function(e) rep(NA_real_, nrow(dte)))
        sig_hat <- get_sigma(fit$model)
        y_hat   <- backtransform_pred(target_name, eta_hat, sig_hat)
        
        oof_idx <- match(dte$.row_id, oof$.row_id)
        oof$y_pred[oof_idx] <- y_hat
      }
      
      met <- cv_metrics(oof$y_true, oof$y_pred)
      met$n_pred <- sum(is.finite(oof$y_true) & is.finite(oof$y_pred))
      met
    }
    
    for (rc in cand_full) {
      met <- cv_for_candidate(rc)
      cv_rows[[length(cv_rows)+1]] <- data.frame(
        target = target_name,
        cap = cap_k,
        fixed = paste(fixed_vars, collapse = " + "),
        random = rc,
        R2 = met$R2,
        RMSE = met$RMSE,
        MAE = met$MAE,
        nRMSE_range = met$nRMSE_range,
        nRMSE_mean = met$nRMSE_mean,
        n = met$n,
        n_pred = met$n_pred,
        stringsAsFactors = FALSE
      )
      
      message("CV: ", target_name, " cap=", cap_k, " | ", rc,
              " => R2=", round(met$R2, 3),
              " RMSE=", round(met$RMSE, 3),
              " n_pred=", met$n_pred, sep = "")
    }
    
    all_results[[paste0(target_name, "_cap", cap_k)]] <- list(
      target = target_name,
      cap = cap_k,
      fixed_vars = fixed_vars,
      fits_full = fits_full,
      meta = list(
        H_PROXY_COL = H_PROXY_COL,
        N_HCLASS = N_HCLASS,
        MIN_N_PER_SPECIES = MIN_N_PER_SPECIES,
        K_FOLDS = K_FOLDS,
        SEED = SEED,
        fold_scheme = "random stratified by skupina on usable rows",
        backtransform = "log-targets: exp(eta + 0.5*sigma^2)"
      )
    )
  }
}

# --------------------------
# 6) Uložení výstupů
# --------------------------
summary_table <- bind_rows(summary_rows)
cv_table      <- bind_rows(cv_rows)

write.csv(summary_table,
          file.path(OUT_DIR, "random_effects_in_sample_summary.csv"),
          row.names = FALSE)

write.csv(cv_table,
          file.path(OUT_DIR, "random_effects_CV10_summary.csv"),
          row.names = FALSE)

saveRDS(all_results,
        file.path(OUT_DIR, "random_effects_full_fits.rds"))

message("\nDONE.")
message("In-sample summary: ", file.path(OUT_DIR, "random_effects_in_sample_summary.csv"))
message("CV summary:        ", file.path(OUT_DIR, "random_effects_CV10_summary.csv"))
message("Full fit objects:  ", file.path(OUT_DIR, "random_effects_full_fits.rds"))
