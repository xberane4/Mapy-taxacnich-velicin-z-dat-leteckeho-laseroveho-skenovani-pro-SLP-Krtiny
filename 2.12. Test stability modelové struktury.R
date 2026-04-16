# ============================================================
# ÚČEL SKRIPTU
# Skript slouží k vyhodnocení stability výběru prediktorů a
# stability použité struktury náhodných efektů napříč scénáři
# hierarchického modelování. Na základě uložených výsledků
# křížové validace pro jednotlivé scénáře vypočítává souhrnné
# ukazatele koncentrace výběru prediktorů, efektivního využití
# foldů a konzistence struktury modelů. Výstupem je souhrnná
# tabulka stability pro další porovnání scénářů.
# ============================================================

# ============================================================
# STABILITA PREDIKTORŮ
# - načítá všechny podsložky v OUT_ROOT, které obsahují soubor
#   CV_results.rds
# - pro každou cílovou veličinu vypočítává stabilitu výběru
#   prediktorů:
#   * effective_folds = počet foldů, které byly reálně využity
#     (odvozený jako total_selected / cap)
#   * topk_slot_share (0..1) = podíl prediktorových „slotů“
#     ve foldu obsazených top-k prediktory
#       - např. při cap = 3 udává top1_slot_share průměrný
#         podíl slotů obsazených nejčastějším prediktorem
#       - top3_slot_share vyjadřuje, do jaké míry je výběr
#         koncentrován do tří nejčastějších prediktorů
#   * pred_unique = počet různých prediktorů, které se objevily
#   * concentration = HHI index, Shannonova entropie a
#     normalizovaná entropie (0..1)
#   * stabilitu struktury náhodných efektů
#     (pevně definované úrovně RE_LEVELS + re_mode)
# - doplňuje metadata scénáře
#   (HCLASS_SOURCE / HCLASS_SOURCE_COL, pokud jsou přítomna v notes)
# - ukládá výstupní tabulku STABILITY_SUMMARY.csv
# ============================================================

# --------------------------
# 0) NASTAVENÍ CESTY
# --------------------------
OUT_ROOT <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/LLM_PIL/SMISENY_MODEL_SCENARIOS_PIL"  # UPRAV

# --------------------------
# 1) POMOCNÉ FUNKCE
# --------------------------
safe_num <- function(x) ifelse(is.finite(x), x, NA_real_)

# Výpočet stability z agregovaných četností
# (table(prediktor -> četnost výskytu))
#
# POZNÁMKA:
# Bez seznamu prediktorů po jednotlivých foldech nelze přesně
# určit „podíl foldů, ve kterých se prediktor objevil“.
# Lze však korektně vypočítat tzv. slot-share (0..1), tedy
# podíl všech prediktorových slotů ve foldech obsazených
# top-k prediktory.
freq_stability <- function(freq_table, cap) {
  if (length(freq_table) == 0 || is.na(cap) || cap <= 0) {
    return(list(
      total_selected = if (length(freq_table) == 0) 0 else sum(as.numeric(freq_table), na.rm = TRUE),
      effective_folds = NA_real_,
      effective_folds_rounded = NA_real_,
      per_fold_selected = NA_real_,
      pred_unique = if (length(freq_table) == 0) 0 else length(freq_table),
      top1 = NA_character_,
      top1_count = NA_real_,
      top1_slot_share = NA_real_,
      top2_slot_share = NA_real_,
      top3_slot_share = NA_real_,
      top5_slot_share = NA_real_,
      HHI = NA_real_,
      shannon = NA_real_,
      shannon_norm = NA_real_
    ))
  }
  
  freq <- as.numeric(freq_table)
  names(freq) <- names(freq_table)
  total_selected <- sum(freq, na.rm = TRUE)
  
  effective_folds <- total_selected / cap
  eff_round <- if (is.finite(effective_folds)) round(effective_folds) else NA_real_
  per_fold_selected <- safe_num(total_selected / effective_folds)  # mělo by se přibližně rovnat cap
  
  ord <- order(freq, decreasing = TRUE)
  freq_sorted <- freq[ord]
  names_sorted <- names(freq_sorted)
  
  top1 <- names_sorted[1]
  top1_count <- freq_sorted[1]
  
  # Výpočet slot-share (0..1):
  # určuje, kolik z celkového počtu slotů (cap * effective_folds)
  # obsadilo top-k prediktorů
  topk_slot_share <- function(k) {
    if (!is.finite(effective_folds) || effective_folds <= 0) return(NA_real_)
    kk <- min(k, length(freq_sorted))
    safe_num(sum(freq_sorted[1:kk]) / (cap * effective_folds))
  }
  
  p <- freq_sorted / total_selected
  HHI <- safe_num(sum(p^2))
  shannon <- safe_num(-sum(p * log(p)))
  shannon_norm <- if (is.finite(shannon) && length(freq_sorted) > 1) safe_num(shannon / log(length(freq_sorted))) else NA_real_
  
  list(
    total_selected = total_selected,
    effective_folds = effective_folds,
    effective_folds_rounded = eff_round,
    per_fold_selected = per_fold_selected,
    pred_unique = length(freq_sorted),
    top1 = top1,
    top1_count = top1_count,
    top1_slot_share = topk_slot_share(1),
    top2_slot_share = topk_slot_share(2),
    top3_slot_share = topk_slot_share(3),
    top5_slot_share = topk_slot_share(5),
    HHI = HHI,
    shannon = shannon,
    shannon_norm = shannon_norm
  )
}

RE_LEVELS <- c(
  "(1|skupina) + (1|Hclass_sp)",
  "(1|skupina)",
  "(1|Hclass_sp)",
  "LM"
)

re_props_fixed <- function(re_table) {
  out <- list()
  if (length(re_table) == 0) {
    out$re_total <- 0
    out$re_mode <- NA_character_
    out$re_mode_share <- NA_real_
    for (lv in RE_LEVELS) {
      key <- paste0("re_share__", gsub("[^A-Za-z0-9]+", "_", lv))
      out[[key]] <- NA_real_
    }
    return(out)
  }
  
  v <- as.numeric(re_table)
  n <- sum(v, na.rm = TRUE)
  out$re_total <- n
  
  re_mode <- if (n > 0) names(re_table)[which.max(v)] else NA_character_
  out$re_mode <- re_mode
  out$re_mode_share <- if (n > 0) safe_num(max(v) / n) else NA_real_
  
  for (lv in RE_LEVELS) {
    key <- paste0("re_share__", gsub("[^A-Za-z0-9]+", "_", lv))
    if (n > 0 && lv %in% names(re_table)) {
      out[[key]] <- safe_num(as.numeric(re_table[lv]) / n)
    } else {
      out[[key]] <- NA_real_
    }
  }
  out
}

# Bezpečné převzetí hodnot z objektu notes
note_chr <- function(x, default = NA_character_) {
  if (is.null(x)) return(default)
  if (length(x) == 0) return(default)
  as.character(x)[1]
}
note_num <- function(x, default = NA_real_) {
  if (is.null(x)) return(default)
  if (length(x) == 0) return(default)
  suppressWarnings(as.numeric(x)[1])
}

# --------------------------
# 2) VYHLEDÁNÍ VŠECH SOUBORŮ CV_results.rds
# --------------------------
scenario_dirs <- list.dirs(OUT_ROOT, recursive = FALSE, full.names = TRUE)
rds_paths <- file.path(scenario_dirs, "CV_results.rds")
rds_paths <- rds_paths[file.exists(rds_paths)]

if (length(rds_paths) == 0) {
  stop("Nenašel jsem žádné CV_results.rds v podsložkách OUT_ROOT: ", OUT_ROOT)
}

cat("Found scenarios:", length(rds_paths), "\n")

# --------------------------
# 3) VYHODNOCENÍ SCÉNÁŘŮ A VÝPOČET STABILITY
# --------------------------
rows <- list()

for (p in rds_paths) {
  scen_dir <- dirname(p)
  scen_id <- basename(scen_dir)
  
  res <- readRDS(p)  # seznam výsledků po jednotlivých targetech
  
  for (target_name in names(res)) {
    r <- res[[target_name]]
    
    # hard cap počtu prediktorů
    cap <- note_num(r$notes$hard_cap, default = NA_real_)
    if (is.finite(cap)) cap <- as.integer(cap) else cap <- NA_integer_
    
    # CV metriky
    cv <- r$cv
    R2 <- if (!is.null(cv$R2)) as.numeric(cv$R2) else NA_real_
    RMSE <- if (!is.null(cv$RMSE)) as.numeric(cv$RMSE) else NA_real_
    MAE <- if (!is.null(cv$MAE)) as.numeric(cv$MAE) else NA_real_
    nRMSE_mean <- if (!is.null(cv$nRMSE_mean)) as.numeric(cv$nRMSE_mean) else NA_real_
    n_obs <- if (!is.null(cv$n)) as.numeric(cv$n) else NA_real_
    n_pred <- if (!is.null(cv$n_pred)) as.numeric(cv$n_pred) else NA_real_
    
    # metadata scénáře z notes (pokud existují)
    cor_cutoff <- note_num(r$notes$cor_cutoff, default = NA_real_)
    n_hclass <- note_num(r$notes$n_hclass, default = NA_real_)
    leaps_criterion <- note_chr(r$notes$leaps_criterion, default = NA_character_)
    hclass_source <- note_chr(r$notes$hclass_source, default = NA_character_)
    hclass_source_col <- note_chr(r$notes$hclass_source_col, default = NA_character_)
    
    # textový přehled nejčastěji vybíraných prediktorů
    top10 <- note_chr(r$notes$top_selected, default = "")
    
    # stabilita prediktorů na základě selected_freq
    sf <- r$selected_freq
    stab <- freq_stability(sf, cap)
    
    # rozdělení použitých struktur náhodných efektů
    re <- r$used_random
    reinfo <- re_props_fixed(re)
    
    row <- c(
      list(
        scenario_id = scen_id,
        target = target_name,
        cap = cap,
        COR_CUTOFF = cor_cutoff,
        N_HCLASS = n_hclass,
        LEAPS_CRITERION = leaps_criterion,
        HCLASS_SOURCE = hclass_source,
        HCLASS_SOURCE_COL = hclass_source_col,
        R2 = R2,
        RMSE = RMSE,
        MAE = MAE,
        nRMSE_mean = nRMSE_mean,
        n_obs = n_obs,
        n_pred = n_pred,
        top10_predictors = top10
      ),
      stab,
      reinfo
    )
    
    rows[[length(rows) + 1]] <- as.data.frame(row, stringsAsFactors = FALSE)
  }
}

stability_df <- do.call(rbind, rows)

# --------------------------
# 4) DOPLŇKOVÉ PŘÍZNAKY STABILITY
# --------------------------
# Stabilní jádro prediktorů:
# top1_slot_share >= 0.5 znamená, že nejčastější prediktor
# obsazuje alespoň 50 % všech prediktorových slotů napříč foldy
stability_df$flag_pred_stable_top1_slot <- ifelse(
  is.finite(stability_df$top1_slot_share) & stability_df$top1_slot_share >= 0.5,
  TRUE, FALSE
)

# Vyšší variabilita výběru prediktorů:
# orientačně je za vysoký počet považováno alespoň 12 různých prediktorů
stability_df$flag_many_predictors <- ifelse(
  is.finite(stability_df$pred_unique) & stability_df$pred_unique >= 12,
  TRUE, FALSE
)

# Konzistence náhodné struktury:
# re_mode_share >= 0.8 značí, že byla téměř vždy použita stejná struktura
stability_df$flag_re_consistent <- ifelse(
  is.finite(stability_df$re_mode_share) & stability_df$re_mode_share >= 0.8,
  TRUE, FALSE
)

# --------------------------
# 5) ULOŽENÍ VÝSTUPU
# --------------------------
out_csv <- file.path(OUT_ROOT, "STABILITY_SUMMARY.csv")
write.csv(stability_df, out_csv, row.names = FALSE)

cat("\nSaved:\n", out_csv, "\n", sep = "")

# --------------------------
# 6) DOPLŇKOVÝ RYCHLÝ PŘEHLED NEJSTABILNĚJŠÍCH VARIANT
# --------------------------
cat("\n--- Quick view: best stability per target (top1_slot_share desc, then RMSE) ---\n")
for (tg in unique(stability_df$target)) {
  sub <- stability_df[stability_df$target == tg, ]
  sub <- sub[order(-sub$top1_slot_share, sub$RMSE), ]
  cat("\nTARGET:", tg, "\n")
  print(utils::head(sub[, c(
    "scenario_id","cap","HCLASS_SOURCE","HCLASS_SOURCE_COL",
    "R2","RMSE","top1","top1_slot_share","pred_unique","effective_folds_rounded",
    "re_mode","re_mode_share"
  )], 5))
}

cat("\nDONE.\n")
