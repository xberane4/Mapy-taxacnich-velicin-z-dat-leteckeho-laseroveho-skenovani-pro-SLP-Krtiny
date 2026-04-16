# ============================================================
# ÚČEL SCRIPTU
# ============================================================
# Tento script slouží k porovnání dvou alternativních sad
# výškových intervalů používaných při konstrukci regresních modelů,
# označených jako sada A a sada B. Cílem je vybrat vhodnější
# intervalovou strukturu pro jednotlivé kombinace skupiny dřevin
# a cílové porostní veličiny.
#
# Hodnocení není zaměřeno na výběr jednoho konkrétního nejlepšího
# modelu, ale na posouzení kvality celé intervalové struktury.
# Pro každý interval jsou proto agregovány informace o více
# kandidátních modelech a následně je vyhodnocena zejména:
# - použitelnost intervalu,
# - stabilita výsledků v rámci intervalu,
# - podíl modelů se zápornou hodnotou CV_R2,
# - úroveň křížově validačních metrik.
#
# Na základě intervalových diagnostik jsou následně vytvořeny:
# - souhrny pro obě porovnávané sady intervalů,
# - doporučení vhodnější sady pro každou kombinaci
#   skupina × cílová veličina,
# - datový soubor obsahující pouze modely z vítězné sady intervalů
#   pro další navazující výběr modelů.
# ============================================================


# ---------------------------
# 0) NAČTENÍ KNIHOVEN
# ---------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(readr)
})

# Pokud je vstup ve formátu XLSX, je možné použít balíček readxl:
# install.packages("readxl")
# library(readxl)


# ---------------------------
# 1) NASTAVENÍ VSTUPŮ A PARAMETRŮ
# ---------------------------
# Cesty ke vstupním souborům pro sadu A a sadu B
PATH_A <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/REG_MOD_PIL/INT_A_MOD_PIL.csv"
PATH_B <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/REG_MOD_PIL/INT_B_MOD_PIL.csv"

# Typ vstupního souboru: "csv" nebo "xlsx"
FILE_TYPE <- "csv"

# Výstupní složka
OUT_DIR <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/LIN_REG_MOD_PIL"

# Maximální povolený počet prediktorů v modelu
MAX_PRED <- 3

# Minimální počet ploch v intervalu
MIN_PLOTS <- 20

# Prahy stability vyjádřené pomocí IQR metriky CV_R2
IQR_STABLE <- 0.12
IQR_OK     <- 0.20

# Mezní podíl záporných hodnot CV_R2, nad kterým je interval
# považován za problematický z hlediska stability
NEG_SHARE_BAD <- 0.50

# Volba, zda ponechat pouze intervaly s alespoň jedním modelem,
# který má kladnou hodnotu CV_R2
REQUIRE_POSITIVE_CVR2 <- TRUE

# Váhy použité při výpočtu výsledného skóre sady intervalů
W_USABLE    <- 0.60
W_UNSTABLE  <- 0.60
W_IQR       <- 0.40
W_NEG_SHARE <- 0.30
W_CV_R2     <- 0.40
W_NRMSE     <- 0.20


# ---------------------------
# 2) POMOCNÉ FUNKCE
# ---------------------------

# Vytvoření výstupní složky, pokud ještě neexistuje
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------
# Funkce pro načtení vstupních dat a doplnění identifikace sady
# ------------------------------------------------------------
read_input <- function(path, set_id, file_type = "csv") {
  if (file_type == "csv") {
    dat <- read_csv(path, show_col_types = FALSE)
  } else if (file_type == "xlsx") {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Chceš číst XLSX, ale nemáš balík readxl. Nainstaluj: install.packages('readxl')")
    }
    dat <- readxl::read_excel(path)
  } else {
    stop("FILE_TYPE musí být 'csv' nebo 'xlsx'.")
  }
  dat %>% mutate(set_id = set_id)
}

# ------------------------------------------------------------
# Funkce pro kontrolu přítomnosti povinných sloupců
# ------------------------------------------------------------
require_cols <- function(df, cols) {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    stop("Chybí povinné sloupce: ", paste(missing, collapse = ", "))
  }
}

# ------------------------------------------------------------
# Funkce pro doplnění počtu prediktorů, pokud není přímo uveden
# ------------------------------------------------------------
add_n_pred_if_needed <- function(df) {
  if (!("n_pred" %in% names(df))) {
    if ("predictors" %in% names(df)) {
      df <- df %>%
        mutate(
          n_pred = case_when(
            is.na(predictors) ~ NA_real_,
            str_trim(predictors) == "" ~ 0,
            TRUE ~ (str_count(predictors, ",") + 1)
          )
        )
    } else {
      df <- df %>% mutate(n_pred = NA_real_)
    }
  }
  df
}

# ------------------------------------------------------------
# Funkce pro intervalovou diagnostiku agregovanou přes modely
# ------------------------------------------------------------
make_interval_diag <- function(df) {
  interval_diag <- df %>%
    group_by(set_id, skupina, target, Hclass, Hclass_id) %>%
    summarise(
      n_models = n(),
      n_plots  = first(n_plots),
      
      median_CV_R2      = median(CV_R2, na.rm = TRUE),
      IQR_CV_R2         = IQR(CV_R2, na.rm = TRUE),
      median_CV_nRMSE   = median(CV_nRMSE_mean, na.rm = TRUE),
      share_negative_R2 = mean(CV_R2 < 0, na.rm = TRUE),
      
      # Nejvyšší dosažená hodnota CV_R2 v rámci intervalu
      max_CV_R2 = suppressWarnings(max(CV_R2, na.rm = TRUE)),
      
      .groups = "drop"
    ) %>%
    mutate(
      # Ošetření případů, kdy jsou všechny hodnoty CV_R2 chybějící
      max_CV_R2 = ifelse(is.finite(max_CV_R2), max_CV_R2, NA_real_),
      
      sample_flag = case_when(
        n_plots >= 40 ~ "HIGH",
        n_plots >= 25 ~ "MEDIUM",
        n_plots >= MIN_PLOTS ~ "LOW",
        TRUE ~ "EXCLUDE"
      ),
      stability_flag = case_when(
        IQR_CV_R2 <= IQR_STABLE ~ "STABLE",
        IQR_CV_R2 <= IQR_OK     ~ "OK",
        TRUE ~ "UNSTABLE"
      ),
      
      # Interval je považován za použitelný, pokud:
      # - splňuje minimální počet ploch,
      # - není vyhodnocen jako nestabilní,
      # - nemá nadlimitní podíl záporných CV_R2,
      # - a volitelně obsahuje alespoň jeden model s kladným CV_R2
      usable = (sample_flag != "EXCLUDE") &
        (stability_flag != "UNSTABLE") &
        (share_negative_R2 < NEG_SHARE_BAD) &
        (!REQUIRE_POSITIVE_CVR2 | (!is.na(max_CV_R2) & max_CV_R2 > 0))
    ) %>%
    filter(sample_flag != "EXCLUDE") %>%
    { if (REQUIRE_POSITIVE_CVR2) filter(., !is.na(max_CV_R2) & max_CV_R2 > 0) else . } %>%
    arrange(set_id, skupina, target, Hclass_id)
  
  interval_diag
}

# ------------------------------------------------------------
# Funkce pro vytvoření souhrnu po sadách intervalů
# ------------------------------------------------------------
make_set_summary <- function(interval_diag) {
  interval_diag %>%
    group_by(set_id, skupina, target) %>%
    summarise(
      n_intervals   = n(),
      n_plots_total = sum(n_plots, na.rm = TRUE),
      
      share_usable   = mean(usable),
      share_unstable = mean(stability_flag == "UNSTABLE"),
      share_neg_bad  = mean(share_negative_R2 >= NEG_SHARE_BAD),
      
      # Vážené charakteristiky, kde váhou je počet ploch v intervalu
      w_median_CV_R2    = weighted.mean(median_CV_R2, w = n_plots, na.rm = TRUE),
      w_median_CV_nRMSE = weighted.mean(median_CV_nRMSE, w = n_plots, na.rm = TRUE),
      w_IQR_CV_R2       = weighted.mean(IQR_CV_R2, w = n_plots, na.rm = TRUE),
      w_share_negative  = weighted.mean(share_negative_R2, w = n_plots, na.rm = TRUE),
      
      .groups = "drop"
    ) %>%
    mutate(
      # Výsledné skóre s důrazem na použitelnost a stabilitu intervalů
      score =
        W_USABLE    * share_usable -
        W_UNSTABLE  * share_unstable -
        W_IQR       * w_IQR_CV_R2 -
        W_NEG_SHARE * w_share_negative +
        W_CV_R2     * w_median_CV_R2 -
        W_NRMSE     * w_median_CV_nRMSE
    ) %>%
    arrange(skupina, target, desc(score))
}

# ------------------------------------------------------------
# Funkce pro výběr lepší sady intervalů pro skupina × target
# ------------------------------------------------------------
choose_best_set <- function(set_summary) {
  set_summary %>%
    group_by(skupina, target) %>%
    slice_max(score, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(
      skupina, target,
      best_set = set_id,
      score,
      share_usable, share_unstable,
      w_median_CV_R2, w_median_CV_nRMSE, w_IQR_CV_R2, w_share_negative,
      n_intervals, n_plots_total
    ) %>%
    arrange(skupina, target)
}


# ---------------------------
# 3) NAČTENÍ VSTUPNÍCH DAT
# ---------------------------
dfA <- read_input(PATH_A, "A", FILE_TYPE)
dfB <- read_input(PATH_B, "B", FILE_TYPE)
df  <- bind_rows(dfA, dfB)

# Kontrola přítomnosti povinných sloupců
required <- c("skupina", "target", "Hclass", "Hclass_id", "n_plots", "CV_R2", "CV_nRMSE_mean")
require_cols(df, required)

# Doplnění počtu prediktorů, pokud není ve vstupu přímo uveden
df <- add_n_pred_if_needed(df)

# Filtrace modelů podle maximálního počtu prediktorů
# Pokud je n_pred chybějící u všech řádků, filtr se fakticky neuplatní
df_filtered <- df %>%
  filter(is.na(n_pred) | n_pred <= MAX_PRED)

# ---------------------------
# 4) INTERVALOVÁ DIAGNOSTIKA
# ---------------------------
interval_diag <- make_interval_diag(df_filtered)

# ---------------------------
# 5) SOUHRN SAD A VOLBA LEPŠÍ SADY
# ---------------------------
set_summary <- make_set_summary(interval_diag)
best_set    <- choose_best_set(set_summary)

# ---------------------------
# 6) ULOŽENÍ VÝSTUPNÍCH SOUBORŮ
# ---------------------------
write_csv(interval_diag, file.path(OUT_DIR, "interval_diagnostics_A_B.csv"))
write_csv(set_summary,   file.path(OUT_DIR, "set_summary_A_B.csv"))
write_csv(best_set,      file.path(OUT_DIR, "best_set_by_species_target.csv"))

# ---------------------------
# 7) VOLITELNÝ PŘEHLED V KONZOLI
# ---------------------------
cat("\n=== HOTOVO ===\n")
cat("Výstupy uložené do:", OUT_DIR, "\n\n")

cat("Top 10 nejlepších kombinací (skupina × target) dle skóre:\n")
print(best_set %>% arrange(desc(score)) %>% head(10))

cat("\nPočet případů, kdy vyhrála sada A vs B:\n")
print(best_set %>% count(best_set))

# ---------------------------
# 8) VYTVOŘENÍ SOUBORU 'DF_BEST'
# ---------------------------
# Tento krok vytvoří datový soubor obsahující pouze modely
# z vítězné sady intervalů pro každou kombinaci skupina × target.
# ------------------------------------------------------------
df_best <- df_filtered %>%
  inner_join(best_set, by = c("skupina", "target")) %>%
  filter(set_id == best_set) %>%
  select(-best_set, -score)

write_csv(df_best, file.path(OUT_DIR, "models_only_best_interval_set.csv"))

cat("\nVytvořen soubor models_only_best_interval_set.csv (jen z vítězné sady intervalů)\n")
