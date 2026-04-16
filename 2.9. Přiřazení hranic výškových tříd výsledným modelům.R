# ============================================================
# ÚČEL SKRIPTU
# ============================================================
# Skript slouží k doplnění dolní a horní hranice výškových intervalů
# do tabulky vybraných modelů. Propojení je provedeno na základě
# identifikátoru sady intervalů, dřevinné skupiny a pořadí intervalu.
# Výstupem je rozšířená tabulka modelů doplněná o proměnné interval_low
# a interval_high, uložená ve formátu XLSX nebo CSV.
# ============================================================

# ============================================================
# DOPLNĚNÍ HRANIC INTERVALŮ (low/high) DO TABULKY MODELŮ
#
# Propojení tabulek probíhá podle kombinace:
# - set_id (označení sady intervalů, např. A/B),
# - skupina,
# - Hclass_id (pořadí intervalu).
#
# VSTUP:
#   1) Vysky_Hierarch_PIL.xlsx
#      (obsahuje definici intervalů: schema, skupina, low, high, Pořadí)
#   2) models_with_collinearity.csv
#      (obsahuje tabulku vybraných modelů: set_id, skupina, Hclass_id, ...)
#
# VÝSTUP:
#   - XLSX soubor, pokud je dostupný balíček openxlsx,
#   - jinak CSV soubor.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(readr)
})

# ------------------------------------------------------------
# 0) Nastavení vstupních a výstupních cest
# ------------------------------------------------------------
INTERVAL_PATH <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/REG_MOD_PIL/Vysky_Hierarch_PIL.xlsx"
MODELS_PATH   <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/REG_MOD_PIL/multicollinearity_check_final_balik/models_with_collinearity.csv"

OUT_XLSX <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/REG_MOD_PIL/Vyber_modelu_with_bounds_PIL.xlsx"
OUT_CSV  <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/REG_MOD_PIL/Vyber_modelu_with_bounds_PIL.csv"

# ------------------------------------------------------------
# 1) Načtení vstupních dat
# ------------------------------------------------------------
intervals_raw <- read_excel(INTERVAL_PATH)

# Soubor s modely je načítán funkcí read_csv2, protože:
# - používá středník jako oddělovač,
# - odpovídá běžnému formátu exportů používaných v českém prostředí.
models_raw <- read_csv2(
  MODELS_PATH,
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

# ------------------------------------------------------------
# 2) Kontrola přítomnosti povinných sloupců
# ------------------------------------------------------------
req_int <- c("schema", "skupina", "low", "high", "Pořadí")
req_mod <- c("set_id", "skupina", "Hclass_id")

miss_int <- setdiff(req_int, names(intervals_raw))
miss_mod <- setdiff(req_mod, names(models_raw))

if (length(miss_int) > 0) stop("V tabulce intervalů chybí sloupce: ", paste(miss_int, collapse = ", "))
if (length(miss_mod) > 0) stop("V tabulce modelů chybí sloupce: ", paste(miss_mod, collapse = ", "))

# ------------------------------------------------------------
# 3) Příprava tabulky intervalů pro propojení
# ------------------------------------------------------------
intervals <- intervals_raw %>%
  transmute(
    set_id    = as.character(schema),      # identifikátor sady intervalů (A/B)
    skupina   = as.character(skupina),
    Hclass_id = as.integer(`Pořadí`),      # pořadí intervalu
    interval_low  = suppressWarnings(as.numeric(low)),
    interval_high = suppressWarnings(as.numeric(high))
  )

# ------------------------------------------------------------
# 4) Propojení tabulky modelů s tabulkou intervalů
# ------------------------------------------------------------
models_out <- models_raw %>%
  mutate(
    set_id    = as.character(set_id),
    skupina   = as.character(skupina),
    Hclass_id = suppressWarnings(as.integer(Hclass_id))
  ) %>%
  left_join(intervals, by = c("set_id", "skupina", "Hclass_id"))

# ------------------------------------------------------------
# 5) Kontrola nenapárovaných kombinací
# ------------------------------------------------------------
missing_rows <- models_out %>%
  filter(is.na(interval_low) | is.na(interval_high)) %>%
  distinct(set_id, skupina, Hclass_id) %>%
  arrange(set_id, skupina, Hclass_id)

if (nrow(missing_rows) > 0) {
  warning(
    "Nepodařilo se doplnit hranice pro ", nrow(missing_rows),
    " unikátních kombinací (set_id, skupina, Hclass_id). Tyto kombinace jsou vypsány níže:"
  )
  print(missing_rows)
}

# ------------------------------------------------------------
# 5b) Ošetření nečíselných speciálních hodnot
# ------------------------------------------------------------
# Hodnoty Inf, -Inf a NaN nejsou při exportu do Excelu vhodně
# interpretovány. Z tohoto důvodu jsou ve všech numerických sloupcích
# nahrazeny hodnotou NA.
models_out <- models_out %>%
  mutate(across(where(is.numeric), ~ ifelse(is.finite(.x), .x, NA_real_)))

# ------------------------------------------------------------
# 6) Export výstupní tabulky
# ------------------------------------------------------------
# Preferovaným formátem je XLSX prostřednictvím balíčku openxlsx.
# Pokud tento balíček není dostupný, bude tabulka uložena jako CSV2,
# tedy se středníkem jako oddělovačem a desetinnou čárkou.
has_openxlsx <- requireNamespace("openxlsx", quietly = TRUE)

if (has_openxlsx) {
  openxlsx::write.xlsx(models_out, OUT_XLSX, overwrite = TRUE)
  cat("Hotovo. Výstup byl uložen ve formátu XLSX do: ", OUT_XLSX, "\n")
} else {
  write_csv2(models_out, OUT_CSV)
  cat("Balíček openxlsx není nainstalován, výstup byl proto uložen ve formátu CSV do: ", OUT_CSV, "\n")
  