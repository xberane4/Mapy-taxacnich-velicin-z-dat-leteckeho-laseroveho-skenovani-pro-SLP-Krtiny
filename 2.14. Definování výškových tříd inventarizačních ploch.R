# ============================================================
# ÚČEL SKRIPTU
# Skript slouží k vytvoření nového sloupce Hclass_sp ve vstupní
# tabulce zkusných ploch na základě kombinace dřevinné skupiny
# ("skupina") a hodnoty výškové proxy veličiny z_p95. Pro každou
# skupinu jsou ve skriptu ručně definovány tři intervaly, podle
# nichž je následně každé ploše přiřazen odpovídající kategoriální
# label. Výstupem je rozšířený datový soubor využitelný v dalším
# modelování.
# ============================================================

# ============================================================
# VYTVOŘENÍ SLOUPCE Hclass_sp PODLE PROMĚNNÝCH "skupina" A "z_p95"
# - pro každou dřevinnou skupinu jsou přímo ve skriptu definovány
#   3 intervaly
# - výsledkem je nový sloupec Hclass_sp
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  library(stringr)
})

# ------------------------------------------------------------
# 0) CESTY K SOUBORŮM
# ------------------------------------------------------------
INPUT_PATH  <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/LLM_X20/sample_plots_REPLACED_BY_X20.csv"
OUTPUT_PATH <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/LLM_X20/sample_plots_REPLACED_BY_X20_HclassSP.csv"

# ------------------------------------------------------------
# 1) DEFINICE INTERVALŮ
# ------------------------------------------------------------
# Uprav hranice intervalů podle potřeby.
# Každá hodnota proměnné "skupina" musí mít definovány právě
# 3 intervaly.
#
# Logika intervalů:
# [low, high) = dolní mez včetně, horní mez bez
# poslední interval může mít horní mez high = Inf
#
# Textový label lze pojmenovat libovolně.
# Pro zachování přehlednosti je vhodné používat jednotný formát,
# například:
# "SM_1", "SM_2", "SM_3"
# nebo
# "SM_z_p95_[0;15)", ...
# ------------------------------------------------------------

intervals <- tribble(
  ~skupina, ~class_id, ~low,   ~high,  ~label,
  
  # SM
  "SM",     1,         0,      15.7457,     "SM_1",
  "SM",     2,         15.7457,     22.9403,     "SM_2",
  "SM",     3,         22.9403,     Inf,    "SM_3",
  
  # BK
  "BK",     1,         0,      22.1824,     "BK_1",
  "BK",     2,         22.1824,     27.533067,     "BK_2",
  "BK",     3,         27.533067,     Inf,    "BK_3",
  
  # DBZ
  "DBZ",     1,         0,      17.143633,     "DBZ_1",
  "DBZ",     2,         17.143633,     20.522117,     "DBZ_2",
  "DBZ",     3,         20.522117,     Inf,    "DBZ_3",
  
  # MD
  "MD",     1,         0,      18.629033,     "MD_1",
  "MD",     2,         18.629033,     27.448733,     "MD_2",
  "MD",     3,         27.448733,     Inf,    "MD_3",
  
  # BO
  "BO",     1,         0,      16.398783,     "BO_1",
  "BO",     2,         16.398783,     22.354217,     "BO_2",
  "BO",     3,         22.354217,     Inf,    "BO_3",
  
  # ostatní jehličnaté
  "ostatní jehličnaté", 1,     0,      22.037667,     "OJ_1",
  "ostatní jehličnaté", 2,     22.037667,     28.519133,     "OJ_2",
  "ostatní jehličnaté", 3,     28.519133,     Inf,    "OJ_3",
  
  # ostatní listnaté
  "ostatní listnaté", 1,       0,      19.649867,     "OL_1",
  "ostatní listnaté", 2,       19.649867,     25.2018,     "OL_2",
  "ostatní listnaté", 3,       25.2018,     Inf,    "OL_3"
)

# ------------------------------------------------------------
# 2) KONTROLA DEFINICE INTERVALŮ
# ------------------------------------------------------------
check_intervals <- intervals %>%
  count(skupina, name = "n_intervals")

if (any(check_intervals$n_intervals != 3)) {
  stop("Každá hodnota proměnné 'skupina' musí mít v tabulce intervals definovány přesně 3 intervaly.")
}

# ------------------------------------------------------------
# 3) FUNKCE PRO BEZPEČNÝ PŘEVOD NA ČÍSELNÝ TYP
# ------------------------------------------------------------
to_num <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x <- str_replace_all(x, ",", ".")
  suppressWarnings(as.numeric(x))
}

# ------------------------------------------------------------
# 4) NAČTENÍ VSTUPNÍCH DAT
# ------------------------------------------------------------
df <- read_csv(INPUT_PATH, show_col_types = FALSE)

# ------------------------------------------------------------
# 5) KONTROLA POVINNÝCH SLOUPCŮ
# ------------------------------------------------------------
required_cols <- c("skupina", "z_p95")
missing_cols <- setdiff(required_cols, names(df))

if (length(missing_cols) > 0) {
  stop(
    paste0(
      "Ve vstupním souboru chybí následující povinné sloupce: ",
      paste(missing_cols, collapse = ", ")
    )
  )
}

# ------------------------------------------------------------
# 6) ÚPRAVA DATOVÝCH TYPŮ
# ------------------------------------------------------------
df <- df %>%
  mutate(
    skupina = as.character(skupina),
    z_p95   = to_num(z_p95)
  )

# ------------------------------------------------------------
# 7) PŘIŘAZENÍ Hclass_sp
# ------------------------------------------------------------
# Postup:
# - provede se spojení tabulky podle proměnné "skupina"
# - ponechají se pouze řádky, v nichž hodnota z_p95 spadá
#   do intervalu [low, high)
# - pokud řádek nespadne do žádného intervalu, zůstane
#   výsledná hodnota Hclass_sp prázdná (NA)
# ------------------------------------------------------------

df_long <- df %>%
  mutate(.row_id = row_number()) %>%
  left_join(intervals, by = "skupina") %>%
  filter(!is.na(z_p95)) %>%
  filter(z_p95 >= low & z_p95 < high) %>%
  group_by(.row_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(.row_id, Hclass_sp = label)

df_out <- df %>%
  mutate(.row_id = row_number()) %>%
  left_join(df_long, by = ".row_id") %>%
  select(-.row_id)

# ------------------------------------------------------------
# 8) VAROVÁNÍ PRO NEPŘIŘAZENÉ PLOCHY
# ------------------------------------------------------------
n_na <- sum(is.na(df_out$Hclass_sp))

if (n_na > 0) {
  warning(
    paste0(
      "Pozor: ", n_na,
      " řádkům nebyla přiřazena hodnota Hclass_sp. ",
      "Zkontroluj hodnoty ve sloupcích 'skupina', 'z_p95' ",
      "a nastavení definovaných intervalů."
    )
  )
}

# ------------------------------------------------------------
# 9) KONTROLNÍ VÝPIS
# ------------------------------------------------------------
cat("\n--- KONTROLA PŘIŘAZENÍ Hclass_sp ---\n")
print(table(df_out$skupina, df_out$Hclass_sp, useNA = "ifany"))

# ------------------------------------------------------------
# 10) ULOŽENÍ VÝSTUPU
# ------------------------------------------------------------
write_csv(df_out, OUTPUT_PATH, na = "")

cat("\nHotovo.\n")
cat("Vstup:  ", INPUT_PATH, "\n")
cat("Výstup: ", OUTPUT_PATH, "\n")
