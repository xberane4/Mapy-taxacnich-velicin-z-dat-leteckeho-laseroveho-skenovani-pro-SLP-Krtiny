
  #============================================================
  # ÚČEL SKRIPTU
  # Skript slouží k agregaci predikovaných taxačních veličin z úrovně
  # pixelů na úroveň hospodářských modelů (MOD) s využitím plošných vah.
  # Pro každou kategorii MOD vypočítává celkovou zastoupenou plochu,
  # vážené průměry výškových veličin a vážené součty zásobních veličin.
  # Výstupem je souhrnná tabulka uložená ve formátech CSV a případně
  # také XLSX pro další analýzu a interpretaci výsledků.
  # ============================================================

# Případná instalace balíčků:
# install.packages(c("readr", "dplyr"))

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

# 

# ============================================================
# SOUHRNNÁ AGREGACE PREDIKOVANÝCH VELIČIN NA ÚROVEŇ MOD
# - vstupem je pixelová tabulka s predikovanými taxačními veličinami
# - agregace probíhá samostatně pro všechny existující sloupce MOD_*
# - výškové veličiny jsou počítány jako plošně vážený průměr
# - ostatní taxační veličiny jsou počítány jako plošně vážený součet
# - výstupem je souhrnná tabulka pro jednotlivé kategorie MOD
# ============================================================

# ============================================================
# 0) NASTAVENÍ CEST K SOUBORŮM
# ============================================================
INPUT_CSV   <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/LLM_PIL_pixels.csv"
OUTPUT_CSV  <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/LLM_PIL/MOD_summary_weighted.csv"
OUTPUT_XLSX <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/LLM_PIL/MOD_summary_weighted.xlsx"

PIXEL_AREA_COL <- "Shape_Area"

# ============================================================
# 1) POMOCNÉ FUNKCE
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
  ok <- is.finite(x) & is.finite(w) & !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

clean_names <- function(nm) {
  nm <- gsub('^"|"$', '', nm)
  nm <- gsub("^\ufeff", "", nm)
  nm <- gsub("^ï»¿", "", nm)
  nm <- gsub("^ď»ż", "", nm)
  nm <- gsub('"', "", nm, fixed = TRUE)
  nm
}

# ============================================================
# 2) NAČTENÍ VSTUPNÍCH DAT
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
  stop("Chybí sloupec ", PIXEL_AREA_COL)
}

# ============================================================
# 3) DEFINICE AGREGOVANÝCH SLOUPCŮ
# - vyberou se pouze skutečně existující sloupce MOD_*
# ============================================================
mod_cols <- grep("^MOD_([0-9]+|NA)$", names(df), value = TRUE)

height_cols <- intersect(
  c("H_DOM_pred", "H_DOM_pred_fixed", "H_LOREY_pred", "H_LOREY_pred_fixed"),
  names(df)
)

sum_cols <- intersect(
  c("G_per_ha_pred", "G_per_ha_pred_fixed",
    "V_per_ha_pred", "V_per_ha_pred_fixed",
    "N_per_ha_pred", "N_per_ha_pred_fixed"),
  names(df)
)

if (length(mod_cols) == 0) {
  stop("Nebyly nalezeny žádné sloupce MOD_*.")
}

# Převod vybraných sloupců na numerický datový typ
num_cols <- unique(c(PIXEL_AREA_COL, mod_cols, height_cols, sum_cols))
df[num_cols] <- lapply(df[num_cols], to_num)

cat("Nalezené MOD sloupce:\n")
print(mod_cols)
cat("\n")

# ============================================================
# 4) VÝPOČET AGREGACE PRO JEDEN MOD
# ============================================================
calc_one_mod <- function(mod_name, data, pixel_area_col, height_cols, sum_cols) {
  share <- data[[mod_name]]
  pixel_area_m2 <- data[[pixel_area_col]]
  
  area_in_mod_m2 <- pixel_area_m2 * share
  area_in_mod_ha <- area_in_mod_m2 / 10000
  
  ok_area <- is.finite(share) & share > 0 &
    is.finite(pixel_area_m2) & pixel_area_m2 > 0 &
    is.finite(area_in_mod_m2) & area_in_mod_m2 > 0
  
  out <- list(
    MOD = mod_name,
    n_parts = sum(ok_area, na.rm = TRUE),
    area_m2 = sum(area_in_mod_m2[ok_area], na.rm = TRUE),
    area_ha = sum(area_in_mod_ha[ok_area], na.rm = TRUE)
  )
  
  # Výškové veličiny jsou agregovány jako plošně vážený průměr.
  for (col in height_cols) {
    out[[col]] <- wmean_safe(data[[col]], area_in_mod_m2)
  }
  
  # Ostatní taxační veličiny jsou agregovány jako plošně vážený součet.
  for (col in sum_cols) {
    ok <- ok_area & is.finite(data[[col]])
    out[[col]] <- if (any(ok)) {
      sum(data[[col]][ok] * area_in_mod_ha[ok], na.rm = TRUE)
    } else {
      NA_real_
    }
  }
  
  as.data.frame(out, check.names = FALSE)
}

# ============================================================
# 5) AGREGACE PRO VŠECHNY KATEGORIE MOD
# ============================================================
result_list <- lapply(
  mod_cols,
  calc_one_mod,
  data = df,
  pixel_area_col = PIXEL_AREA_COL,
  height_cols = height_cols,
  sum_cols = sum_cols
)

result <- bind_rows(result_list)

# Seřazení kategorií MOD podle číselného pořadí,
# přičemž MOD_NA je přesunut na konec.
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
# 6) ULOŽENÍ VÝSTUPU
# ============================================================
write.csv2(result, OUTPUT_CSV, row.names = FALSE)

if (requireNamespace("openxlsx", quietly = TRUE)) {
  openxlsx::write.xlsx(result, OUTPUT_XLSX, overwrite = TRUE)
}

# ============================================================
# 7) KONTROLNÍ VÝPIS
# ============================================================
cat("Počet řádků výsledku:", nrow(result), "\n\n")
print(result)
