# ------------------------------------------------------------
# ÚČEL SCRIPTU
# ------------------------------------------------------------
# Tento script slouží k přiřazení výsledné skupiny dřevin
# jednotlivým záznamům na základě podílů zastoupení dřevinných
# kategorií v atributové tabulce. Současně je pro každý záznam
# vypočten váhový koeficient vyjadřující podíl lesních kategorií
# na celkové ploše záznamu.
#
# Výsledná skupina dřevin je určena podle následujících pravidel:
# - pokud některá kategorie přesahuje 50 % lesní složky, je tato
#   kategorie přiřazena jako výsledná skupina,
# - v opačném případě je rozhodnuto podle převahy jehličnatých
#   nebo listnatých kategorií.
#
# Výstupem scriptu je nová tabulka doplněná o:
# - sloupec weight, vyjadřující podíl lesní části na celku,
# - sloupec skupina, obsahující výsledně přiřazenou skupinu dřevin.
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

# ------------------------------------------------------------
# DEFINICE VSTUPNÍ A VÝSTUPNÍ CESTY
# ------------------------------------------------------------
DATA_PATH <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_test/Vystupy_GIS/TAB_SEN_2_TableToExcel.csv"
OUT_PATH  <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_test/Vystupy_GIS/TAB_SEN_2_DREV_70.csv"

# ------------------------------------------------------------
# 1) NAČTENÍ VSTUPNÍ TABULKY
# ------------------------------------------------------------
# Nejprve je zkusmo použito standardní načtení CSV. Pokud by byla
# data oddělena středníkem, je soubor načten alternativně pomocí
# explicitně zadaného oddělovače.
# ------------------------------------------------------------
df <- read_csv(DATA_PATH, show_col_types = FALSE)
if (ncol(df) == 1) {
  df <- read_delim(DATA_PATH, delim = ";", show_col_types = FALSE)
}

# ------------------------------------------------------------
# 2) DEFINICE NÁZVŮ RELEVANTNÍCH SLOUPCŮ
# ------------------------------------------------------------
col_sm  <- "SM"
col_bo  <- "BO"
col_dbz <- "DBZ"
col_bk  <- "BK"
col_md  <- "MD"
col_ol  <- "ostatní listnaté"
col_oj  <- "ostatní jehličnaté"
col_nic <- "NIC"

forest_cols <- c(col_sm, col_bo, col_ol, col_dbz, col_oj, col_bk, col_md)

# Kontrola přítomnosti požadovaných sloupců ve vstupních datech
missing_cols <- setdiff(c(forest_cols, col_nic), names(df))
if (length(missing_cols) > 0) {
  stop("V datech chybí sloupce: ", paste(missing_cols, collapse = ", "))
}

# ------------------------------------------------------------
# 3) VÝPOČET PODÍLŮ A PŘIŘAZENÍ VÝSLEDNÉ SKUPINY DŘEVIN
# ------------------------------------------------------------
# Výpočet probíhá vektorově pro rychlejší zpracování:
# - je určena suma lesních kategorií,
# - je vypočten podíl lesní složky na celkové ploše,
# - je identifikována nejvíce zastoupená kategorie,
# - výsledná skupina je přiřazena podle pravidla 50 %
#   nebo podle převahy jehličnatých a listnatých kategorií.
# ------------------------------------------------------------
M <- as.matrix(df[, forest_cols])
mode(M) <- "numeric"

forest_sum <- rowSums(M, na.rm = TRUE)
nic        <- as.numeric(df[[col_nic]])
total_sum  <- forest_sum + nic

weight <- ifelse(total_sum > 0, forest_sum / total_sum, NA_real_)

row_max   <- apply(M, 1, max, na.rm = TRUE)
max_idx   <- max.col(M, ties.method = "first")
max_share <- ifelse(forest_sum > 0, row_max / forest_sum, NA_real_)
max_col   <- forest_cols[max_idx]

sum_conif <- as.numeric(df[[col_sm]]) +
  as.numeric(df[[col_md]]) +
  as.numeric(df[[col_bo]]) +
  as.numeric(df[[col_oj]])

sum_broad <- as.numeric(df[[col_dbz]]) +
  as.numeric(df[[col_bk]]) +
  as.numeric(df[[col_ol]])

skupina <- ifelse(
  is.na(forest_sum) | forest_sum <= 0, NA_character_,
  ifelse(
    !is.na(max_share) & max_share > 0.50, max_col,
    ifelse(sum_conif >= sum_broad, col_oj, col_ol)
  )
)

# ------------------------------------------------------------
# 4) DOPLNĚNÍ NOVÝCH SLOUPCŮ DO VÝSTUPNÍ TABULKY
# ------------------------------------------------------------
df_out <- df %>%
  mutate(
    weight = weight,
    skupina = skupina
  )

# ------------------------------------------------------------
# 5) EXPORT VÝSLEDNÉ TABULKY A ZÁKLADNÍ KONTROLA
# ------------------------------------------------------------
write_csv(df_out, OUT_PATH)

if (!file.exists(OUT_PATH)) {
  stop("Soubor se neuložil (blokuje ho Excel / práva / špatná cesta).")
}

message("Hotovo! Uloženo do: ", OUT_PATH)
message("Řádků: ", nrow(df_out), " | Sloupců: ", ncol(df_out))
