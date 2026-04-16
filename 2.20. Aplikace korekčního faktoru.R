

# ============================================================
# ÚČEL SKRIPTU
# Skript slouží k aplikaci korekčních koeficientů na predikované
# taxační veličiny v pixelové tabulce s podíly hospodářských
# modelů (MOD). Pro každý pixel nejprve rozloží predikované
# hodnoty podle zastoupení jednotlivých MOD, následně na tyto
# dílčí příspěvky aplikuje odpovídající korekční koeficienty
# a poté hodnoty znovu agreguje na úroveň pixelu. Výstupem je
# rozšířená pixelová tabulka a současně detailní tabulka dílčích
# příspěvků jednotlivých MOD.
# ============================================================

# ============================================================
# APLIKACE KOREKČNÍCH KOEFICIENTŮ MOD NA PREDIKOVANÉ HODNOTY
# V PIXELOVÉ TABULCE
# - vstupem je tabulka korekčních koeficientů a pixelová tabulka
#   s predikovanými taxačními veličinami
# - predikce jsou rozděleny podle podílu jednotlivých MOD v pixelu
# - pro každou část je vypočten základní příspěvek a následně
#   korigovaný příspěvek
# - výsledné korigované hodnoty jsou poté agregovány zpět
#   na úroveň pixelu
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(openxlsx)
})
# ============================================================
# 1) NASTAVENÍ
# ============================================================

# ---- vstupní soubory
COEF_FILE  <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/LM_X20_x_PIL.xlsx"
PIXEL_FILE <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/REG_MOD_PIL/LM_PIL_pixels_filtred.csv"

COEF_SHEET  <- 1
PIXEL_SHEET <- 1

# ---- výstupní soubory
OUT_XLSX <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/REG_MOD_PIL/pixely_MOD.xlsx"
OUT_CSV  <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/REG_MOD_PIL/pixely_MOD.csv"

# ---- identifikátor pixelu
# Pokud pixelová tabulka neobsahuje vlastní identifikátor,
# skript jej vytvoří automaticky.
PIX_ID_COL <- "PIX_ID"

# ---- plocha jednoho pixelu v hektarech
PIXEL_AREA_HA <- 0.05   # 500 m2

# ---- predikované veličiny v pixelové tabulce (/ha)
PRED_COLS <- c(
  G = "G_per_ha_filtred",
  V = "V_per_ha_filtred",
  N = "N_per_ha_filtred"
)

# ---- sloupce s podíly jednotlivých MOD v pixelové tabulce
MOD_SHARE_COLS <- c(
  "MOD_11", "MOD_14", "MOD_5", "MOD_2", "MOD_4", "MOD_16", "MOD_15",
  "MOD_12", "MOD_3", "MOD_10", "MOD_7", "MOD_6", "MOD_1", "MOD_9",
  "MOD_8", "MOD_19", "MOD_NA"
)

# ---- názvy sloupců v tabulce korekčních koeficientů
# Uprav pouze v případě, že se ve vstupním souboru jmenují odlišně.
COEF_MOD_COL <- "MOD"

COEF_COLS <- c(
  G_norm = "k_G",
  V_norm = "k_V",
  N_norm = "k_N",
  G_cap  = "k_G_cap",
  V_cap  = "k_V_cap",
  N_cap  = "k_N_cap"
)

# ---- práce s kategorií MOD_NA = bezlesí
# TRUE  = MOD_NA zůstane v tabulce a dostane koeficienty 0
# FALSE = MOD_NA bude z výpočtu zcela vyloučeno
USE_MOD_NA <- TRUE

# ============================================================
# 2) POMOCNÉ FUNKCE
# ============================================================

to_num <- function(x) {
  if (is.numeric(x)) return(x)
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN", "NULL", "-", "--")] <- NA
  x <- gsub("\u00A0", "", x, fixed = TRUE)
  x <- gsub(" ", "", x, fixed = TRUE)
  x <- gsub(",", ".", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

read_any <- function(path, sheet = 1) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) {
    read_excel(path, sheet = sheet)
  } else if (ext == "csv") {
    read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    stop("Nepodporovaný formát souboru: ", ext)
  }
}

# ============================================================
# 3) NAČTENÍ VSTUPNÍCH DAT
# ============================================================

coef <- read_any(COEF_FILE, COEF_SHEET)
pix  <- read_any(PIXEL_FILE, PIXEL_SHEET)

# Pokud pixelová tabulka neobsahuje vlastní identifikátor,
# je vytvořen automaticky.
if (!(PIX_ID_COL %in% names(pix))) {
  pix[[PIX_ID_COL]] <- seq_len(nrow(pix))
}

# ---- kontrola povinných sloupců v tabulce koeficientů
need_coef <- c(COEF_MOD_COL, unname(COEF_COLS))
miss_coef <- setdiff(need_coef, names(coef))
if (length(miss_coef) > 0) {
  stop("V tabulce korekcí chybí sloupce: ", paste(miss_coef, collapse = ", "))
}

# ---- kontrola povinných sloupců v pixelové tabulce
need_pix <- c(PIX_ID_COL, MOD_SHARE_COLS, unname(PRED_COLS))
miss_pix <- setdiff(need_pix, names(pix))
if (length(miss_pix) > 0) {
  stop("V pixelové tabulce chybí sloupce: ", paste(miss_pix, collapse = ", "))
}

# ---- převod vybraných sloupců na numerický typ
coef <- coef %>%
  mutate(across(all_of(unname(COEF_COLS)), to_num))

pix <- pix %>%
  mutate(across(all_of(c(MOD_SHARE_COLS, unname(PRED_COLS))), to_num))

# ============================================================
# 4) PŘEVOD PIXELOVÉ TABULKY DO DLOUHÉHO FORMÁTU
# ============================================================

pix_long <- pix %>%
  select(all_of(c(PIX_ID_COL, unname(PRED_COLS), MOD_SHARE_COLS))) %>%
  pivot_longer(
    cols = all_of(MOD_SHARE_COLS),
    names_to = "MOD",
    values_to = "share"
  ) %>%
  mutate(
    share = to_num(share),
    # Pokud jsou podíly zapsány v procentech, jsou převedeny na rozsah 0-1.
    share = ifelse(!is.na(share) & share > 1, share / 100, share)
  ) %>%
  filter(!is.na(share), share > 0)

# ============================================================
# 5) ZPRACOVÁNÍ KATEGORIE MOD_NA
# ============================================================

if (USE_MOD_NA) {
  # Pokud v tabulce koeficientů chybí MOD_NA, je doplněn
  # s nulovými koeficienty.
  if (!("MOD_NA" %in% coef[[COEF_MOD_COL]])) {
    coef_na <- data.frame(
      MOD     = "MOD_NA",
      k_G     = 0,
      k_V     = 0,
      k_N     = 0,
      k_G_cap = 0,
      k_V_cap = 0,
      k_N_cap = 0,
      stringsAsFactors = FALSE
    )
    
    names(coef_na) <- c(
      COEF_MOD_COL,
      COEF_COLS["G_norm"], COEF_COLS["V_norm"], COEF_COLS["N_norm"],
      COEF_COLS["G_cap"],  COEF_COLS["V_cap"],  COEF_COLS["N_cap"]
    )
    
    coef <- bind_rows(coef, coef_na)
  }
} else {
  pix_long <- pix_long %>%
    filter(MOD != "MOD_NA")
}

# ============================================================
# 6) PŘIPOJENÍ KOREKČNÍCH KOEFICIENTŮ
# ============================================================

coef_join <- coef %>%
  rename(MOD = all_of(COEF_MOD_COL)) %>%
  select(MOD, all_of(unname(COEF_COLS)))

dat <- pix_long %>%
  left_join(coef_join, by = "MOD")

# Kontrola chybějících koeficientů po spojení
for (cc in unname(COEF_COLS)) {
  n_miss <- sum(is.na(dat[[cc]]))
  if (n_miss > 0) {
    warning("Ve sloupci ", cc, " chybí ", n_miss, " hodnot.")
  }
}

# ============================================================
# 7) VÝPOČET DÍLČÍCH PŘÍSPĚVKŮ
# ============================================================
# exact_part = pred_ha * share * pixel_area_ha * koeficient

for (v in names(PRED_COLS)) {
  
  pred_col   <- PRED_COLS[[v]]
  k_norm_col <- COEF_COLS[[paste0(v, "_norm")]]
  k_cap_col  <- COEF_COLS[[paste0(v, "_cap")]]
  
  base_col <- paste0(v, "_base_part")
  norm_col <- paste0(v, "_exact_part_normal")
  cap_col  <- paste0(v, "_exact_part_cap")
  
  # Základní dílčí příspěvek bez korekce
  dat[[base_col]] <- dat[[pred_col]] * dat$share * PIXEL_AREA_HA
  
  # Dílčí příspěvek po aplikaci standardního koeficientu
  dat[[norm_col]] <- dat[[base_col]] * dat[[k_norm_col]]
  
  # Dílčí příspěvek po aplikaci zastropovaného koeficientu
  dat[[cap_col]]  <- dat[[base_col]] * dat[[k_cap_col]]
}

# ============================================================
# 8) AGREGACE HODNOT ZPĚT NA ÚROVEŇ PIXELU
# ============================================================

pix_sum <- dat %>%
  group_by(.data[[PIX_ID_COL]]) %>%
  summarise(
    G_exact_normal = sum(G_exact_part_normal, na.rm = TRUE),
    V_exact_normal = sum(V_exact_part_normal, na.rm = TRUE),
    N_exact_normal = sum(N_exact_part_normal, na.rm = TRUE),
    
    G_exact_cap = sum(G_exact_part_cap, na.rm = TRUE),
    V_exact_cap = sum(V_exact_part_cap, na.rm = TRUE),
    N_exact_cap = sum(N_exact_part_cap, na.rm = TRUE),
    
    # Kategorie MOD_NA se do lesní části pixelu nezapočítává.
    forest_share = sum(ifelse(MOD != "MOD_NA", share, 0), na.rm = TRUE),
    .groups = "drop"
  )

# ============================================================
# 9) ZPĚTNÝ PŘEPOČET NA HODNOTY /ha PRO LESNÍ ČÁST PIXELU
# ============================================================

pix_sum <- pix_sum %>%
  mutate(
    G_ha_normal = ifelse(forest_share > 0, G_exact_normal / (forest_share * PIXEL_AREA_HA), NA_real_),
    V_ha_normal = ifelse(forest_share > 0, V_exact_normal / (forest_share * PIXEL_AREA_HA), NA_real_),
    N_ha_normal = ifelse(forest_share > 0, N_exact_normal / (forest_share * PIXEL_AREA_HA), NA_real_),
    
    G_ha_cap = ifelse(forest_share > 0, G_exact_cap / (forest_share * PIXEL_AREA_HA), NA_real_),
    V_ha_cap = ifelse(forest_share > 0, V_exact_cap / (forest_share * PIXEL_AREA_HA), NA_real_),
    N_ha_cap = ifelse(forest_share > 0, N_exact_cap / (forest_share * PIXEL_AREA_HA), NA_real_)
  )

# ============================================================
# 10) VOLITELNÉ PŘIPOJENÍ VÝSLEDKŮ ZPĚT K PŮVODNÍ PIXELOVÉ TABULCE
# ============================================================

pix_out <- pix %>%
  left_join(pix_sum, by = setNames("PIX_ID", PIX_ID_COL) |> names())

# ============================================================
# 11) EXPORT VÝSTUPŮ
# ============================================================

write.csv(pix_out, OUT_CSV, row.names = FALSE)

wb <- createWorkbook()

addWorksheet(wb, "pixel_sum")
writeData(wb, "pixel_sum", pix_out)

addWorksheet(wb, "pixel_mod_parts")
writeData(wb, "pixel_mod_parts", dat)

info <- data.frame(
  parametr = c("PIXEL_AREA_HA", "USE_MOD_NA", "PIX_ID_COL"),
  hodnota  = c(as.character(PIXEL_AREA_HA), as.character(USE_MOD_NA), PIX_ID_COL)
)

addWorksheet(wb, "info")
writeData(wb, "info", info)

saveWorkbook(wb, OUT_XLSX, overwrite = TRUE)

cat("Hotovo.\n")
cat("CSV:  ", OUT_CSV, "\n")
cat("XLSX: ", OUT_XLSX, "\n")
