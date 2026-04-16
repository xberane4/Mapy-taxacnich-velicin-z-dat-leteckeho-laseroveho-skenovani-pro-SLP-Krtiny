# ============================================================
# ÚČEL SKRIPTU
# ============================================================
# Skript slouží ke zpracování výsledků prostorového překryvu
# čtvercové polygonové sítě a polygonů hospodářských souborů.
# Pro každý identifikátor c_pix agreguje hodnoty plošného podílu
# (weight_mod) podle kategorií HOS_1, převádí data do širokého
# formátu, dopočítává zbývající podíl do hodnoty 1 jako třídu NA
# a ukládá výsledky do souboru XLSX. Součástí výstupu je rovněž
# kontrolní tabulka umožňující ověřit přesnost součtů před
# a po zaokrouhlení.
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(openxlsx)
})

# ============================================================
# NASTAVENÍ VSTUPNÍCH A VÝSTUPNÍCH PARAMETRŮ
# ============================================================
INPUT_XLSX  <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/PIL_LLM_INTER_TableToExcel_1.xlsx"
OUTPUT_XLSX <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/intersect_vystup_1.xlsx"
SHEET_IN    <- 1
TOL         <- 0.02   # tolerance využitá pouze pro kontrolní označení řádků s překročením součtu 1

# ============================================================
# FUNKCE PRO PŘEVOD TEXTOVÝCH HODNOT NA NUMERICKÝ FORMÁT
# - zohledňuje desetinnou čárku
# - odstraňuje mezery a prázdné hodnoty
# ============================================================
to_num <- function(x) {
  if (is.numeric(x)) return(x)
  
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN", "NULL")] <- NA
  x <- gsub("\u00A0", "", x, fixed = TRUE)  # odstranění pevné mezery
  x <- gsub(" ", "", x, fixed = TRUE)
  x <- gsub(",", ".", x, fixed = TRUE)
  
  suppressWarnings(as.numeric(x))
}

# ============================================================
# 1) NAČTENÍ VSTUPNÍCH DAT
# ============================================================
df <- read_xlsx(INPUT_XLSX, sheet = SHEET_IN)

req <- c("c_pix", "HOS_1", "weight_mod")
miss <- setdiff(req, names(df))
if (length(miss) > 0) {
  stop("V souboru chybí tyto sloupce: ", paste(miss, collapse = ", "))
}

# ============================================================
# 2) ČIŠTĚNÍ A PŘÍPRAVA DAT
# ============================================================
df2 <- df %>%
  transmute(
    c_pix      = as.character(c_pix),
    HOS_1      = trimws(as.character(HOS_1)),
    weight_mod = to_num(weight_mod)
  ) %>%
  mutate(
    HOS_1 = na_if(HOS_1, ""),
    weight_mod = pmax(weight_mod, 0)   # záporné hodnoty podílu nejsou přípustné
  ) %>%
  filter(!is.na(c_pix)) %>%
  filter(!is.na(weight_mod))

if (nrow(df2) == 0) {
  stop("Po načtení a vyčištění nezůstala žádná data.")
}

# určení pořadí sloupců HOS podle jejich prvního výskytu ve vstupních datech
hos_cols <- df2 %>%
  filter(!is.na(HOS_1)) %>%
  distinct(HOS_1) %>%
  pull(HOS_1)

# ============================================================
# 3) AGREGACE DAT PODLE c_pix A HOS_1
# - agregace probíhá v plné numerické přesnosti
# - zaokrouhlování se provádí až v závěrečné fázi
# ============================================================
agg <- df2 %>%
  group_by(c_pix, HOS_1) %>%
  summarise(weight_sum = sum(weight_mod, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    weight_sum = pmin(pmax(weight_sum, 0), 1)
  )

# ============================================================
# 4) PŘEVOD DO ŠIROKÉHO FORMÁTU
# ============================================================
wide_known <- agg %>%
  filter(!is.na(HOS_1)) %>%
  pivot_wider(
    names_from  = HOS_1,
    values_from = weight_sum,
    values_fill = 0
  )

# volitelně je zachycen i podíl řádků bez přiřazené hodnoty HOS_1 pro kontrolní účely
na_base <- agg %>%
  filter(is.na(HOS_1)) %>%
  group_by(c_pix) %>%
  summarise(NA_base_exact = sum(weight_sum, na.rm = TRUE), .groups = "drop")

pixels <- df2 %>% distinct(c_pix)

out <- pixels %>%
  left_join(wide_known, by = "c_pix") %>%
  left_join(na_base, by = "c_pix")

# doplnění nulových hodnot do všech očekávaných sloupců HOS
if (length(hos_cols) > 0) {
  for (nm in hos_cols) {
    if (!nm %in% names(out)) out[[nm]] <- 0
    out[[nm]][is.na(out[[nm]])] <- 0
    out[[nm]] <- pmin(pmax(out[[nm]], 0), 1)
  }
}

out$NA_base_exact[is.na(out$NA_base_exact)] <- 0
out$NA_base_exact <- pmin(pmax(out$NA_base_exact, 0), 1)

# ============================================================
# 5) VÝPOČET PŘESNÝCH SOUČTŮ
# ============================================================
sum_known_exact <- if (length(hos_cols) > 0) {
  rowSums(out[, hos_cols, drop = FALSE], na.rm = TRUE)
} else {
  rep(0, nrow(out))
}

# výpočet přesného zbytku do hodnoty 1
# finální hodnota NA představuje podíl, který chybí do úplného součtu po sečtení kategorií HOS
out[["NA_exact"]] <- pmax(0, 1 - sum_known_exact)

# identifikace řádků, kde součet známých kategorií HOS překračuje hodnotu 1
problem_over_1 <- sum_known_exact > (1 + TOL)

sum_final_exact <- sum_known_exact + out[["NA_exact"]]

# ============================================================
# 6) ZAOKROUHLENÍ AŽ V ZÁVĚREČNÉM KROKU
# - hodnoty HOS se zaokrouhlují na tři desetinná místa
# - hodnota NA ve výstupu je dopočtena ze zaokrouhlených HOS tak,
#   aby výsledný součet byl pokud možno přesně 1,000
# ============================================================
final_out <- out %>%
  select(c_pix, all_of(hos_cols))

# zaokrouhlení hodnot HOS
if (length(hos_cols) > 0) {
  for (nm in hos_cols) {
    final_out[[nm]] <- round(final_out[[nm]], 3)
  }
}

sum_known_round <- if (length(hos_cols) > 0) {
  rowSums(final_out[, hos_cols, drop = FALSE], na.rm = TRUE)
} else {
  rep(0, nrow(final_out))
}

# výpočet exportní hodnoty NA:
# - standardně jako rozdíl do hodnoty 1 po zaokrouhlení HOS
# - pokud již součet HOS překračuje 1, nastaví se NA na 0
#   a řádek je označen v kontrolní tabulce
final_out[["NA"]] <- ifelse(
  problem_over_1,
  0,
  round(pmax(0, 1 - sum_known_round), 3)
)

sum_final_round <- sum_known_round + final_out[["NA"]]

# ============================================================
# 7) VYTVOŘENÍ KONTROLNÍ TABULKY
# ============================================================
control <- data.frame(
  c_pix             = out$c_pix,
  sum_known_exact   = round(sum_known_exact, 6),
  NA_base_exact     = round(out$NA_base_exact, 6),
  NA_exact          = round(out[["NA_exact"]], 6),
  sum_final_exact   = round(sum_final_exact, 6),
  sum_known_round   = round(sum_known_round, 3),
  `NA`              = round(final_out[["NA"]], 3),
  sum_final_round   = round(sum_final_round, 3),
  problem_over_1    = problem_over_1,
  check.names       = FALSE
)

# ============================================================
# 8) ULOŽENÍ VÝSTUPU DO SOUBORU XLSX
# ============================================================
wb <- createWorkbook()

addWorksheet(wb, "data")
writeData(wb, "data", final_out)

addWorksheet(wb, "kontrola")
writeData(wb, "kontrola", control)

num_style_3 <- createStyle(numFmt = "0.000")
num_style_6 <- createStyle(numFmt = "0.000000")

if (ncol(final_out) > 1) {
  addStyle(
    wb, "data", num_style_3,
    rows = 2:(nrow(final_out) + 1),
    cols = 2:ncol(final_out),
    gridExpand = TRUE,
    stack = TRUE
  )
}

if (ncol(control) > 1) {
  # sloupce 2 až 5 jsou formátovány na šest desetinných míst
  addStyle(
    wb, "kontrola", num_style_6,
    rows = 2:(nrow(control) + 1),
    cols = 2:5,
    gridExpand = TRUE,
    stack = TRUE
  )
  
  # sloupce 6 až 8 jsou formátovány na tři desetinná místa
  addStyle(
    wb, "kontrola", num_style_3,
    rows = 2:(nrow(control) + 1),
    cols = 6:8,
    gridExpand = TRUE,
    stack = TRUE
  )
}

setColWidths(wb, "data", cols = 1:ncol(final_out), widths = "auto")
setColWidths(wb, "kontrola", cols = 1:ncol(control), widths = "auto")

saveWorkbook(wb, OUTPUT_XLSX, overwrite = TRUE)

cat("Hotovo. Výstup uložen do:\n", OUTPUT_XLSX, "\n")
