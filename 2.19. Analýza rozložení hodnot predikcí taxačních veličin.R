# ============================================================
# ÚČEL SKRIPTU
# Skript slouží k vytvoření histogramů predikovaných taxačních
# veličin na úrovni pixelů a k výpočtu jejich základních
# souhrnných statistik a vybraných kvantilů. Pro každý zvolený
# sloupec vytváří samostatný histogram, doplňuje orientační
# značky důležitých percentilů a ukládá souhrnnou tabulku
# statistických charakteristik ve formátu CSV, případně také XLSX.
# Výstupy slouží zejména k posouzení rozdělení hodnot a volbě
# vhodných hranic pro další interpretaci nebo zastropování.
# ============================================================

# ============================================================
# TVORBA HISTOGRAMŮ A TABULKY KVANTILŮ PREDIKOVANÝCH VELIČIN
# - vstupem je pixelová tabulka s predikovanými taxačními veličinami
# - pro každý vybraný sloupec je vytvořen samostatný histogram
# - současně jsou vypočteny základní statistiky a vybrané kvantily
# - volitelně lze zahrnout i varianty predikcí založené pouze
#   na fixní části modelu (_fixed)
# ============================================================

suppressPackageStartupMessages({
  library(readr)
})

# ============================================================
# 0) NASTAVENÍ CEST A PARAMETRŮ
# ============================================================
INPUT_CSV  <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/LLM_PIL/LLM_PIL_pixels.csv"
OUTPUT_DIR <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/LLM_PIL/histogramy_LLM_PIL1"

# Pokud je hodnota TRUE, budou vytvořeny histogramy také
# pro sloupce s příponou _fixed.
INCLUDE_FIXED <- TRUE

# Počet tříd histogramu
N_BINS <- 60

# ============================================================
# 1) DEFINICE POŽADOVANÝCH KVANTILŮ
# ============================================================
quantile_probs <- c(
  p01   = 0.01,
  p05   = 0.05,
  p25   = 0.25,
  median = 0.50,
  p75   = 0.75,
  p95   = 0.95,
  p96   = 0.96,
  p97   = 0.97,
  p98   = 0.98,
  p99   = 0.99,
  p995  = 0.995,
  p996  = 0.996,
  p997  = 0.997,
  p998  = 0.998,
  p999  = 0.999,
  p9995 = 0.9995,
  p9996 = 0.9996,
  p9997 = 0.9997,
  p9998 = 0.9998,
  p9999 = 0.9999
)

# ============================================================
# 2) POMOCNÉ FUNKCE
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

clean_names <- function(nm) {
  nm <- gsub('^"|"$', '', nm)
  nm <- gsub("^\ufeff", "", nm)
  nm <- gsub("^ï»¿", "", nm)
  nm <- gsub("^ď»ż", "", nm)
  nm <- gsub('"', "", nm, fixed = TRUE)
  nm
}

safe_quantile <- function(x, probs) {
  if (length(x) == 0) return(rep(NA_real_, length(probs)))
  as.numeric(quantile(x, probs = probs, na.rm = TRUE, names = FALSE, type = 7))
}

# ============================================================
# 3) VYTVOŘENÍ VÝSTUPNÍ SLOŽKY
# ============================================================
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

# ============================================================
# 4) NAČTENÍ VSTUPNÍCH DAT
# ============================================================
df <- read_delim(
  file = INPUT_CSV,
  delim = ";",
  locale = locale(decimal_mark = ","),
  show_col_types = FALSE,
  guess_max = 100000
)

names(df) <- clean_names(names(df))

# ============================================================
# 5) DEFINICE ZPRACOVÁVANÝCH SLOUPCŮ
# ============================================================
base_cols <- c(
  "H_DOM_pred",
  "H_LOREY_pred",
  "G_per_ha_pred",
  "N_per_ha_pred",
  "V_per_ha_pred"
)

fixed_cols <- c(
  "H_DOM_pred_fixed",
  "H_LOREY_pred_fixed",
  "G_per_ha_pred_fixed",
  "N_per_ha_pred_fixed",
  "V_per_ha_pred_fixed"
)

plot_cols <- intersect(base_cols, names(df))

if (INCLUDE_FIXED) {
  plot_cols <- c(plot_cols, intersect(fixed_cols, names(df)))
}

if (length(plot_cols) == 0) {
  stop("Nebyl nalezen žádný odpovídající sloupec taxačních veličin.")
}

# Převod vybraných sloupců na numerický datový typ
df[plot_cols] <- lapply(df[plot_cols], to_num)

cat("Budou zpracovány tyto sloupce:\n")
print(plot_cols)
cat("\n")

# ============================================================
# 6) TVORBA HISTOGRAMŮ A SOUHRNNÉ TABULKY KVANTILŮ
# ============================================================
summary_list <- list()

for (col in plot_cols) {
  x_all <- df[[col]]
  x <- x_all[is.finite(x_all) & !is.na(x_all)]
  
  if (length(x) == 0) {
    warning("Sloupec ", col, " neobsahuje žádné platné numerické hodnoty.")
    next
  }
  
  # Výpočet požadovaných kvantilů
  qs <- safe_quantile(x, quantile_probs)
  names(qs) <- names(quantile_probs)
  
  # Souhrnná statistika pro aktuální sloupec
  stat_row <- data.frame(
    variable = col,
    n        = length(x),
    mean     = mean(x, na.rm = TRUE),
    sd       = sd(x, na.rm = TRUE),
    min      = min(x, na.rm = TRUE),
    
    p01      = qs["p01"],
    p05      = qs["p05"],
    p25      = qs["p25"],
    median   = qs["median"],
    p75      = qs["p75"],
    p95      = qs["p95"],
    p96      = qs["p96"],
    p97      = qs["p97"],
    p98      = qs["p98"],
    p99      = qs["p99"],
    p995     = qs["p995"],
    p996     = qs["p996"],
    p997     = qs["p997"],
    p998     = qs["p998"],
    p999     = qs["p999"],
    p9995    = qs["p9995"],
    p9996    = qs["p9996"],
    p9997    = qs["p9997"],
    p9998    = qs["p9998"],
    p9999    = qs["p9999"],
    
    max      = max(x, na.rm = TRUE),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  summary_list[[col]] <- stat_row
  
  # Název výstupního obrázku
  file_png <- file.path(OUTPUT_DIR, paste0(col, "_hist.png"))
  
  # Vytvoření histogramu
  png(filename = file_png, width = 1800, height = 1200, res = 180)
  
  hist(
    x,
    breaks = N_BINS,
    main = paste("Histogram:", col),
    xlab = col,
    ylab = "Frekvence",
    border = "grey30",
    col = "grey85"
  )
  
  # Pro lepší čitelnost jsou do grafu doplněny pouze vybrané
  # svislé referenční čáry.
  abline(v = stat_row$median, lwd = 2, lty = 2)
  abline(v = stat_row$p95,    lwd = 2, lty = 3)
  abline(v = stat_row$p99,    lwd = 2, lty = 4)
  abline(v = stat_row$p995,   lwd = 2, lty = 5)
  abline(v = stat_row$p999,   lwd = 2, lty = 6)
  abline(v = stat_row$p9995,  lwd = 2, lty = 1)
  abline(v = stat_row$max,    lwd = 2, lty = 1)
  
  legend(
    "topright",
    legend = c(
      paste0("median = ", round(stat_row$median, 2)),
      paste0("p95 = ", round(stat_row$p95, 2)),
      paste0("p99 = ", round(stat_row$p99, 2)),
      paste0("p99.5 = ", round(stat_row$p995, 2)),
      paste0("p99.9 = ", round(stat_row$p999, 2)),
      paste0("p99.95 = ", round(stat_row$p9995, 2)),
      paste0("max = ", round(stat_row$max, 2))
    ),
    lty = c(2, 3, 4, 5, 6, 1, 1),
    lwd = 2,
    bty = "n",
    cex = 0.8
  )
  
  dev.off()
}

# ============================================================
# 7) ULOŽENÍ TABULKY STATISTIK
# ============================================================
summary_table <- do.call(rbind, summary_list)

csv_out <- file.path(OUTPUT_DIR, "hist_summary_quantiles.csv")
write.csv2(summary_table, csv_out, row.names = FALSE)

if (requireNamespace("openxlsx", quietly = TRUE)) {
  xlsx_out <- file.path(OUTPUT_DIR, "hist_summary_quantiles.xlsx")
  openxlsx::write.xlsx(summary_table, xlsx_out, overwrite = TRUE)
}

# ============================================================
# 8) KONTROLNÍ VÝPIS
# ============================================================
cat("Hotovo.\n")
cat("Histogramy byly uloženy do:\n", OUTPUT_DIR, "\n\n")
cat("Souhrnná tabulka kvantilů:\n")
print(summary_table)
