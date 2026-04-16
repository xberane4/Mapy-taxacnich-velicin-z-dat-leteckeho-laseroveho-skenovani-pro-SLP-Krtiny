# ============================================================
# ÚČEL SKRIPTU
# ============================================================
# Skript slouží k plošné aplikaci vybraných regresních modelů na
# pixelovou síť zájmového území. Pro každý pixel vybírá odpovídající
# model na základě kombinace cílové proměnné, dřevinné skupiny
# a výškového intervalu, následně vyhodnotí rovnici modelu a doplní
# predikované hodnoty i vybrané charakteristiky modelu do výstupní
# atributové tabulky.
# ============================================================

# ============================================================
# APLIKACE REGRESNÍCH MODELŮ NA PIXELY (ŠLP Křtiny)
#
# Princip zpracování:
# - výběr modelu podle kombinace:
#     target + skupina + interval_low <= H_proxy < interval_high,
# - výpočet predikce z textového zápisu rovnice ve sloupci "equation",
# - doplnění výstupních sloupců pro každý target:
#     <target>, <target>_equation, <target>_CV_R2, <target>_CV_RMSE,
# - export výsledné tabulky do CSV ve formátu vhodném pro české
#   prostředí Excelu (oddělovač ;, desetinná čárka, UTF-8 BOM).
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(readr)
  library(readxl)
  library(stringr)
})

# ------------------------------------------------------------
# 0) Nastavení vstupních a výstupních cest
# ------------------------------------------------------------
PIXELS_PATH <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_test/Vystupy_GIS/TAB_GRID_FIN_FIN.csv"
MODELS_PATH <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/REG_MOD_PIL/Vyber_modelu_with_bounds_PIL_FIN.xlsx"
OUT_PATH    <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/REG_MOD_PIL/PIL_TAB_GRID_FIN_with_predictions.csv"

# ------------------------------------------------------------
# 1) Pomocná funkce pro robustní automatické načtení CSV souboru
# ------------------------------------------------------------
# Funkce:
# - odhadne oddělovač podle prvního řádku,
# - zohlední běžné varianty desetinného znaku,
# - v případě chybného načtení se pokusí použít alternativní oddělovač.
read_csv_auto <- function(path, guess_max = 200000) {
  first_line <- readLines(path, n = 1, warn = FALSE)
  n_semicol  <- str_count(first_line, ";")
  n_comma    <- str_count(first_line, ",")
  delim <- if (n_semicol > n_comma) ";" else ","
  
  loc <- if (delim == ";") {
    locale(encoding = "UTF-8", decimal_mark = ",")
  } else {
    locale(encoding = "UTF-8", decimal_mark = ".")
  }
  
  df <- read_delim(
    file = path,
    delim = delim,
    locale = loc,
    show_col_types = FALSE,
    guess_max = guess_max,
    progress = FALSE
  )
  
  # Pokud dojde k načtení pouze jednoho sloupce,
  # je vyzkoušen opačný oddělovač.
  if (ncol(df) == 1) {
    delim2 <- if (delim == ";") "," else ";"
    loc2 <- if (delim2 == ";") locale(encoding = "UTF-8", decimal_mark = ",") else locale(encoding = "UTF-8", decimal_mark = ".")
    df <- read_delim(
      file = path,
      delim = delim2,
      locale = loc2,
      show_col_types = FALSE,
      guess_max = guess_max,
      progress = FALSE
    )
  }
  
  df
}

# Pomocná funkce pro načtení tabulky modelů ve formátu XLSX nebo CSV
read_models_auto <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) {
    as.data.table(read_excel(path, sheet = 1))
  } else {
    as.data.table(read_csv_auto(path, guess_max = 200000))
  }
}

# ------------------------------------------------------------
# 2) Načtení pixelové tabulky
# ------------------------------------------------------------
pixels_df <- read_csv_auto(PIXELS_PATH, guess_max = 200000)
pixels <- as.data.table(pixels_df)

req_pix <- c("H_proxy", "skupina")
miss_pix <- setdiff(req_pix, names(pixels))
if (length(miss_pix) > 0) {
  stop("V tabulce pixelů chybí sloupce: ", paste(miss_pix, collapse = ", "),
       "\nTip: soubor se mohl načíst s nesprávným oddělovačem nebo desetinným znakem.")
}

# ------------------------------------------------------------
# 3) Načtení tabulky modelů (CSV nebo XLSX)
# ------------------------------------------------------------
models <- read_models_auto(MODELS_PATH)

req_mod <- c("target","skupina","interval_low","interval_high","equation","CV_R2","CV_RMSE")
miss_mod <- setdiff(req_mod, names(models))
if (length(miss_mod) > 0) {
  stop("V tabulce modelů chybí sloupce: ", paste(miss_mod, collapse = ", "))
}

models[, interval_low  := suppressWarnings(as.numeric(interval_low))]
models[, interval_high := suppressWarnings(as.numeric(interval_high))]

# ------------------------------------------------------------
# 4) Pomocná funkce pro získání pravé strany rovnice
# ------------------------------------------------------------
# Z textového zápisu rovnice ve tvaru "target = ..."
# vrací pouze pravou stranu výrazu určenou pro výpočet predikce.
equation_rhs <- function(eq_txt) {
  if (is.na(eq_txt) || !nzchar(eq_txt)) return(NA_character_)
  pos <- regexpr("=", eq_txt, fixed = TRUE)
  if (pos[1] == -1) return(NA_character_)
  rhs <- trimws(substr(eq_txt, pos[1] + 1, nchar(eq_txt)))
  if (!nzchar(rhs)) return(NA_character_)
  rhs
}

# ------------------------------------------------------------
# 5) Převod proměnných použitých v rovnicích na numerický formát
# ------------------------------------------------------------
# Funkce zajišťuje:
# - převod faktorů a textových proměnných,
# - nahrazení desetinné čárky desetinnou tečkou,
# - bezpečný převod na numerický typ.
to_num_safe <- function(x) {
  if (is.numeric(x)) return(x)
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    x <- trimws(x)
    x[x == ""] <- NA
    x <- gsub(",", ".", x, fixed = TRUE)
  }
  suppressWarnings(as.numeric(x))
}

# Identifikace všech proměnných použitých na pravých stranách rovnic
rhs_all <- vapply(models$equation, equation_rhs, character(1))
rhs_all <- rhs_all[!is.na(rhs_all) & nzchar(rhs_all)]

extract_vars <- function(rhs) {
  toks <- unique(unlist(regmatches(rhs, gregexpr("[A-Za-z.][A-Za-z0-9._]*", rhs, perl = TRUE))))
  setdiff(toks, c("NA","NaN","Inf","TRUE","FALSE","pi","e",
                  "log","log10","exp","sqrt","abs","sin","cos","tan",
                  "pmin","pmax","ifelse","min","max","round","floor","ceiling"))
}

vars_used <- unique(unlist(lapply(rhs_all, extract_vars)))
vars_used <- intersect(vars_used, names(pixels))

for (v in vars_used) {
  pixels[[v]] <- to_num_safe(pixels[[v]])
}

# ------------------------------------------------------------
# 6) Aplikace modelů pro jednotlivé cílové proměnné
# ------------------------------------------------------------
targets <- unique(models$target)

for (tg in targets) {
  
  cat(">> Zpracovávám target:", tg, "\n")
  
  m_tg <- models[target == tg]
  
  # Výběr odpovídajícího modelu pro každý pixel
  # na základě dřevinné skupiny a výškového intervalu.
  joined <- m_tg[pixels,
                 on = .(skupina, interval_low <= H_proxy, interval_high > H_proxy),
                 mult = "first"
  ]
  
  # Extrakce pravé strany rovnice pro vlastní výpočet predikce
  joined[, rhs := vapply(equation, equation_rhs, character(1))]
  
  pred_col <- tg
  joined[, (pred_col) := as.numeric(NA)]
  
  rhs_list <- unique(joined$rhs)
  rhs_list <- rhs_list[!is.na(rhs_list) & nzchar(rhs_list)]
  
  if (length(rhs_list) > 0) {
    for (rhs_expr in rhs_list) {
      idx <- which(joined$rhs == rhs_expr)
      joined[idx, (pred_col) := suppressWarnings(eval(parse(text = rhs_expr)))]
    }
  }
  
  # Zápis výsledků zpět do hlavní pixelové tabulky
  pixels[, (tg) := joined[[pred_col]]]
  pixels[, paste0(tg, "_equation") := joined[["equation"]]]
  pixels[, paste0(tg, "_CV_R2")    := joined[["CV_R2"]]]
  pixels[, paste0(tg, "_CV_RMSE")  := joined[["CV_RMSE"]]]
  
  rm(joined, m_tg)
  gc(verbose = FALSE)
}

# ------------------------------------------------------------
# 7) Export výsledné tabulky
# ------------------------------------------------------------
# Výstup je uložen jako CSV se středníkem, desetinnou čárkou
# a UTF-8 BOM, aby byla zachována kompatibilita s prostředím Excelu
# a nedocházelo k porušení diakritiky.
fwrite(
  pixels,
  file = OUT_PATH,
  sep = ";",
  dec = ",",
  na = "",
  bom = TRUE
)

cat("\nHOTOVO\nVýstup uložen do:\n", OUT_PATH, "\n")
