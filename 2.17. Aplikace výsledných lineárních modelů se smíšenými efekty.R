# ============================================================
# ÚČEL SKRIPTU
# Skript slouží k plošné aplikaci pěti finálních regresních
# modelů na tabulku pixelů čtvercové sítě. Podporuje jak klasické
# lineární modely (LM), tak hierarchické smíšené modely (LMM)
# vytvořené pomocí balíčku lme4. Pro každou modelovanou taxační
# veličinu vypočítává predikci včetně varianty založené pouze
# na fixní části modelu. U veličin modelovaných na logaritmické
# škále provádí bias-korigovanou zpětnou transformaci do původní
# škály. Výstupem je rozšířená tabulka pixelů určená pro další
# analýzu a využití v prostředí GIS.
# ============================================================

# ============================================================
# APLIKACE 5 REGRESNÍCH MODELŮ (LMM lme4 NEBO LM) NA PIXELY
# (TAB_GRID_FIN_FIN)
# - modelované veličiny: H_DOM, H_LOREY, G_per_ha, V_per_ha, N_per_ha
# - modely jsou uloženy ve formátu .rds (LM nebo LMM z balíčku lme4)
# - u veličin modelovaných na logaritmické škále
#   (G_per_ha, V_per_ha, N_per_ha) platí:
#     predict() vrací eta na logaritmické škále a následně je
#     provedena bias-korigovaná zpětná transformace:
#       y = exp(eta + 0.5 * sigma^2)
# - výsledné predikce pro pixely:
#     <target>_pred       = conditional predikce
#                           (fixní + náhodná složka) u LMM,
#                           běžná predikce u LM
#     <target>_pred_fixed = predikce pouze z fixní části u LMM,
#                           shodná s běžnou predikcí u LM
# - skript zahrnuje robustní načtení pixelové tabulky
#   (automatická detekce oddělovače + očištění názvů sloupců)
# - součástí je také kontrola požadovaných sloupců a zarovnání
#   úrovní faktorových proměnných podle trénovacích dat modelu
# - export je přizpůsoben pro české prostředí MS Excel:
#   oddělovač ; + desetinná čárka + kódování CP1250
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(lme4)
  library(stringr)
  library(reformulas)  # funkce nobars / findbars
  library(readr)
})

# --------------------------
# 0) CESTY K SOUBORŮM
# --------------------------
PIXELS_PATH <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_test/Vystupy_GIS/TAB_GRID_FIN_FIN.csv"

MODELS <- list(
  H_DOM     = "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/LLM_PIL/OUT_MIXED_MODELS/MODEL_H_DOM.rds",
  H_LOREY   = "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/LLM_PIL/OUT_MIXED_MODELS/MODEL_H_LOREY.rds",
  G_per_ha  = "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/LLM_PIL/OUT_MIXED_MODELS/MODEL_G_per_ha.rds",
  V_per_ha  = "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/LLM_PIL/OUT_MIXED_MODELS/MODEL_V_per_ha.rds",
  N_per_ha  = "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/LLM_PIL/OUT_MIXED_MODELS/MODEL_N_per_ha.rds"
)

# Pokud chceš logaritmicky modelované veličiny načítat automaticky,
# ponech vyplněnou cestu SUMMARY_PATH.
# Pokud ne, nastav SUMMARY_PATH <- NA_character_ a použije se
# ručně definovaný vektor LOG_TARGETS_FALLBACK.
SUMMARY_PATH <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/LLM_PIL/OUT_MIXED_MODELS/MODELS_summary.csv"

OUT_PATH <- "C:/Users/Martin/Desktop/LDF/_ING/Dip_Les/R_true/LLM_PIL/OUT_MIXED_MODELS/PIL_TAB_GRID__LMM_predictions_BACKTRANSFORM.csv"

# --------------------------
# 1) NASTAVENÍ
# --------------------------
# Záložní definice veličin modelovaných na logaritmické škále.
# Použije se v případě, že soubor summary není dostupný nebo
# neobsahuje potřebnou informaci o použité škále.
LOG_TARGETS_FALLBACK <- c("G_per_ha","V_per_ha","N_per_ha")

# --------------------------
# 2) POMOCNÉ FUNKCE
# --------------------------
is_mer <- function(mod) inherits(mod, "merMod")

# Získání proměnných fixní části modelu
# (bez náhodných efektů)
get_fixed_vars <- function(mod) {
  ff <- reformulas::nobars(formula(mod))
  vars <- all.vars(ff)
  # První prvek představuje závisle proměnnou, proto je odebrán.
  unique(vars[-1])
}

# Získání seskupovacích proměnných použitých
# v náhodné části modelu
get_group_vars <- function(mod) {
  bars <- reformulas::findbars(formula(mod))
  if (length(bars) == 0) return(character(0))
  groups <- vapply(bars, function(b) as.character(b[[3]]), character(1))
  unique(groups)
}

# Souhrnný seznam všech sloupců potřebných pro predikci
get_required_cols <- function(mod) {
  unique(c(get_fixed_vars(mod), get_group_vars(mod)))
}

# Pokus o získání trénovacích dat modelu
# pro následné zarovnání faktorových úrovní
get_train_frame <- function(mod) {
  mf <- tryCatch(model.frame(mod), error = function(e) NULL)
  if (!is.null(mf)) return(mf)
  if (is_mer(mod)) {
    mf2 <- tryCatch(mod@frame, error = function(e) NULL)
    return(mf2)
  }
  NULL
}

# Zarovnání faktorových úrovní v nových datech
# podle trénovacích dat použitého modelu
align_factor_levels <- function(mod, newdata) {
  mf <- get_train_frame(mod)
  if (is.null(mf)) return(newdata)
  
  for (nm in names(mf)) {
    if (!nm %in% names(newdata)) next
    if (is.factor(mf[[nm]])) {
      newdata[[nm]] <- factor(newdata[[nm]], levels = levels(mf[[nm]]))
    }
  }
  newdata
}

# Výpočet obou variant predikce:
# - conditional = fixní + náhodná složka
# - fixed-only  = pouze fixní část modelu
predict_model_both <- function(mod, newdata) {
  if (is_mer(mod)) {
    # Conditional predikce: fixní + náhodná složka.
    # Pokud některé úrovně náhodných efektů v nových datech chybí,
    # použije se jejich příspěvek jako 0.
    p_cond <- predict(mod, newdata = newdata, allow.new.levels = TRUE)
    
    # Predikce pouze z fixní části modelu
    p_fix  <- predict(mod, newdata = newdata, re.form = NA, allow.new.levels = TRUE)
    
    return(list(cond = as.numeric(p_cond), fixed = as.numeric(p_fix)))
  } else {
    p <- as.numeric(predict(mod, newdata = newdata))
    return(list(cond = p, fixed = p))
  }
}

# Bias-korigovaná zpětná transformace z logaritmické škály
backtransform_log <- function(eta, mod) {
  s2 <- sigma(mod)^2
  exp(eta + 0.5 * s2)
}

# --------------------------
# 3) IDENTIFIKACE LOGARITMICKY MODELOVANÝCH VELIČIN
#    (AUTOMATICKY / ZÁLOŽNÍ VARIANTA)
# --------------------------
LOG_TARGETS <- LOG_TARGETS_FALLBACK

if (!is.na(SUMMARY_PATH) && file.exists(SUMMARY_PATH)) {
  sumtbl <- tryCatch(readr::read_csv(SUMMARY_PATH, show_col_types = FALSE), error = function(e) NULL)
  if (!is.null(sumtbl) && all(c("target","scale") %in% names(sumtbl))) {
    lt <- sumtbl$target[sumtbl$scale == "log"]
    lt <- lt[!is.na(lt) & lt != ""]
    if (length(lt) > 0) {
      LOG_TARGETS <- unique(as.character(lt))
      message("LOG_TARGETS načteny ze summary: ", paste(LOG_TARGETS, collapse = ", "))
    } else {
      message("Summary bylo načteno, ale nebyly nalezeny žádné veličiny se scale == 'log'. Používám záložní definici.")
    }
  } else {
    message("Soubor summary se nepodařilo načíst nebo neobsahuje sloupce target a scale. Používám záložní definici.")
  }
} else {
  message("SUMMARY_PATH neexistuje nebo je NA. Používám záložní definici LOG_TARGETS.")
}

# --------------------------
# 4) NAČTENÍ PIXELOVÉ TABULKY
# --------------------------
message("Načítám pixely: ", PIXELS_PATH)

dt <- data.table::fread(
  PIXELS_PATH,
  sep = "auto",
  encoding = "UTF-8",
  na.strings = c("", "NA", "NaN", "NULL"),
  quote = "\""
)

# Očištění BOM a uvozovek v názvech sloupců
# (časté zejména u exportů z ArcGIS)
data.table::setnames(dt, names(dt), gsub("^\\ufeff", "", names(dt)))
data.table::setnames(dt, names(dt), gsub('^"|"$', "", names(dt)))

message("Nahráno řádků: ", nrow(dt), " | sloupců: ", ncol(dt))

# --------------------------
# 5) NAČTENÍ MODELŮ A VÝPOČET PREDIKCÍ
# --------------------------
for (target in names(MODELS)) {
  model_path <- MODELS[[target]]
  message("\n---\nTarget: ", target, "\nModel: ", model_path)
  
  if (!file.exists(model_path)) stop("Chybí modelový soubor: ", model_path)
  
  mod <- readRDS(model_path)
  message("Třída objektu: ", paste(class(mod), collapse = ", "))
  
  # Kontrola, zda tabulka pixelů obsahuje všechny sloupce
  # potřebné pro daný model
  req <- get_required_cols(mod)
  missing <- setdiff(req, names(dt))
  if (length(missing) > 0) {
    stop(
      "V tabulce pixelů chybí sloupce potřebné pro model '", target, "':\n  - ",
      paste(missing, collapse = "\n  - "),
      "\n\nTip: zkontroluj, zda pixely obsahují také sloupce pro náhodné efekty ",
      "(např. skupina / Hclass / ...) a zda názvy sloupců přesně odpovídají ",
      "trénovacím datům modelu."
    )
  }
  
  # Zarovnání faktorových proměnných podle trénovacích dat modelu
  dt_aligned <- align_factor_levels(mod, dt)
  
  # Predikce na škále modelu (eta)
  preds <- predict_model_both(mod, dt_aligned)
  eta_cond <- preds$cond
  eta_fix  <- preds$fixed
  
  # Zpětná transformace do původní škály,
  # pokud je cílová veličina modelována logaritmicky
  if (target %in% LOG_TARGETS) {
    y_cond <- backtransform_log(eta_cond, mod)
    y_fix  <- backtransform_log(eta_fix,  mod)
  } else {
    y_cond <- eta_cond
    y_fix  <- eta_fix
  }
  
  # Uložení výsledků do tabulky pixelů
  dt[[paste0(target, "_pred")]]       <- as.numeric(y_cond)
  dt[[paste0(target, "_pred_fixed")]] <- as.numeric(y_fix)
  
  # Ochrana proti záporným hodnotám:
  # všechny modelované taxační veličiny mají smysluplný rozsah >= 0
  dt[[paste0(target, "_pred")]]       <- pmax(dt[[paste0(target, "_pred")]], 0)
  dt[[paste0(target, "_pred_fixed")]] <- pmax(dt[[paste0(target, "_pred_fixed")]], 0)
  
  message(
    "OK: zapsány sloupce: ",
    paste0(target, "_pred"), ", ", paste0(target, "_pred_fixed"),
    if (target %in% LOG_TARGETS) " (provedena zpětná transformace z logaritmické škály)" else ""
  )
}

# --------------------------
# 6) EXPORT VÝSLEDNÉ TABULKY
# --------------------------
message("\nUkládám výstup (formát vhodný pro CZ Excel): ", OUT_PATH)

# write.csv2:
# - používá oddělovač ;
# - používá desetinnou čárku
# - fileEncoding = "CP1250" zachovává správné zobrazení češtiny
#   v prostředí MS Excel na Windows
write.csv2(dt, OUT_PATH, row.names = FALSE, fileEncoding = "CP1250")

message("Hotovo.")
