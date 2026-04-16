# ------------------------------------------------------------
# ÚČEL SCRIPTU
# ------------------------------------------------------------
# Tento script slouží k výpočtu základních porostních veličin
# na úrovni inventarizačních ploch z tabulky jednotlivě měřených stromů.
# Na základě vstupních dat jsou pro každou plochu vypočteny zejména:
# - počet stromů na hektar (N/ha),
# - kruhová základna na hektar (G/ha),
# - zásoba na hektar (V/ha),
# - Loreyova střední výška,
# - dominantní výška Hdom20,
# - dominantní dřevina podle kruhové základny,
# - druhová klasifikace podle pravidla 70 %.
#
# Script současně porovnává dva způsoby přepočtu stromových dat:
# 1) variantu PIL, která respektuje metodiku soustředných kruhů
#    a rozdílné expanzní faktory podle tloušťky stromů,
# 2) variantu X20, ve které jsou všechny stromy přepočteny
#    jednotnou vahou z plochy 500 m² na hektar.
#
# Výstupem jsou samostatné tabulky pro obě varianty a také
# společná srovnávací tabulka, která umožňuje posoudit vliv
# zvoleného způsobu přepočtu na výsledné porostní charakteristiky.
# ------------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)

# ------------------------------------------------------------
# FUNKCE PRO PŘEVOD KÓDŮ DŘEVIN DO AGREGOVANÝCH KATEGORIÍ
# ------------------------------------------------------------
# Funkce převádí původní druhové označení dřevin (DR_ZKR)
# do sedmi sjednocených kategorií používaných v další analýze.
# ------------------------------------------------------------
map_species <- function(x) {
  x <- trimws(as.character(x))
  case_when(
    x == "SM" ~ "SM",
    x == "BK" ~ "BK",
    x %in% c("DB", "DBZ", "DBC") ~ "DB",
    x == "MD" ~ "MD",
    x %in% c("BO", "BOC") ~ "BO",
    x %in% c("JD", "DG", "KS", "SMP", "TP", "JR") ~ "ostatní jehličnaté",
    x %in% c(
      "HB", "LP", "JS", "JV", "BR", "BB", "AK", "TR", "OL",
      "JIV", "JL", "OS", "JLH", "BRK", "LTX", "OR", "HR", "VJ", "KL"
    ) ~ "ostatní listnaté",
    TRUE ~ NA_character_
  )
}

# ------------------------------------------------------------
# FUNKCE PRO VÝPOČET DOMINANTNÍ VÝŠKY Hdom20
# ------------------------------------------------------------
# Dominantní výška je počítána jako vážený průměr výšek stromů,
# které představují 20 % nejtlustších jedinců podle přepočtu na hektar.
# Vážení je provedeno pomocí hodnoty N/ha.
# ------------------------------------------------------------
hdom20_one <- function(d) {
  d <- d[order(d$TLOUSTKA_K, decreasing = TRUE), , drop = FALSE]
  totalN <- sum(d$w_ha, na.rm = TRUE)
  if (!is.finite(totalN) || totalN <= 0) return(NA_real_)
  
  target <- 0.2 * totalN
  cumN <- 0
  num <- 0
  den <- 0
  
  for (i in seq_len(nrow(d))) {
    if (cumN >= target) break
    take <- min(d$w_ha[i], target - cumN)
    num <- num + d$MOD_VYSKA[i] * take
    den <- den + take
    cumN <- cumN + take
  }
  
  if (den == 0) NA_real_ else num / den
}

# ------------------------------------------------------------
# FUNKCE PRO VÝPOČET POROSTNÍCH VELIČIN A DRUHOVÉ DOMINANCE
# ------------------------------------------------------------
# Funkce pro každou inventarizační plochu vypočítá:
# - počet stromů na hektar,
# - kruhovou základnu na hektar,
# - zásobu na hektar,
# - Loreyovu střední výšku,
# - dominantní výšku Hdom20,
# - dominantní dřevinu podle maximální kruhové základny,
# - dominantní skupinu dřevin podle pravidla 70 %.
# ------------------------------------------------------------
compute_table <- function(trees) {
  
  # Výpočet dominantní výšky pro jednotlivé plochy
  hdom_tab <- trees %>%
    group_by(IP_FKEY) %>%
    group_modify(~ tibble(Hdom20_m = hdom20_one(.x))) %>%
    ungroup()
  
  # Výpočet základních porostních veličin
  metrics <- trees %>%
    group_by(IP_FKEY) %>%
    summarise(
      N_ha     = sum(w_ha, na.rm = TRUE),
      G_ha_m2  = sum(g_m2 * w_ha, na.rm = TRUE),
      V_ha_m3  = sum(TO_MODV_BK * w_ha, na.rm = TRUE),
      LoreyH_m = sum(MOD_VYSKA * g_m2 * w_ha, na.rm = TRUE) / sum(g_m2 * w_ha, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(hdom_tab, by = "IP_FKEY")
  
  # Určení dominantní dřeviny podle nejvyšší hodnoty součtu (g * w)
  dom_by_g <- trees %>%
    group_by(IP_FKEY, DRUH) %>%
    summarise(Gw = sum(gw, na.rm = TRUE), .groups = "drop") %>%
    arrange(IP_FKEY, desc(Gw)) %>%
    group_by(IP_FKEY) %>%
    slice(1) %>%
    ungroup() %>%
    transmute(IP_FKEY, DREVINA_DOM_Gw = DRUH)
  
  # Výpočet podílů kruhové základny jednotlivých kategorií dřevin
  shares_wide <- trees %>%
    group_by(IP_FKEY, DRUH) %>%
    summarise(Gw = sum(gw, na.rm = TRUE), .groups = "drop") %>%
    group_by(IP_FKEY) %>%
    mutate(
      G_total = sum(Gw, na.rm = TRUE),
      Gshare = ifelse(G_total > 0, Gw / G_total, NA_real_)
    ) %>%
    ungroup() %>%
    select(IP_FKEY, DRUH, Gshare) %>%
    pivot_wider(names_from = DRUH, values_from = Gshare, values_fill = 0)
  
  # Doplnění chybějících kategorií dřevin
  need_cols <- c("SM", "BK", "DB", "MD", "BO", "ostatní jehličnaté", "ostatní listnaté")
  for (cc in need_cols) {
    if (!cc %in% names(shares_wide)) shares_wide[[cc]] <- 0
  }
  
  # Určení výsledné dominantní skupiny podle pravidla 70 %
  rule70 <- shares_wide %>%
    mutate(
      DREVINA_70G = case_when(
        SM >= 0.70 ~ "SM",
        BK >= 0.70 ~ "BK",
        MD >= 0.70 ~ "MD",
        BO >= 0.70 ~ "BO",
        DB >= 0.70 ~ "DB",
        (SM + MD + BO + `ostatní jehličnaté`) >= (BK + DB + `ostatní listnaté`) ~ "ostatní jehličnaté",
        TRUE ~ "ostatní listnaté"
      ),
      Gshare_SM = SM,
      Gshare_BK = BK,
      Gshare_DB = DB,
      Gshare_MD = MD,
      Gshare_BO = BO,
      Gshare_ostatni_jeh  = `ostatní jehličnaté`,
      Gshare_ostatni_list = `ostatní listnaté`
    ) %>%
    select(IP_FKEY, DREVINA_70G, starts_with("Gshare_"))
  
  metrics %>%
    left_join(rule70, by = "IP_FKEY") %>%
    left_join(dom_by_g, by = "IP_FKEY")
}

# ------------------------------------------------------------
# 1) NAČTENÍ VSTUPNÍCH DAT
# ------------------------------------------------------------

infile <- "D:/LHP2023/vz_bez_parezu.xlsx"
df <- read_excel(infile)

# Převod vybraných sloupců na numerický formát
# (náhrada desetinné čárky za desetinnou tečku)
df <- df %>%
  mutate(across(
    c(TLOUSTKA_K, VZD_KM, MOD_VYSKA, TO_MODV_BK),
    ~ as.numeric(gsub(",", ".", as.character(.)))
  ))

# ------------------------------------------------------------
# 2) SPOLEČNÁ PŘÍPRAVA DAT
# ------------------------------------------------------------
# V této části dochází k:
# - převedení druhového označení do sjednocených kategorií,
# - výpočtu kruhové základny jednotlivých stromů,
# - odstranění záznamů s chybějícími klíčovými hodnotami.
# ------------------------------------------------------------
base <- df %>%
  mutate(
    DRUH = map_species(DR_ZKR),
    g_m2 = pi * (TLOUSTKA_K / 200)^2
  ) %>%
  filter(
    !is.na(IP_FKEY),
    !is.na(DRUH),
    !is.na(TLOUSTKA_K),
    !is.na(MOD_VYSKA),
    !is.na(g_m2)
  )

# ------------------------------------------------------------
# 3A) VARIANTA PIL
# ------------------------------------------------------------
# Varianta respektuje princip soustředných kruhů a použití
# různých vah podle tloušťkových prahů a vzdálenosti stromu
# od středu inventarizační plochy.
# ------------------------------------------------------------
r_small <- 3
r_mid   <- 7
r_full  <- 12.62

w_small <- 10000 / (pi * r_small^2)
w_mid   <- 10000 / (pi * r_mid^2)
w_full  <- 20

trees_PIL <- base %>%
  mutate(
    w_ha = case_when(
      TLOUSTKA_K >= 30 & VZD_KM <= r_full  ~ w_full,
      TLOUSTKA_K >= 12 & VZD_KM <= r_mid   ~ w_mid,
      TLOUSTKA_K >= 7  & VZD_KM <= r_small ~ w_small,
      TRUE ~ NA_real_
    ),
    gw = g_m2 * w_ha
  ) %>%
  filter(!is.na(w_ha), !is.na(VZD_KM), !is.na(TO_MODV_BK))

out_PIL <- compute_table(trees_PIL) %>%
  mutate(variant = "PIL")

# ------------------------------------------------------------
# 3B) VARIANTA X20
# ------------------------------------------------------------
# Ve variantě X20 mají všechny stromy stejnou váhu odpovídající
# přepočtu z plochy 500 m² na 1 hektar.
# ------------------------------------------------------------
trees_X20 <- base %>%
  mutate(
    w_ha = 20,
    gw = g_m2 * w_ha
  ) %>%
  filter(!is.na(TO_MODV_BK))

out_X20 <- compute_table(trees_X20) %>%
  mutate(variant = "X20")

# ------------------------------------------------------------
# 4) EXPORT VÝSTUPNÍCH TABULEK
# ------------------------------------------------------------
# Uloženy jsou:
# - výsledky pro variantu PIL,
# - výsledky pro variantu X20,
# - společná tabulka obou variant pro vzájemné porovnání.
# ------------------------------------------------------------
write.csv(out_PIL, "D:/LHP2023/PIL_metrics_variant_PIL.csv", row.names = FALSE)
write.csv(out_X20, "D:/LHP2023/PIL_metrics_variant_X20.csv", row.names = FALSE)

out_both <- bind_rows(out_PIL, out_X20)
write.csv(out_both, "D:/LHP2023/PIL_metrics_variants_PIL_vs_X20.csv", row.names = FALSE)
