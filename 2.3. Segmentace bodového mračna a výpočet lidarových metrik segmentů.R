# ============================================================
# ÚČEL SCRIPTU
# ============================================================
# Tento script slouží k výpočtu rozšířených LiDARových metrik
# pro pravidelnou čtvercovou síť o velikosti pixelu 500 m²
# z normalizovaných bodových mračen ve formátu LAS/LAZ.
#
# Pro každý pixel jsou vypočteny stejné metriky jako v případě
# inventarizačních ploch, aby bylo možné zajistit metodickou
# srovnatelnost mezi daty ze zkusných ploch a plošně
# odvozenými hodnotami v rámci gridu.
#
# V rámci jednotlivých pixelů jsou určeny zejména:
# - základní výškové charakteristiky,
# - výškové kvantily,
# - metriky zastoupení bodů ve fixních výškových třídách,
# - strukturní metriky porostu,
# - metriky podle pořadí návratu paprsku,
# - metriky intenzity odrazu,
# - geometrické charakteristiky bodového mračna,
# - metrika rumple a celkový počet bodů.
#
# Výstupem scriptu je tabulka gridových buněk s prostorovou
# polohou jejich středu a vypočtenými LiDARovými metrikami,
# která je určena pro následnou aplikaci regresních modelů
# a tvorbu map porostních veličin.
# ============================================================

# ============================================================
# GRID METRICS (pixel_metrics) — stejné metriky jako pro plochy
# ============================================================

library(lidR)
library(future)
library(dplyr)
library(e1071)

# -----------------------------------------------------------
# DEFINICE VSTUPNÍCH A VÝSTUPNÍCH CEST
# -----------------------------------------------------------
input_folder <- "D:/lidar2024_las/normalized"   # LAS/LAZ normalizované na Z
out_csv      <- "D:/lidar2024_las/grid_metrics_500m2_like_plots.csv"

# -----------------------------------------------------------
# DEFINICE VELIKOSTI GRIDU
# -----------------------------------------------------------
# Plocha jedné buňky odpovídá 500 m², což představuje délku
# strany čtverce přibližně 22,36 m.
# -----------------------------------------------------------
grid_res <- sqrt(500)  # ~22.36 m

# -----------------------------------------------------------
# FUNKCE PRO VÝPOČET METRIK
# -----------------------------------------------------------
# Funkce odpovídá definici použité při výpočtu metrik
# pro inventarizační plochy, aby byla zachována
# shodná struktura výstupních proměnných.
# -----------------------------------------------------------
my_metrics_extended <- function(z, i, rn, nr, x, y)
{
  idx <- !is.na(z)
  z  <- z[idx]; i  <- i[idx]; rn <- rn[idx]; nr <- nr[idx]
  x  <- x[idx]; y  <- y[idx]
  
  n <- length(z)
  
  # Vytvoření šablony s chybějícími hodnotami a pevně danými názvy metrik,
  # aby byla zachována stejná struktura výstupu i při nedostatečném počtu bodů
  make_na_out <- function() {
    bins <- seq(0, 40, by = 2)
    pct_names  <- paste0("z_p", c(1,5,10,20,25,30,40,50,60,70,75,80,90,95,99))
    bin_names  <- paste0("bin_", bins[-length(bins)], "_", bins[-1])
    ipct_names <- paste0("i_p", c(5,25,50,75,95))
    
    nm <- c(
      "z_min","z_max","z_mean","z_sd","z_IQR","z_cv",
      pct_names,
      "z_skew","z_kurt",
      bin_names,
      "prop_z_ge2","prop_z_ge10","CRR","entropy","FHD","gap_frac",
      "prop_first","prop_last","prop_single","prop_multi",
      "m_first","m_last","m_single","m_multi",
      "i_mean","i_sd","i_cv","i_min","i_max","i_range","i_skew","i_kurt",
      ipct_names,
      "linearity","planarity","sphericity","anisotropy",
      "rumple","n"
    )
    
    out <- as.list(rep(NA_real_, length(nm)))
    names(out) <- nm
    out
  }
  
  if (n < 3) return(make_na_out())
  
  # -------------------------------------------
  # 1) VÝŠKOVÉ METRIKY
  # -------------------------------------------
  z_min  <- min(z)
  z_max  <- max(z)
  z_mean <- mean(z)
  z_sd   <- sd(z)
  z_IQR  <- IQR(z)
  z_cv   <- ifelse(z_mean > 0, z_sd / z_mean, NA)
  
  pct <- tryCatch(
    quantile(
      z,
      probs = c(.01,.05,.10,.20,.25,.30,.40,.50,.60,.70,.75,.80,.90,.95,.99),
      na.rm = TRUE,
      names = FALSE
    ),
    error = function(e) rep(NA, 15)
  )
  names(pct) <- paste0("z_p", c(1,5,10,20,25,30,40,50,60,70,75,80,90,95,99))
  
  z_skew <- tryCatch(e1071::skewness(z), error = function(e) NA)
  z_kurt <- tryCatch(e1071::kurtosis(z), error = function(e) NA)
  
  # -------------------------------------------
  # 2) METRIKY ZASTOUPENÍ VE FIXNÍCH VÝŠKOVÝCH TŘÍDÁCH
  # -------------------------------------------
  bins <- seq(0, 40, by = 2)
  h <- cut(z, bins, include.lowest = TRUE)  # right = TRUE (výchozí nastavení)
  htab <- table(h)
  
  hbin <- numeric(length(bins) - 1)
  for (k in seq_along(hbin)) {
    label <- levels(h)[k]
    hbin[k] <- ifelse(label %in% names(htab), htab[[label]], 0)
  }
  names(hbin) <- paste0("bin_", bins[-length(bins)], "_", bins[-1])
  
  # -------------------------------------------
  # 3) STRUKTURNÍ METRIKY POROSTU
  # -------------------------------------------
  prop_z_ge2  <- mean(z >= 2)
  prop_z_ge10 <- mean(z >= 10)
  
  CRR <- ifelse((z_max - z_min) > 0,
                (z_mean - z_min) / (z_max - z_min), NA)
  
  z_pos <- z[z > 0]
  if (length(z_pos) > 1) {
    hh <- hist(z_pos, breaks = seq(0, max(z_pos) + 1, 1), plot = FALSE)$counts
    hh <- hh[hh > 0]
    if (length(hh) > 0) {
      p <- hh / sum(hh)
      entropy_val <- -sum(p * log(p))
      FHD <- entropy_val / log(length(hh))
    } else {
      entropy_val <- NA
      FHD <- NA
    }
  } else {
    entropy_val <- NA
    FHD <- NA
  }
  
  gap_frac <- mean(z < 2)
  
  # -------------------------------------------
  # 4) METRIKY PODLE POŘADÍ NÁVRATU PAPRSKU
  # -------------------------------------------
  is_first  <- rn == 1
  is_last   <- rn == nr
  is_single <- nr == 1
  is_multi  <- nr > 1
  
  prop_first  <- mean(is_first)
  prop_last   <- mean(is_last)
  prop_single <- mean(is_single)
  prop_multi  <- mean(is_multi)
  
  m_first  <- mean(z[is_first],  na.rm = TRUE)
  m_last   <- mean(z[is_last],   na.rm = TRUE)
  m_single <- mean(z[is_single], na.rm = TRUE)
  m_multi  <- mean(z[is_multi],  na.rm = TRUE)
  
  # -------------------------------------------
  # 5) METRIKY INTENZITY ODRAZU
  # -------------------------------------------
  i_mean  <- mean(i)
  i_sd    <- sd(i)
  i_cv    <- ifelse(i_mean > 0, i_sd / i_mean, NA)
  i_min   <- min(i)
  i_max   <- max(i)
  i_range <- i_max - i_min
  
  i_skew <- tryCatch(e1071::skewness(i), error = function(e) NA)
  i_kurt <- tryCatch(e1071::kurtosis(i), error = function(e) NA)
  
  ipct <- tryCatch(
    quantile(i, probs = c(.05,.25,.50,.75,.95), na.rm = TRUE, names = FALSE),
    error = function(e) rep(NA, 5)
  )
  names(ipct) <- paste0("i_p", c(5,25,50,75,95))
  
  # -------------------------------------------
  # 6) GEOMETRICKÉ CHARAKTERISTIKY BODOVÉHO MRAČNA
  # -------------------------------------------
  xyz <- cbind(x, y, z)
  C <- tryCatch(cov(xyz), error = function(e) NULL)
  
  if (is.null(C) || any(is.na(C))) {
    linearity = planarity = sphericity = anisotropy = NA
  } else {
    eig <- eigen(C)$values
    eig <- sort(eig, decreasing = TRUE)
    if (any(eig <= 0)) {
      linearity = planarity = sphericity = anisotropy = NA
    } else {
      λ1 <- eig[1]; λ2 <- eig[2]; λ3 <- eig[3]
      linearity  <- (λ1 - λ2) / λ1
      planarity  <- (λ2 - λ3) / λ1
      sphericity <-  λ3 / λ1
      anisotropy <- (λ1 - λ3) / λ1
    }
  }
  
  # -------------------------------------------
  # 7) METRIKA RUMPLE
  # -------------------------------------------
  rumple <- (z_max - z_min) / z_mean
  
  # -------------------------------------------
  # SESTAVENÍ VÝSTUPNÍ SADY METRIK
  # -------------------------------------------
  out <- c(
    z_min = z_min, z_max = z_max, z_mean = z_mean, z_sd = z_sd, z_IQR = z_IQR, z_cv = z_cv,
    pct,
    z_skew = z_skew, z_kurt = z_kurt,
    hbin,
    prop_z_ge2 = prop_z_ge2, prop_z_ge10 = prop_z_ge10,
    CRR = CRR, entropy = entropy_val, FHD = FHD, gap_frac = gap_frac,
    prop_first = prop_first, prop_last = prop_last,
    prop_single = prop_single, prop_multi = prop_multi,
    m_first = m_first, m_last = m_last,
    m_single = m_single, m_multi = m_multi,
    i_mean = i_mean, i_sd = i_sd, i_cv = i_cv,
    i_min = i_min, i_max = i_max, i_range = i_range,
    i_skew = i_skew, i_kurt = i_kurt,
    ipct,
    linearity = linearity, planarity = planarity,
    sphericity = sphericity, anisotropy = anisotropy,
    rumple = rumple,
    n = n
  )
  
  as.list(out)
}

# -----------------------------------------------------------
# VYTVOŘENÍ LAS KATALOGU A NASTAVENÍ PARALELIZACE
# -----------------------------------------------------------
ctg <- readLAScatalog(input_folder)
if (is.null(ctg)) stop("Nelze načíst LAS katalog: ", input_folder)

plan(multisession, workers = 6)

# Doporučené nastavení pro stabilnější a rychlejší zpracování
opt_chunk_size(ctg)   <- 0
opt_chunk_buffer(ctg) <- 0
opt_progress(ctg)     <- TRUE

# -----------------------------------------------------------
# VÝPOČET GRIDOVÝCH METRIK
# -----------------------------------------------------------
metrics_raster <- pixel_metrics(
  ctg,
  ~my_metrics_extended(Z, Intensity, ReturnNumber, NumberOfReturns, X, Y),
  res = grid_res
)

# Převod výsledku do tabulkové podoby včetně souřadnic středu pixelu
metrics_df <- as.data.frame(metrics_raster, xy = TRUE)

# -----------------------------------------------------------
# ULOŽENÍ VÝSLEDNÉ TABULKY
# -----------------------------------------------------------
write.csv(metrics_df, out_csv, row.names = FALSE)
message("✔ Hotovo — uloženo: ", out_csv)
