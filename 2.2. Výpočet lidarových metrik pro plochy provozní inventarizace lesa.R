# -----------------------------------------------------------
# ÚČEL SCRIPTU
# -----------------------------------------------------------
# Tento script slouží k výpočtu rozšířených LiDARových metrik
# pro jednotlivé inventarizační plochy na základě bodových mračen
# uložených v samostatných souborech LAS. Pro každou plochu jsou
# z bodového mračna vypočteny zejména:
# - základní výškové charakteristiky,
# - výškové kvantily,
# - metriky zastoupení bodů ve fixních výškových třídách,
# - strukturní metriky porostu,
# - metriky podle pořadí návratu paprsku,
# - metriky intenzity odrazu,
# - geometrické charakteristiky bodového mračna,
# - metrika rumple a celkový počet bodů.
#
# Script načítá tabulku zkusných ploch a pro každou plochu
# odpovídající soubor LAS, ze kterého vypočítá sadu LiDARových
# metrik. Tyto metriky jsou následně připojeny k původní tabulce
# ploch prostřednictvím identifikátoru IP_FKEY.
#
# Výstupem je výsledná tabulka obsahující původní atributy ploch
# doplněné o vypočtené LiDARové metriky, která je určena pro další
# analýzy, zejména pro tvorbu regresních a hierarchických modelů.
# -----------------------------------------------------------

library(lidR)
library(dplyr)
library(stringr)
library(e1071)

# -----------------------------------------------------------
# DEFINICE VSTUPNÍCH A VÝSTUPNÍCH CEST
# -----------------------------------------------------------

las_dir  <- "D:/lidar2024_las/plot_las"
csv_path <- "D:/lidar2024_las/sample_plots_R2.csv"
out_path <- "D:/lidar2024_las/sample_plots_with_lidar_metrics.csv"

# -----------------------------------------------------------
# NAČTENÍ TABULKY ZKUSNÝCH PLOCH
# -----------------------------------------------------------

plots <- read.csv(csv_path, stringsAsFactors = FALSE)
stopifnot("IP_FKEY" %in% names(plots))

# -----------------------------------------------------------
# FUNKCE PRO VÝPOČET ROZŠÍŘENÝCH LiDAROVÝCH METRIK
# -----------------------------------------------------------
# Funkce počítá sadu výškových, strukturních, návratových,
# intenzitních a geometrických metrik z bodového mračna
# odpovídajícího jednotlivým zkusným plochám.
# -----------------------------------------------------------

my_metrics_extended <- function(z, i, rn, nr, x, y)
{
  # Odstranění neplatných hodnot ve výšce bodů a synchronizace ostatních vstupů
  idx <- !is.na(z)
  z <- z[idx]; i <- i[idx]; rn <- rn[idx]; nr <- nr[idx]
  x <- x[idx]; y <- y[idx]
  
  n <- length(z)
  if (n < 3) {
    # Při nedostatečném počtu bodů je vrácen fixní počet prázdných metrik
    return(as.list(setNames(rep(NA_real_, 120), paste0("m", 1:120))))
  }
  
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
    quantile(z, probs = c(.01,.05,.10,.20,.25,.30,.40,.50,.60,.70,.75,.80,.90,.95,.99),
             na.rm = TRUE, names = FALSE),
    error = function(e) rep(NA, 15)
  )
  names(pct) <- paste0("z_p", c(1,5,10,20,25,30,40,50,60,70,75,80,90,95,99))
  
  z_skew <- tryCatch(e1071::skewness(z), error = function(e) NA)
  z_kurt <- tryCatch(e1071::kurtosis(z), error = function(e) NA)
  
  # -------------------------------------------
  # 2) METRIKY ZASTOUPENÍ VE FIXNÍCH VÝŠKOVÝCH TŘÍDÁCH
  # -------------------------------------------
  bins <- seq(0, 40, by = 2)
  h <- cut(z, bins, include.lowest = TRUE)
  htab <- table(h)
  
  # Zajištění fixní délky výstupu pro 20 výškových intervalů
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
  
  CRR <- ifelse(
    (z_max - z_min) > 0,
    (z_mean - z_min) / (z_max - z_min),
    NA
  )
  
  # Výpočet entropie a FHD (Foliage Height Diversity)
  z_pos <- z[z > 0]
  if (length(z_pos) > 1) {
    h <- hist(z_pos, breaks = seq(0, max(z_pos) + 1, 1), plot = FALSE)$counts
    h <- h[h > 0]
    if (length(h) > 0) {
      p <- h / sum(h)
      entropy_val <- -sum(p * log(p))
      FHD <- entropy_val / log(length(h))
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
  
  m_first  <- mean(z[is_first], na.rm = TRUE)
  m_last   <- mean(z[is_last], na.rm = TRUE)
  m_single <- mean(z[is_single], na.rm = TRUE)
  m_multi  <- mean(z[is_multi], na.rm = TRUE)
  
  # -------------------------------------------
  # 5) METRIKY INTENZITY ODRAZU
  # -------------------------------------------
  i_mean <- mean(i)
  i_sd   <- sd(i)
  i_cv   <- ifelse(i_mean > 0, i_sd / i_mean, NA)
  
  i_min <- min(i)
  i_max <- max(i)
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
      sphericity <- λ3 / λ1
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
    # výškové metriky
    z_min = z_min, z_max = z_max, z_mean = z_mean, z_sd = z_sd, z_IQR = z_IQR, z_cv = z_cv,
    pct,
    z_skew = z_skew, z_kurt = z_kurt,
    
    # metriky výškových tříd
    hbin,
    
    # strukturní metriky
    prop_z_ge2 = prop_z_ge2, prop_z_ge10 = prop_z_ge10,
    CRR = CRR, entropy = entropy_val, FHD = FHD, gap_frac = gap_frac,
    
    # metriky návratů
    prop_first = prop_first, prop_last = prop_last,
    prop_single = prop_single, prop_multi = prop_multi,
    m_first = m_first, m_last = m_last,
    m_single = m_single, m_multi = m_multi,
    
    # metriky intenzity
    i_mean = i_mean, i_sd = i_sd, i_cv = i_cv,
    i_min = i_min, i_max = i_max, i_range = i_range,
    i_skew = i_skew, i_kurt = i_kurt,
    ipct,
    
    # geometrické metriky
    linearity = linearity, planarity = planarity,
    sphericity = sphericity, anisotropy = anisotropy,
    
    # rumple
    rumple = rumple,
    
    # počet bodů
    n = n
  )
  
  # Výstup je vrácen jako seznam numerických hodnot
  out <- as.list(out)
  return(out)
}

# -----------------------------------------------------------
# VÝPOČET LiDAROVÝCH METRIK PRO VŠECHNY SOUBORY LAS
# -----------------------------------------------------------

las_files <- list.files(las_dir, pattern = "\\.las$", full.names = TRUE)

results <- lapply(las_files, function(f) {
  
  id <- str_match(basename(f), "plot_(\\d+)\\.las")[,2] |> as.numeric()
  
  las <- try(readLAS(f), silent = TRUE)
  if (inherits(las, "try-error") || is.null(las)) {
    return(data.frame(IP_FKEY = id))
  }
  
  m <- cloud_metrics(
    las,
    ~my_metrics_extended(
      Z,
      Intensity,
      ReturnNumber,
      NumberOfReturns,
      X,
      Y
    )
  )
  
  m$IP_FKEY <- id
  m
})

# -----------------------------------------------------------
# PŘIPOJENÍ VYPOČTENÝCH METRIK K TABULCE ZKUSNÝCH PLOCH
# -----------------------------------------------------------

lidar_df <- bind_rows(results)
final_df <- left_join(plots, lidar_df, by = "IP_FKEY")

# -----------------------------------------------------------
# ULOŽENÍ VÝSLEDNÉ TABULKY
# -----------------------------------------------------------

write.csv(final_df, out_path, row.names = FALSE)

message("✔ Hotovo — uložen soubor: ", out_path)
