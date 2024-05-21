estimable_subset <- function(M, exptV, ID, g_level, sconn_ee, fo_level, rho_i_rate, delta_i_rate, sectorList, maxindex) {
  # Restricts the strongly connected subset of the graph to the firms that have some employment and
  # hire from unemployment
  # 
  # Input
  #   see compute_M or model_master
  # 
  # Output
  #   M_r keeps the row of the move matrix, M, where the indices of both firms are in the restricted strongly connected
  #   subset
  #   M_out records, for each firm index, the sum of all move coefficients (second column of M) for all transitions into
  #   the firm froms firms that are in the sample. Unemployment is set at 0.
  #   exptV_r is the expt_V vector restricted to the new, smaller sample.
  #   g_level is the g_level vector restricted to the new, smaller sample.
  #   fo_level_r is the fo_level vector restricted to the new, smaller sample.
  #   delta_sector_r computes, on the new, restricted sample, the mean of delta_i_rate, weighted by employment.
  #   rho_sector_r computes, on the new, restricted sample, the mean of rho_i_rate, weighted by employment.
  #   sector_matrix_r restricts the sector matrix to the new, smaller sample.
  #   rho_r gives for each firm the value of rho_sector_r in its economic sector.
  #   delta_r gives for each firm the value of delta_sector_r in its economic sector.
  #   ID_r gives, for each index in the new restricted sample, the original index of the firm (before any was taken out).

  # Expand the three criterion vectors
  indic_employ <- (g_level > 0)
  maxf <- max(fo_level[, 1])
  if (maxf < maxindex) {
    fo_level <- rbind(fo_level, c(maxindex, 0))
    rho_i_rate <- rbind(rho_i_rate, c(maxindex, 0))
    delta_i_rate <- rbind(delta_i_rate, c(maxindex, 0))
  }
  
  indic_hirene <- (rep(0, length(g_level)))
  indic_hirene[fo_level[fo_level[, 2] > 0, 1]] <- 1
  
  # M
  insample <- (1:length(g_level))[(sconn_ee * indic_employ * indic_hirene) != 0]
  insample <- c(1, insample)
  
  M_r <- M[(M[, 1] %in% insample) & (M[, 2] %in% insample), ]

  # exptV
  if (max(ID) < maxindex) {
    ID <- c(ID, maxindex)
    exptV <- c(exptV, 0)
  }
  
  exptV_r <- exptV[(ID %in% insample) == 1]
  
  # g_level
  g_level_r <- g_level[insample]
  
  # fo_level, delta_i_rate, rho_i_rate
  fo_level_r <- rep(0, length(maxindex))
  fo_level_r[fo_level[, 1]] <- fo_level[, 2]
  fo_level_r <- fo_level_r[insample]
  delta_r <- delta_i_rate[delta_i_rate[, 1] %in% insample, 1:2]
  rho_r <- rho_i_rate[rho_i_rate[, 1] %in% insample, 1:2]
  rankDelta <- frank(delta_r[, 1])
  delta_r[rankDelta, ] <- delta_r
  rankRho <- frank(rho_r[, 1])
  rho_r[rankRho, ] <- rho_r
  
  # ID_r
  ID_r <- insample
  
  # sector_matrix_r
  sectorList_r <- sectorList[insample]
  
  # Make delta_r and rho_r sector-specific
  sector_weight <- rep(0, max(sectorList, na.rm = TRUE))
  delta_sector_r <- sector_weight
  rho_sector_r <- sector_weight
  
  jRho <- 1
  jDelta <- 1
  for (i in 1:length(sectorList_r)) {
    if (!is.na(sectorList_r[i])) {
      sector_weight[sectorList_r[i]] <- sector_weight[sectorList_r[i]] + g_level_r[i]
      if (jDelta <= length(delta_r) & (delta_r[jDelta, 1] == insample[i])) {
        delta_sector_r[sectorList_r[i]] <- delta_sector_r[sectorList_r[i]] + g_level_r[i] * delta_r[jDelta, 2]
        jDelta <- jDelta + 1
      }
      if (jRho <= length(rho_r) & (rho_r[jRho, 1] == insample[i])) {
        rho_sector_r[sectorList_r[i]] <- rho_sector_r[sectorList_r[i]] + g_level_r[i] * rho_r[jRho, 2]
        jRho <- jRho + 1
      }
    }
  }
  
  delta_sector_r <- delta_sector_r / sector_weight
  rho_sector_r <- rho_sector_r / sector_weight
  
  delta_r <- delta_sector_r[sectorList_r]
  rho_r <- rho_sector_r[sectorList_r]
  
  return(list(
    M_r = M_r,
    exptV_r = exptV_r,
    g_level_r = g_level_r,
    fo_level_r = fo_level_r,
    ID_r = ID_r,
    rho_r = rho_r,
    delta_r = delta_r,
    rho_sector_r = rho_sector_r,
    delta_sector_r = delta_sector_r,
    sectorList_r = sectorList_r
  ))
}
