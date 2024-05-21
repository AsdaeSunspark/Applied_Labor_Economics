given_lambda_ee_gap_specific <- function(M_r, exptV_r, g_level_r, fo_level_r, ID_r, delta_r, rho_r, lambda1) {
  # computes the lambda that minimises the gap

  M <- M_r
  g_level <- g_level_r
  fo_level <- fo_level_r
  exptV <- exptV_r
  
  hiresNE <- sum(fo_level)
  W <- sum(g_level)
  g <- g_level / W
  fo <- fo_level / hiresNE
  
  g_level_ns <- g_level * (1 - delta_r) * (1 - rho_r)
  W_ns <- sum(g_level_ns)
  g_ns <- g_level_ns / W_ns
  
  # Shocks
  b <- exptV * g_ns / fo
  a <- exptV[1] * hiresNE / W
  
  # Average EE transition probability
  ee_prob_data <- sum(M[(M[, 1] != 1) & (M[, 2] != 1), 3]) / W_ns
  g <- g[2:length(g)]

  # Recover V^e
  a2 = a / (1 - lambda1)
    
  CexpV <- b - a2

  CexpV[CexpV <= 0] <- min(CexpV[CexpV > 0])  
  
}