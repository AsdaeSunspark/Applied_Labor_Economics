compute_M <- function(ee_rate_contract, en_rate_contract, ee, ene, index1, index2, year1, maxindex, W, g_level, ee_counterfactual, en_counterfactual) {
#   Computes the M (move) matrix
#     M_jk = Number of workers entering j from k
#     Index number 1 is unemployment
#     index1 and index2 are the company id before and after transition, respectively
#   
#   Key variables
#     ee_rate_exog, en_rate_exog are the maximum of 0 or the deviation in employee turnover for a given firm-year pair,
#     relative to the mean for its production sector over the whole period (for firms undergoing growth). It is expressed as 
#     a percentage of the rate of employee turnover for the given firm-year pair. The sample is restricted to firms undergoing
#     a contraction in economic activity.
#     ee_endog, en_endog are 1 minus the previous rates.
#     endogweight_ee is a column vector for all EE job transitions, with the value from ee_endog corresponding to the firm-year pair
#     (firm being index1 i.e. the firm the transition is out from). Zeros are transformed into 0.01.
#     endogweight_en is a column vector for all EN job transitions, with the value from en_endog corresponding to the firm-year pair
#     (firm being index1 i.e. the firm the transition is out from)
#     endogweight_ne is a column vector for all NE job transitions, with value 1 as observation weight.
#     (EN and NE transitions arise from splitting ENE transitions in 2).
#     M is obtained as a matrix where each ordered pair of firms A, B (or unemployment for index 1) is represented on a row with firm
#     A in the first column, firm B in the second, and in the third column the corresponding endogweight_ee multiplied by the number of
#     transitions from A to B observed.
#     Mo is similar to M but the third column just counts the number of transitions observed.
#     delta_i_rate records, for all (observed) firm indices, the sum over all EN transitions out of the firm of the en_rate_exog divided by
#     the employment level in the firm over the whole period (second column). The first column is the firm indices. It represents the rate
#     of exogenous job destructions.
#     rho_i_rate is similar but for al EE transitions out of the firm. It represents the rate of exogenous job transitions.
#     fo_level is the number of NE transitions into each firm (second column), given by the index in the first column.
#   
#   Input
#     see model_master
#   
#   Output
#     M, Mo, rho_i_rate, delta_i_rate, fo_level
  
  # Compute ee_exog and en_exog for all possible separations
  ee_rate_excess <- matrix(rep(0, 5 * length(ee_counterfactual)), ncol = 5)
  en_rate_excess <- matrix(rep(0, 5 * length(en_counterfactual)), ncol = 5)
  
  for (i in 1:5) {
    ee_rate_excess[, i] <- max(0, ee_rate_contract[, i] - ee_counterfactual)
    en_rate_excess[, i] <- max(0, en_rate_contract[, i] - en_counterfactual)
  }
  
  en_rate_exog <- en_rate_excess / (en_rate_contract + (en_rate_contract == 0))
  ee_rate_exog <- ee_rate_excess / (ee_rate_contract + (ee_rate_contract == 0))
  
  # Compute M matrix
  
  ### EE
  index1_ee <- index1[!is.na(ee) & (ee == 1)]
  index2_ee <- index2[!is.na(ee) & (ee == 1)]
  year1_ee <- year1[!is.na(ee) & (ee == 1)]
  
  entry_ee <- index1_ee + (year1_ee - 2002)*maxindex
  endogweight_ee <- 1 - ee_rate_exog[entry_ee]
  endogweight_ee <- endogweight_ee + 0.01*(endogweight_ee == 0)
  
  ### EN
  index1_ene <- index1[!is.na(ene) & (ene == 1) & (year1 != 2007)]
  index2_ene <- index2[!is.na(ene) & (ene == 1) & (year1 != 2007)]
  year1_ene <- year1[!is.na(ene) & (ene == 1) & (year1 != 2007)]
  
  entry_en <- index1_ene + maxindex*(year1_ene - 2002)
  endogweight_en <- 1 - en_rate_exog[entry_en]
  
  endogweight_en <- endogweight_en + 0.01 * (endogweight_en == 0)
  
  ### Combine EE, EN and NE
  index1_comb <- c(index1_ee, index1_ene, rep(1, length(index1_ene)))
  index2_comb <- c(index2_ee, rep(1, length(index1_ene)), index2_ene)
  endogweight_comb <- c(endogweight_ee, endogweight_en, rep(1, length(index1_ene)))
  
  ### Order index1_comb, index2_comb in lexicographic order
  rerank <- frankv(list(x = index1_comb, y = index2_comb), cols = c("x", "y"))
  index1_comb[rerank] <- index1_comb
  index2_comb[rerank] <- index2_comb
  
  ### Accumulate arrays to compute M and M0
  i <- 1
  for (j in 2:length(index1_comb)) {
    if (!(index1_comb[j - 1] == index1_comb[j]) | !(index2_comb[j - 1] == index2_comb[j])) {
      i <- i + 1
    }
  }
  
  cM <- rep(0, 3*i)
  cMo <- rep(0, 3*i)
  
  i <- 0
  for (j in 1:length(index1_comb)) {
    if (j == 1) {
      i <- i + 1
      cM[1 + 3*(i - 1)] <- index1_comb[j]
      cM[2 + 3*(i - 1)] <- index2_comb[j]
      cMo[1 + 3*(i - 1)] <- index1_comb[j]
      cMo[2 + 3*(i - 1)] <- index2_comb[j]
    }
    else if (!(index1_comb[j - 1] == index1_comb[j]) | !(index2_comb[j - 1] == index2_comb[j])) {
      i <- i + 1
      cM[1 + 3*(i - 1)] <- index1_comb[j]
      cM[2 + 3*(i - 1)] <- index2_comb[j]
      cMo[1 + 3*(i - 1)] <- index1_comb[j]
      cMo[2 + 3*(i - 1)] <- index2_comb[j]
    }
    cM[3 + 3*(i - 1)] <- cM[3 + 3*(i - 1)] + endogweight_comb[j]
    cMo[3 + 3*(i - 1)] <- cMo[3 + 3*(i - 1)] + 1
  }
  
  
  M <- matrix(cM, ncol = 3, byrow = TRUE)
  Mo <- matrix(cMo, ncol = 3, byrow = TRUE)
  
  # Compute delta, rho and f
  
  ### delta
  rank1 <- frank(index1_ene, ties.method = "random")
  index1_ene[rank1] <- index1_ene
  exogweight_en <- 1 - endogweight_en
  exogweight_en[rank1] <- exogweight_en
  indexDelta <- unique(index1_ene)
  delta_i_level <- rep(0, length(indexDelta))
  j <- 1
  for (i in 1:length(indexDelta)) {
    while ((j <= length(index1_ene)) & (index1_ene[j] == indexDelta[i])) {
      delta_i_level[i] <- delta_i_level[i] + exogweight_en[j]
      j <- j + 1
    }
  }
  delta_i_level <- matrix(c(indexDelta, delta_i_level), ncol = 2)
  delta_i_rate <- delta_i_level
  delta_i_rate[, 2] <- delta_i_level[, 2] / g_level[indexDelta]
    
  ### rho
  rank1 <- frank(index1_ee, ties.method = "random")
  index1_ee[rank1] <- index1_ee
  exogweight_ee <- 1 - endogweight_ee
  exogweight_ee[rank1] <- exogweight_ee
  indexRho <- unique(index1_ee)
  rho_i_level <- rep(0, length(indexRho))
  j <- 1
  for (i in 1:length(indexRho)) {
    while ((j <= length(index1_ee)) & (index1_ee[j] == indexRho[i])) {
      rho_i_level[i] <- rho_i_level[i] + exogweight_ee[j]
      j <- j + 1
    }
  }
  rho_i_level <- matrix(c(indexRho, rho_i_level), ncol = 2)
  rho_i_rate <- rho_i_level
  rho_i_rate[, 2] <- rho_i_level[, 2] / g_level[indexRho]
  
  ### f0
  rank2 <- frank(index2_ene, ties.method = "random")
  index2_ene[rank2] <- index2_ene
  indexFo <- unique(index2_ene)
  fo_level <- rep(0, length(indexFo))
  j <- 1
  for (i in 1:length(indexFo)) {
    while ((j <= length(index2_ene)) & (index2_ene[j] == indexFo[i])) {
      fo_level[i] <- fo_level[i] + 1
      j <- j + 1
    }
  }
  fo_level <- matrix(c(indexFo, fo_level), ncol = 2)
  
  nonemployerid <- 1

return(list(
  M = M,
  Mo = Mo,
  rho_i_rate = rho_i_rate,
  delta_i_rate = delta_i_rate,
  fo_level = fo_level
))
  
}