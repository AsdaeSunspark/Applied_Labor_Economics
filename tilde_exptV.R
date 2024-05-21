tilde_exptV <- function(M, maxindex) {
  # computes the values (exponentiated) given M
  # Input
  #   M : matrix of moves. 1 is unemployment and everything else is a company
  #   It is a (nCompany + 1) x 3 matrix with each row : companyIndex1, companyIndex2, numberTransitions
  #   companyIndex 1 corresponds to unemployment
  #   maxindex : number of companies + 1 (for unemployment)
  # 
  # Output
  #   ID : conversion of indices (for the reduced matrix on the strongly connected component) to the original indices
  #   exptV : exponentiated values for each of these firms
  
  M <- rbind(M, c(maxindex, maxindex, 0))
  M[frankv(list(x = M[, 1], y = M[, 2]), cols = c("x", "y"), ties.method = "random"), ] <- M
  answer <- scomponents(M)
  cind <- which.max(answer$sizes)
  p <- (answer$sci == cind)
  ID <- (1:maxindex)[p]
  
  M <- M[(M[, 1] %in% ID) & (M[, 2] %in% ID), 1:3]
  
  newIndex1 <- rep(0, nrow(M))
  index1 <- M[, 1]
  j <- 1
  for (i in 1:length(ID)) {
    while ((j <= length(index1)) & (index1[j] == ID[i])) {
      newIndex1[j] <- i
      j <- j + 1
    }
  }
  M[, 1] <- newIndex1
  
  rank2 <- frank(M[, 2], ties.method = "random")
  newIndex2 <- rep(0, nrow(M))
  index2 <- sort(M[, 2])
  j <- 1
  for (i in 1:length(ID)) {
    while ((j <= length(index2)) & (index2[j] == ID[i])) {
      newIndex2[j] <- i
      j <- j + 1
    }
  }
  M[rank2, 2] <- newIndex2
  
  S <- rep(0, length(ID))
  for (i in 1:nrow(M)) {
    S[M[i, 1]] <- S[M[i, 1]] + M[i, 3]
  }
  SinvM <- M
  for (i in 1:nrow(M)) {
    SinvM[i, 3] <- M[i, 3] / S[M[i, 1]]
  }
  
  answer <- power_method(SinvM)
  
  exptV <- abs(answer$answer)
  
  return(list(
    exptV = exptV,
    ID = ID
  ))
}