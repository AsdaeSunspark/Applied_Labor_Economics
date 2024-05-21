sconn_ee_function <- function(M, maxindex) {
  # Computes the largest strongly connected component from matrix M
  #
  # Input
  #   M : matrix of moves. 1 is unemployment and everything else is a company
  #   It is a (nCompany + 1) x 3 matrix with each row : companyIndex1, companyIndex2, numberTransitions
  #   companyIndex 1 corresponds to unemployment
  #
  # Output
  #   sconn_ee : for every index, 1 if it lies in the largest strongly connected component, 0 otherwise
  #   Index 1 (unemployment) always has value 1
  
  # Disconnect unemployment from the rest of the graph
  isnonempsend <- (M[, 1] == 1)
  isnonemprec <- (M[, 2] == 1)
  M[isnonempsend, 3] <- 0
  M[isnonemprec, 3] <- 0
  M <- rbind(M, c(maxindex, maxindex, 0))
  
  # Find strongly connected components and return the biggest one
  ans <- scomponents(M)
  sconn_ee <- (ans$sci == which.max(ans$sizes))
  
  return(sconn_ee)
}