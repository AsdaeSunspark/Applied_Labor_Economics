sparse_to_csr <- function(A) {
  # Converts a mtrix from sparse to CSR format
  #
  # Input
  #   A is a n x 3 matrix where the first column corresponds to row indices, the
  #   second corresponds to column indices, and the last one to values
  #
  # Output
  #   CSR is a n x 3 matrix where the first column is the row pointer (indices
  #   of the other columns where each row starts), second column gives the
  #   column index for each value, and the third column gives the values
  
  A[rank(A[, 1], ties.method = "random"),] <- A
  
  nbRows <- max(A[, 1])
  A <- A[A[, 3] != 0,]
  curRow <- 0
  rp <- rep(nrow(A) + 1, nbRows + 1)
  for (i in 1:nrow(A)) {
    if (A[i, 1] > curRow) {
      for (j in (curRow+1):A[i, 1]) {
        rp[j] <- i
        curRow <- A[i, 1]
      }
    }
  }
  
  return(list(
    rp = rp,
    ci = A[, 2],
    val = A[, 3]
  ))
}
