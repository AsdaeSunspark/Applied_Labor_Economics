power_method <- function(SinvM) {
  # Computes fixed point for the input matrix (more exactly, an eigenvector for the largest eigenvalue)
  # Input
  #   SinvM the matrix we want the eigenvector of (it is sparse)
  # 
  # Output
  #   answer the eigenvector
  #   i the number of iterations

  n <- max(SinvM[, 1])
  guess <- rnorm(n, 0, 1)
  guess <- abs(guess) / sum(abs(guess))
  gap <- sum(guess)
  tol <- 10^-4
  
  while ((gap > tol) & (i < 500)) {
    i <- i + 1
    guessNew <- rep(0, length(guess))
    for (k in 1:nrow(SinvM)) {
      guessNew[SinvM[k, 1]] <- guessNew[SinvM[k, 1]] + SinvM[k, 3] * guess[SinvM[k, 2]]
    }
    guessNew <- abs(guessNew) / sum(abs(guessNew))
    gap <- sum(abs(guess - guessNew))
    guess <- guessNew
  }
  
  answer <- guess
  
  return(list(
    answer = answer,
    i = i
  ))
  
}