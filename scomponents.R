scomponents <- function(A) {
  # Computes the strongly connected components of the graph
  # Input
  #   A : adjacency matrix (non-zero = edge, matrix is directed), in sparse
  #   format
  # 
  # Output
  #   sci : vertex by vertex component index
  #   sizes : component by component size
  #   rp : row pointers for edges (cf csr matrix format)
  #   ci : column indices for edges (cf csr matrix format)

  A <- sparse_to_csr(A)
  
  n <- length(A$rp) - 1
  sci <- rep(0, n)
  cn <- 1
  
  root <- rep(0, n)
  dt <- rep(0, n)
  t <- 0
  
  cs <- rep(0, n)
  css <- 0
  
  rs <- rep(0, 2*n)
  rss <- 0
  
  for (sv in 1:n) {
    
    v <- sv
    if (root[v] > 0) {
      next
    }
    
    rss <- rss + 1
    rs[2*rss - 1] <- v
    rs[2*rss] <- A$rp[v]
    
    root[v] <- v
    sci[v] <- -1
    dt[v] <- t
    t <- t + 1
    
    css <- css + 1
    cs[css] <- v
    
    while (rss > 0) {
    
      v <- rs[2*rss - 1]
      ri <- rs[2*rss]
      rss <- rss - 1
      
      while (ri < A$rp[v + 1]) {
        
        w <- A$ci[ri]
        ri <- ri + 1

        if (root[w] == 0) {
          root[w] <- w
          sci[w] <- -1
          dt[w] <- t
          t <- t + 1
          
          css <- css + 1
          cs[css] <- w
          
          rss <- rss + 1
          rs[2*rss - 1] <- v
          rs[2*rss] <- ri
          
          v <- w
          ri <- A$rp[w]
          
          next
        }
      }
      
      if (A$rp[v] >= A$rp[v + 1]) {
        if (A$rp[v] > A$rp[v + 1]) {
          print("Bug")
        }
      }
      else {
      for (ri in A$rp[v]:(A$rp[v + 1] - 1)) {
        
        w <- A$ci[ri]
        
        if (sci[w] == -1) {
          if (dt[root[v]] > dt[root[w]]) {
            root[v] <- root[w]
          }
        }
        
        
      }}
      
      if (root[v] == v) {
        while (css > 0) {
          w <- cs[css]
          css <- css - 1
          sci[w] <- cn
          
          if (w == v) {
            break
          }
        }
        
        cn <- cn + 1
      }
      
      
    }
  }
  
  sizes <- rep(0, max(sci))
  
  for (i in 1:length(sci)) {
    sizes[sci[i]] <- sizes[sci[i]] + 1
  }
  
  return(list(
    sci = sci,
    sizes = sizes,
    rp = A$rp,
    ci = A$ci
  ))
}