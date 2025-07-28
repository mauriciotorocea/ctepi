hnormalcorrection <- function(p1, n1, p0, n0, method = "gcd") {
  
  if ( ! method %in% c("average","gcd") ) stop('Error: invalid method. Use "average" or "gcd"')
  
  # Fork of MASS::.rat() function
  .rat <- function (x, cycles = 10, max.denominator = 2000) {
    a0 <- rep(0, length(x))
    A <- matrix(b0 <- rep(1, length(x)))
    fin <- is.finite(x)
    B <- matrix(floor(x))
    r <- as.vector(x) - drop(B)
    len <- 0
    while (any(which <- fin & (r > 1/max.denominator)) && (len <- len + 
                                                           1) <= cycles) {
      a <- a0
      b <- b0
      a[which] <- 1
      r[which] <- 1/r[which]
      b[which] <- floor(r[which])
      r[which] <- r[which] - b[which]
      A <- cbind(A, a)
      B <- cbind(B, b)
    }
    pq1 <- cbind(b0, a0)
    pq <- cbind(B[, 1], b0)
    len <- 1
    while ((len <- len + 1) <= ncol(B)) {
      pq0 <- pq1
      pq1 <- pq
      pq <- B[, len] * pq1 + A[, len] * pq0
    }
    pq[!fin, 1] <- x[!fin]
    #list(rat = pq, x = x)
    pq
  }
    
  if ( method == "gcd" ) {
    frac1 <- .rat(p1 / n1)
    frac2 <- .rat(p0 / n0)
    
    num1 <- frac1[1]
    den1 <- frac1[2]
    num2 <- frac2[1]
    den2 <- frac2[2]
    
    gcd_int <- function(a, b) {
      if (b == 0) abs(a) else gcd_int(b, a %% b)
    }
    
    a_int <- num1 * den2
    b_int <- num2 * den1
    gcd_val <- gcd_int(a_int, b_int)
    h <- gcd_val / (den1 * den2)
  } 
  else if( method == "average" )  {
    h <- 0.5 * ( p1 / n1  +  p0 / n0 )
  }
  
  return(h)
}

