#' @export
degreeoftruth <- function( CTEpr , epsilon=0 ){
  
  p <- CTEpr$p
  criteria <- c( criterion1 = CTEpr$probMZ$probZ1K1 * CTEpr$probMZ$probY1Z1K1 + CTEpr$probMZ$probZ0K1 * CTEpr$probMZ$probY1Z0K1 ,
                 DeltaL = min(CTEpr$probMZ$probY1Z1K1 , CTEpr$probMZ$probY1Z0K1) ,
                 DeltaU = max(CTEpr$probMZ$probY1Z1K1 , CTEpr$probMZ$probY1Z0K1) )
  
  rval <- list()
  for (eps in 1:length(epsilon)) {
    # Computing probabilities of events E+, E- and E0. Exact and approximate methods.
    if ( is.null(CTEpr$masses) ) {
      # Approximation to normal distribution
      dfaux <- data.frame( EpPi = CTEpr$Pi$EpPi , sqrtVpPi = sqrt(CTEpr$Pi$VpPi) )
      h <- hnormalcorrection(p1 = 1-CTEpr$probMZ$probK1Z1, n1 = CTEpr$probMZ$n1,
                             p0 = 1-CTEpr$probMZ$probK1Z0, n0 = CTEpr$probMZ$n0)
      p_positive_effect <- apply( dfaux , 1, function(x) {pnorm( epsilon[eps] + h/2 , x[1] , x[2] , lower.tail = F) } )
      p_negative_effect <- apply( dfaux , 1, function(x) {pnorm( -epsilon[eps] - h/2 , x[1] , x[2] , lower.tail = T) } )
      p_no_effect <- apply( dfaux , 1, function(x) {
        pnorm( epsilon[eps]  + h/2 , x[1] , x[2] , lower.tail = T) - pnorm( -epsilon[eps]  - h/2 , x[1] , x[2] , lower.tail = T) 
      } )
      
    } else {
      # Exact method
      masses <- CTEpr$masses
      
      # Calculating the probabilities of negative and positive causal effects
      p_negative_effect <- apply( masses[ masses$cte < -epsilon[eps],-1] , 2, sum )
      p_positive_effect <- apply( masses[ masses$cte > epsilon[eps],-1] , 2, sum )
      # Calculating the probabilities of no causal effects
      ine <- (masses$cte <= epsilon[eps]) & (masses$cte >= -epsilon[eps])
      if ( sum( ine )>0 ) {
        p_no_effect <- apply( masses[ ine ,-1] , 2, sum )
      } else {
        p_no_effect <- rep( 0 , length(p_positive_effect) )
        names(p_no_effect) <- names(p_negative_effect)
      }
    }
    
    rval[[eps]] <- list(p_positive_effect = p_positive_effect, 
                        p_negative_effect = p_negative_effect, 
                        p_no_effect = p_no_effect )
  }
  
  # Rename list elements
  names(rval) <- paste0( "epsilon", epsilon )
  
  class(rval) <- c( "degreeoftruth", class(rval) )
  attr(rval, "p") <- p
  attr(rval, "criteria") <- criteria
  attr(rval, "epsilon") <- epsilon
  
  rval
}
