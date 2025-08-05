#' @export
EpPiplot <- function( CTEpr, 
					  xmin = 0, 
					  xmax = 1, 
					  ymin = NA, 
					  ymax = NA, 
					  add = F, 
					  addlegend = T, 
                      lwd = 2, 
                      col.EpPi ="black", 
                      col.VI = "gray", 
                      col.VI.alpha = 0.3,
                      addCriteria = T , 
                      legend.location = NA ){
  
  if ( anyNA(ymin) ) {
    ymin <- range( CTEpr$Pi$EpPi )[1] * 1.10
  }
  if ( anyNA(ymax) ) {
    ymax <- range( CTEpr$Pi$EpPi )[2] * 1.10
  }
  
  p <- CTEpr$p
  criteria <- c( criterion1 = CTEpr$probMZ$probZ1K1 * CTEpr$probMZ$probY1Z1K1 + CTEpr$probMZ$probZ0K1 * CTEpr$probMZ$probY1Z0K1 ,
                 DeltaL = min(CTEpr$probMZ$probY1Z1K1 , CTEpr$probMZ$probY1Z0K1) ,
                 DeltaU = max(CTEpr$probMZ$probY1Z1K1 , CTEpr$probMZ$probY1Z0K1) )
  
  if (!add) {
    gridplot(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax,
             xlab = "Prior propensity p", ylab = expression(Pi), 
             xlab.line = 2.5 )
    abline(h=0, lwd=1.5, col="gray")
  }
  
  if (addCriteria & !add ) {
    polygon( criteria[c(2,3,3,2)] , c( c(-1,-1) , rev(c(2,2)) ) ,  
             border = NA , col=AlphaCol("blue3",0.08) )
    abline( v = criteria[1], col=AlphaCol("gray",0.3) , lty=1, lwd=2)
  }
  
  # Plot Ep(Pi)
  lines( p , CTEpr$Pi$EpPi , lwd=lwd, col=col.EpPi )
  
  # Adding verity interval computed when CTEpr was created
  polygon( c( p, rev(p) ) , 
           c( CTEpr$Pi$qalpha2 , rev(CTEpr$Pi$q1malpha2) ) ,
           border = NA , col=AlphaCol(col.VI, col.VI.alpha ) )  
  
  if (addlegend & !add) {
    if ( anyNA(legend.location) ) {
      legend.location.x <- ifelse( CTEpr$probMZ$probK1Z0 - CTEpr$probMZ$probK1Z1 > 0 , "topleft" , "bottomleft")
      legend.location.y <- NULL
    } else {
      if ( length(legend.location) == 1 ) {
        legend.location.x <- legend.location
        legend.location.y <- NULL
      } else {
        legend.location.x <- legend.location[1]
        legend.location.y <- legend.location[2]
      }
    }
    if ( is.null(as.list(CTEpr$call)$alpha) ) {
      alpha <- formals(viPiy)$alpha
    } else {
      alpha <- as.list(CTEpr$call)$alpha
    }
    legend( x = legend.location.x, y = legend.location.y,
            cex=0.7, 
            legend = c(expression(E[p](Pi)), paste0( round( 100 * ( 1 - alpha ), 2) ,"% VI") ), 
            lty = c(1, NA),  pch = c(NA, 15),  
            col = c(col.EpPi, AlphaCol(col.VI, col.VI.alpha) ), 
            pt.cex = 2,  # Size of the rectangle
            pt.bg = "gray",  
            bg = "white",
            box.col = "gray" )
  }
  
}
