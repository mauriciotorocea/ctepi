#' @export
plot.viPiy <- function( Pi, xmin = NA, xmax = NA , ymin = NA, ymax = NA ,
                        gammascale = T,
                        xlab = "Y", 
                        pch=20, cex.points = 0.2, lwd=2, lty = 1, 
                        lwd.vertical=1, lty.vertical = 2,
                        ylab = ifelse(gammascale,"CTE",expression(Pi)), 
                        main = ifelse(gammascale,"CTE",expression(Pi)),
                        line.title = 0.5 + 0.5*!gammascale ,
                        col=NULL, 
                        col.lines=NA, 
                        addlegend=TRUE,
                        legend = NULL,
                        add=FALSE){
  
  rangeYobs <- 1.05 * attr(Pi, "rangeYobs")
  
  if ( is.data.frame(Pi) ) Pi <- list( "Converted" = Pi )
  
  
  Pi$ATE <- NULL
  Pi$call <- NULL
  
  if ( is.null(col) ) {
    col <- rainbow( length(Pi) , s = 0.8, v = 0.8, start = 0)
  }
    
  if (gammascale) {
    Pi <- lapply(Pi, function(df) {
      df$EpPi <- df$gamma * df$EpPi
      df$L    <- df$gamma * df$L
      df$U    <- df$gamma * df$U
      return(df)
    })
  }
  
  if ( anyNA(xmin) ) xmin <- rangeYobs[1]
  if ( anyNA(xmax) ) xmax <- rangeYobs[2]
  if ( anyNA(ymin) ) ymin <- 1.10 * min(sapply(Pi, function(df) min(df$L, na.rm = TRUE)), na.rm = TRUE)
  if ( anyNA(ymax) ) ymax <- 1.10 * max(sapply(Pi, function(df) max(df$U, na.rm = TRUE)), na.rm = TRUE)
  
  if (!add) {
    gridplot( xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, xlab = xlab, ylab = ylab)
  }
    
  i <- 0
  for (df in Pi) {
    i <- i+1
    CTEemp <- stepfun(df$y, c(0, df$EpPi))
    lines(CTEemp, do.points = FALSE, verticals = FALSE, col = col[i], 
          pch = pch, lwd = lwd, lty = lty)
    
    # Incluir líneas verticales con otro estilo
    segments(x0 = df$y[-length(df$y)], y0 = df$EpPi[-length(df$EpPi)], 
             x1 = df$y[-1], y1 = df$EpPi[-length(df$EpPi)], 
             col = col[i], lty = lty.vertical, lwd = lwd.vertical)
    
    # Uso de addStepBounds para dibujar las cotas como escalones
    addStepBounds( stepfun(df$y, c(0, df$L)),
                   stepfun(df$y, c(0, df$U)),
                   col.bounds = col[i], 
                   alpha.bounds = 0.3,
                   col.lines = col.lines[i] )
  }
  
  title(main = main, cex.main=0.9, adj=0, line= line.title )
  
  if ( addlegend & !add ) {
    if (length(Pi)==1) {
      if ( is.null(legend) ) {
        legend <- c( ifelse( gammascale, "CTEign", expression(E(Pi)) ) ,"95% VI")
      }
      graphics::legend("topleft",
             legend = legend ,
             pch=c(19,15),
             pt.cex = c(1, 1.7) , # tamaño de los símbolos pch
             col = c(col, AlphaCol(col,0.3)),
             lty=c(1,NA), lwd=2, cex=0.7, bg = "white",
             box.col = "gray",
             horiz=F, text.width = NA )
    } else {
      if ( is.null(legend) ) {
        legend <- names(Pi)
      }
      graphics::legend("topleft", cex=0.7,
             legend = legend , 
             lty = 1,  lwd=2,
             col = col, 
             pt.cex = 2, 
             pt.bg = "gray",  
             bg = "white",
             box.col = "gray" )
    }
    
  }
}
