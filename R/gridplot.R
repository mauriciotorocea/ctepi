#' @export
gridplot <- function(  x.grid=NULL, y.grid=NULL,
                       xmin = NA, xmax = NA,
                       ymin = NA, ymax = NA,
                       x.drawminorgrid = T, 
                       y.drawminorgrid = T,
                       cex.axis = 0.8 , 
                       col.grid = "#d9d9d9" ,
                       col.bg = "#e7e8ea" ,
                       col.line = "red",
                       las=1, type.plot="l" ,
                       pch = 20 ,
                       style = 'classic',
                       xlab="x", ylab="y",
                       xlab.line=2.3, 
                       ylab.line=2.5, 
                       x=NULL,y=NULL, 
                       add.plot=FALSE,
                       x.axes = T,
                       y.axes = T,
                       ...){
  
  check_numeric_scalar <- function(x) {
    is.numeric(x) && length(x) == 1 && !is.na(x)
  }
  if (check_numeric_scalar(xmin) && check_numeric_scalar(xmax)) {
    if (xmin >= xmax) stop("xmin must be less than xmax.")
  }
  if (check_numeric_scalar(ymin) && check_numeric_scalar(ymax)) {
    if (ymin >= ymax) stop("ymin must be less than ymax.")
  }
  
  if (is.null(x.grid) || anyNA(x.grid)) {
    if (!(check_numeric_scalar(xmin) && check_numeric_scalar(xmax))) {
      stop("Provide either x.grid or both valid xmin and xmax.")
    }
    x.grid <- getpartition(xmin, xmax)
  }
  if (is.null(y.grid) || anyNA(y.grid)) {
    if (!(check_numeric_scalar(ymin) && check_numeric_scalar(ymax))) {
      stop("Provide either y.grid or both valid ymin and ymax.")
    }
    y.grid <- getpartition(ymin, ymax)
  }
  
  x.lim <- c( ifelse(!is.na(xmin) , xmin , min(x.grid) ) ,
              ifelse(!is.na(xmax) , xmax , max(x.grid) ) )
  y.lim <- c( ifelse(!is.na(ymin) , ymin , min(y.grid) ) ,
              ifelse(!is.na(ymax) , ymax , max(y.grid) ) )
  
  plot( x=1 , y=1 , 
        xlim = x.lim, ylim = y.lim,
        bty = "n" ,         # gráfico sin borde 
        fg = "white" ,      # color del borde del gráfico
        xaxt="n", yaxt="n", # no añado ejes
        type = "n",         # No grafico puntos
        col = NA,
        xaxs = "i", yaxs = "i", # internal (no expansion of the ranges)
        xlab="", ylab=""
  )
  
  mtext(xlab,side=1, line=xlab.line ) #<-># line es la distancia al gráfico.
  mtext(ylab,side=2, line=ylab.line ) #<-># line es la distancia al gráfico.
  
  par(new = TRUE)
  rect(par("usr")[1], par("usr")[3],
       par("usr")[2], par("usr")[4],
       col = ifelse( style == 'ggplot' , col.bg , NA) ,
       border = ifelse( style == 'ggplot' , "white" , col.grid ) )
  
  if (x.axes) {
    axis(1 , x.grid , las = las , cex.axis = cex.axis ,  #<-># eje x
         lwd=0 , lwd.ticks=1 ) 
  }
  if (y.axes) {
    axis(2 , y.grid , las = las , cex.axis = cex.axis ,  #<-># eje y
         lwd=0 , lwd.ticks=1 ) #<-># eje y
  }
  
  abline( h = y.grid , v = x.grid , 
          col = ifelse( style == 'ggplot' , "white" , col.grid) ,
          lwd = ifelse( style == 'ggplot' , 1.5 , 1 )  )
  abline( h = y.lim[1] , v = x.lim[1] , lwd=2 ,
          col = ifelse( style == 'ggplot' , "white" , col.grid)  )
  
  if (x.drawminorgrid) {
    deltax <- x.grid[-1] - x.grid[-length(x.grid)]
    abline( v = x.grid[-length(x.grid)] + deltax/2 , 
            col = ifelse( style == 'ggplot' , "white" , col.grid) ,
            lwd = ifelse( style == 'ggplot' , 0.7 , 0.5 )  )
  }
  if (y.drawminorgrid) {
    deltay <- y.grid[-1] - y.grid[-length(y.grid)]
    abline( h = y.grid[-length(y.grid)] + deltay/2 , 
            col = ifelse( style == 'ggplot' , "white" , col.grid) ,
            lwd = ifelse( style == 'ggplot' , 0.7 , 0.5 )  )
  }
  
  if (add.plot) { #<-># add.plot igual a FALSE hace que no grafique
    lines( x , y , pch = pch, col = col.line, type = type.plot , ...)
  }
} 

