.deltay <- function(Yobs, C=NULL, y) {
  
  if ( is.null(C) ) return( 1*(Yobs > y) )
  
  aux <- Yobs > y
  aux[is.na(aux)] <- FALSE
  
  stopifnot(length(Yobs) == length(C))
  
  out <- rep(NA_integer_, length(Yobs))
  
  out[aux & C %in% c(0, 1)] <- 1
  out[!aux & C %in% c(0, -1)] <- 0
  out[ is.na(Yobs) ] <- NA
  
  return(out)
}
