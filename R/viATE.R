#' @export
viATE <- function( viPiyObj ){
  ATE <- c()
  for (obj in viPiyObj) {
    aux1 <- approxfun(obj$y, obj$L * obj$gamma, method = "constant")
    aux2 <- approxfun(obj$y, obj$U * obj$gamma, method = "constant")
    class(aux1) <- class(aux2) <- c("stepfun", class(aux2))
    ATE <- rbind( ATE ,
                     c( "Lower bound" = integrate.sf( aux1 )( max(obj$y)*100 ) ,
                        "Upper bound" = integrate.sf( aux2 )( max(obj$y)*100 ) ) )
  }
  rownames(ATE) <- names(viPiyObj)
  result <- list( ATE=ATE )
  result$call <- match.call()
  result
}
