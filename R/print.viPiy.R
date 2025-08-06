#'
#' @export
print.viPiy <- function (x, digits = max(3L, getOption("digits") - 3L), ...) 
{
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  if ( is.null(as.list(x$call)$alpha) ) {
    alpha <- formals(viPiy)$alpha
  } else {
    alpha <- as.list(x$call)$alpha
  }
  cat(paste0(round( 100 * ( 1 - alpha ), 2),
             "% Verity intervals for the ATE:\n"))
  print.default(format(x$ATE, digits = digits), print.gap = 2L, 
                quote = FALSE)
  cat("\n")
  
  x.aux <- x
  x.aux$ATE  <- NULL
  x.aux$call <- NULL
  
  cat("gamma proportionality factor between Pi and CTEign:\n")
  for (i in 1:(length(x.aux)) ) {
    printgamma( x.aux[[i]], names(x.aux)[i] )
  }
  
  invisible(x)
}
