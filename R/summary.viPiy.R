#'
#' @export
summary.viPiy <- function(x, digits = 3) {
  y <- eval( unlist(x$call)$y )
  probMZ <- attr( x, "probMZ")
  rows <- lapply(probMZ, function(w) as.numeric( rownames(w) ) )
  
  probMZ <- lapply(probMZ, function(df) {
    for ( col in c("probK1Z1", "probK1Z0", "gamma") ) {
      if (col %in% names(df)) {
        df[[col]] <- formatC(df[[col]], digits = digits, format = "f")
      }
    }
    df
  })
  
  intervals <- lapply(rows, function(indices) {
    points <- indices[-1]
    cuts <- y[points]
    starts <- c(-Inf, cuts)
    ends   <- c(cuts, Inf)
    starts_fmt <- ifelse(is.infinite(starts), "-Inf", formatC(starts, digits = digits, format = "f"))
    ends_fmt   <- ifelse(is.infinite(ends),   "Inf",  formatC(ends,   digits = digits, format = "f"))
    
    paste0("[", starts_fmt, ", ", ends_fmt, ")")
  })
  
  detailsSubpop <- Map(cbind, intervals, probMZ)
  #colnames(detailsSubpop) <- c("y range", "M", "P(K=1|Z=1)", "P(K=1|Z=0)", "gamma")
  
  detailsSubpop <- lapply(detailsSubpop, function(df) {
    colnames(df) <- c("y", "M", "P(K=1|Z=1)", "P(K=1|Z=0)", "gamma")
    rownames(df) <- NULL
    df
  })
  
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
  
  cat( ifelse( length(x.aux)>1, "Summary of subpopulations:\n", "Summary of population:\n") )
  #print(detailsSubpop, row.names = FALSE)
  for (name in names(detailsSubpop)) {
    if (length(x.aux)>1) cat(sprintf("\nSubpopulation: %s\n", name))
    print(noquote(format(detailsSubpop[[name]], trim = TRUE)), row.names = FALSE)
  }
}
