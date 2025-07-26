#' @export
viPiy <- function(Yobs, Zobs, y , alpha=0.05, X=NULL, covariates=NULL, 
                  censoring=NULL, approxtonorm=TRUE) {
  
  #<-> Si entrego censoringType, lo limpio para que sea vector de valores -1,0,1
  if ( !is.null(censoring) ) {
    #<-> Función auxiliar
    cleancensoringType <- function(censoringType) {
      ct_char <- as.character(censoringType)
      result <- rep(0L, length(ct_char))
      result[ct_char %in% c("-1", "l", "L", "left")] <- -1L
      result[ct_char %in% c("1", "r", "R", "right", "u")] <- 1L
      result
    }
    censoring <- cleancensoringType(censoring)
  }
  
  if ( is.null(X) | is.null(covariates) ) {
    nlist <- 1
  } else {
    nlist <- X |> unique() |> nrow()
  }
  
  result <- list()
  for (i in 1:nlist) {
    result[[i]] <- data.frame()
  }
  for (i in 1:length(y)) {
    Yobs.aux <- deltay( Yobs, censoring, y[i] )
    
    CTEpd <- ctepi::CTEprobdata(Yobs=Yobs.aux, Zobs=Zobs, p = "PY1K1", alpha=alpha , suppressMessages = TRUE,
                                X = X, covariates = !is.null(covariates) , approxtonorm = approxtonorm)
    
    if ( nlist == 1) CTEpd <- list( "Marginal" = CTEpd )
    for (j in 1:nlist) {
      result[[j]] <- rbind(  result[[j]]  ,  c(y[i], CTEpd[[j]]$Pi$qalpha2 , CTEpd[[j]]$Pi$q1malpha2, CTEpd[[j]]$p, CTEpd[[j]]$Pi$EpPi , CTEpd[[j]]$gamma)   )
      colnames(result[[j]]) <- c("y","L","U","p","EpPi","gamma")
    }
  }
  
  if (nlist>1) {
    names(result) <- names(CTEpd)
  }
  
  attr( result, "rangeYobs") <- range(Yobs, na.rm = TRUE)
  class(result) <- c( "viPiy", class(result) )
  
  result
}



