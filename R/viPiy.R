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
    if ( is.vector(X) ) X <- matrix(X, ncol=1, dimnames = list( NULL, "X") )
    nlist <- X |> unique() |> nrow()
  }
  
  result <- list()
  probMZ <- list()
  for (i in 1:nlist) {
    result[[i]] <- data.frame()
    probMZ[[i]] <- data.frame()
  } 
  for (i in 1:length(y)) {
    Yobs.aux <- .deltay( Yobs, censoring, y[i] )
    
    CTEpd <- Piy(Yobs=Yobs.aux, Zobs=Zobs, p = "PY1K1", alpha=alpha , suppressMessages = TRUE,
                 X = X, covariates = !is.null(covariates) , approxtonorm = approxtonorm)
    
    if ( nlist == 1) CTEpd <- list( "Marginal" = CTEpd )
    for (j in 1:nlist) {
      result[[j]] <- rbind(  result[[j]]  ,  c(y[i], CTEpd[[j]]$Pi$qalpha2 , CTEpd[[j]]$Pi$q1malpha2, CTEpd[[j]]$p, CTEpd[[j]]$Pi$EpPi , CTEpd[[j]]$gamma)   )
      colnames(result[[j]]) <- c("y","L","U","p","EpPi","gamma")
      
      probMZ[[j]] <- rbind( probMZ[[j]] , 
                            c( unlist(CTEpd[[j]]$probMZ[c("M","probK1Z1","probK1Z0")]) , "gamma"=CTEpd[[j]]$gamma) )
      colnames(probMZ[[j]]) <- c("M","probK1Z1","probK1Z0","gamma")
    }
  }
  
  names(result) <- names(CTEpd)
  names(probMZ) <- names(CTEpd)
  
  class(result) <- c( "viPiy", class(result) )
  result$ATE <- viATE(result)$ATE

  attr( result, "rangeYobs") <- range(Yobs, na.rm = TRUE)
  attr( result, "probMZ") <- lapply(probMZ, unique) 

  result$call <- match.call()
  
  result
}
