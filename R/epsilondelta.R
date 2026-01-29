#' @export
epsilondelta <- function(Y, Z, 
                         delta.seq = pretty( c(0,diff( range(Y, na.rm = T) )/20) , n = 10), 
                         eta= c(0, 0.5, 1), 
                         ysup = max(Y, na.rm = TRUE)*100,
                         positive.effect = TRUE, 
                         output.noroot = NA,
                         y1.limit=NULL, y0.limit=NULL, equalsuppY1Y0=F, 
                         eps.tol = .Machine$double.eps, max.iter = 100) {
  if (!positive.effect) Y <- -Y
  
  result <- sapply(eta, function(current_eta) {
    sapply(delta.seq, function(delta) {
      target_fun <- function(eps) {
        ctef.aux <- ctefunctions(Y, Z,
                                 eps11 = eps, eps12 = eps,
                                 eps01 = eps, eps02 = eps,
                                 eta11 = current_eta, eta12 = current_eta,
                                 eta01 = current_eta, eta02 = current_eta, 
                                 y1.limit = y1.limit, y0.limit = y0.limit, 
                                 equalsuppY1Y0 = equalsuppY1Y0)
        integrate.sf( ctef.aux$CTEbounds.eps$CTElb , returnfunction=TRUE )(ysup) - delta
      }
      
      f0 <- tryCatch(target_fun(0), error = function(e) NA)
      f1 <- tryCatch(target_fun(1), error = function(e) NA)
      
      if (is.na(f0) || is.na(f1)) {
        NA
      } else if (sign(f0) == sign(f1)) {
        ifelse( f0>0 , output.noroot , NA )
      } else {
        tryCatch(uniroot(target_fun, c(0, 1), tol = eps.tol, maxiter = max.iter)$root,
                 error = function(e) NA)
      }
    })
  })
  
  if (length(delta.seq)>1) {
    rownames(result) <- paste0("delta=", signif(delta.seq, 4))
    colnames(result) <- paste0("eta=", signif(eta, 4))
  } else {
    names(result) <- paste0("eta=", signif(eta, 4))
  }
  
  return(result)
}
