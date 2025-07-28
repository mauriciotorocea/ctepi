#' 
#' @import parallel
#' 
#' @export
multiOSlapply <- function(X, FUN, mc.cores = 1, ...) {
  if (Sys.info()["sysname"] == "Windows") {
    cl <- parallel::makeCluster(mc.cores)
    on.exit(parallel::stopCluster(cl), add = TRUE) 
    parallel::clusterExport(cl, ls(globalenv()))  
    parallel::parLapply(cl, X, FUN, ...)
  } else {
    parallel::mclapply(X, FUN, mc.cores = mc.cores, ...)
  }
}
