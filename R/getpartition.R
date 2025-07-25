#' @export
getpartition <- function(xmin, xmax) {
  numpartsoptions <- 5:8
  
  rng <- xmax - xmin
  
  candidates <- lapply(numpartsoptions, function(n) {
    rawstep <- rng / n
    exp <- floor(log10(rawstep))
    basesteps <- c(1, 2, 2.5, 5, 10) * 10^exp
    step <- basesteps[which.min(abs(basesteps - rawstep))]
    list(n = n, step = step)
  })
  
  for (cand in candidates) {
    step <- cand$step
    n <- cand$n
    start <- floor(xmin / step) * step
    end <- ceiling(xmax / step) * step
    points <- seq(start, end, by = step)
    if (length(points) >= n + 1 && length(points) <= n + 3) {
      return(round(points, digits = 10))  #<-># Para evitar errores de punto flotante
    }
  }
  
  return(pretty(c(xmin, xmax), n = 7))
}
