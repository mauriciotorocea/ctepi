#' @export
plot.possibleworlds <- function( pw, ...) {
  oldpar <- par(no.readonly = TRUE)
  
  if (interactive()) {
    par(ask = TRUE)
    on.exit(par(oldpar))  
  }
  
  pw |> degreeoftruth() |> plot(...)
  pw |> EpPiplot(...)
}

