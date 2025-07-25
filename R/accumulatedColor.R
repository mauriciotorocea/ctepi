#' @export
accumulatedColor <- function(n, alpha, layerSolidColor, backgroundColor = "white" ) {
  backgroundColor <- col2rgb( backgroundColor )/255
  
  alphaeff <- round(alpha * 255) / 255
  
  layerSolidColor <- col2rgb(layerSolidColor) / 255
  
  opacidadacumulada <- 1 - (1 - alphaeff)^n
  
  colorfinal <- backgroundColor * (1 - opacidadacumulada) + layerSolidColor * opacidadacumulada
  
  colorfinalhex <- rgb(colorfinal[1], colorfinal[2], colorfinal[3])
  
  return(colorfinalhex)
}
accumulatedColor <- Vectorize(accumulatedColor,vectorize.args = "n")

