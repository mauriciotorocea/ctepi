#' @export
accumLayerColor <- function(alpha, layerSolidColor, backgroundColor = "white", n = NULL) {
  backgroundColor <- col2rgb(backgroundColor) / 255
  
  if (length(alpha) != length(layerSolidColor)) {
    stop("alpha y layerSolidColor deben tener la misma longitud.")
  }
  
  if (is.null(n)) {
    n <- rep(1, length(alpha))  #<-># Asigna '1' a cada capa
  } else {
    if (length(n) != length(alpha)) {
      stop("n debe tener la misma longitud que alpha y layerSolidColor.")
    }
  }
  
  accumulatedColor <- backgroundColor
  
  for (i in 1:length(alpha)) {
    layerColor <- col2rgb(layerSolidColor[i]) / 255
    
    alphaeff <- round(alpha[i] * 255) / 255
    
    opacidadacumulada <- 1 - (1 - alphaeff)^n[i]
    
    accumulatedColor <- accumulatedColor * (1 - opacidadacumulada) + layerColor * opacidadacumulada
  }
  
  accumulatedColorHex <- rgb(accumulatedColor[1], accumulatedColor[2], accumulatedColor[3])
  
  return(accumulatedColorHex)
}
