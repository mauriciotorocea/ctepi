#' Cumulative integral for step functions
#'
#' This function returns the cumulative integral of a step function.
#'
#' @param sf A step function of class "stepfun".
#' 
#' @return A function of class "sfintegrate" and "stepfun", which represents the cumulative integral of the input step function.
#' 
#'
#' @examples
#' # Example of a step function
#' x <- c(0, 1, 2, 3, 4)
#' y <- c(0, 1, 0, 1, 0)
#' sf <- stepfun(x, y)
#'
#' # Integrate the step function
#' integral_sf <- integrate.sf(sf, returnfunction=TRUE)
#'
#' # Plot the original step function and its integral
#' plot(sf, main = "Step Function")
#' plot(integral_sf, main = "Integral of Step Function")
#'
#' 
#' @export
integrate.sf <- function( sf , lower=NULL, upper=NULL, returnfunction=FALSE) {
  # Modifico la función para poder integrar con límites personalizados
  #x_vals <- knots(sf)
  x_vals <- c( lower, knots(sf) , upper )
  if (!is.null(lower)) x_vals <- x_vals[x_vals >= lower]
  if (!is.null(upper)) x_vals <- x_vals[x_vals <= upper]
  
  y_vals <- sf(x_vals)
  
  dx <- diff(x_vals)
  segment_areas <- dx * y_vals[-length(y_vals)]
  cumulative_areas <- cumsum(c(0, segment_areas))
  
  if ( returnfunction ) {
    integral <- approxfun(x_vals, cumulative_areas, 
                          method = "linear", yleft = 0, yright = cumulative_areas[length(cumulative_areas)], ties = "ordered")
    
    class(integral) <- c( "sfintegrate" , "stepfun" , class(integral) )
    attr(integral, "call") <- sys.call()
    
    assign("x_vals", x_vals, envir = environment(integral))
    assign("y_vals", y_vals, envir = environment(integral))
  } else {
    integral <- tail(cumulative_areas, 1)
  }
  
  return(integral)
}
