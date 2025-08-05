printgamma <- function(df, group = "gamma factor", digits = 3) {
  df <- df[order(df$y), c("y", "gamma")]
  
  change_points <- c(TRUE, df$gamma[-1] != df$gamma[-nrow(df)])
  idx <- which(change_points)
  
  gammas <- df$gamma[idx]
  y_breaks <- df$y[idx][-1]  
  
  if (length(gammas) == 1) {
    gamma_fmt <- formatC(gammas, digits = digits, format = "f")
    cat(paste0(group, ": gamma = "), gamma_fmt, "\n")
  } else {
    starts <- c(-Inf, y_breaks)
    ends   <- c(y_breaks, Inf)
    
    starts_fmt <- ifelse(is.infinite(starts), "-Inf", formatC(starts, digits = digits, format = "f"))
    ends_fmt   <- ifelse(is.infinite(ends),   "Inf",  formatC(ends,   digits = digits, format = "f"))
    gammas_fmt <- formatC(gammas, digits = digits, format = "f")
    
    intervals <- sprintf("y in [%s, %s):", starts_fmt, ends_fmt)
    max_width <- max(nchar(intervals))
    
    cat(paste0(group, ":\n"))
    for (i in seq_along(gammas)) {
      pad <- sprintf("%-*s", max_width, intervals[i])
      cat(sprintf(" %s gamma = %s\n", pad, gammas_fmt[i]))
    }
  }
}
