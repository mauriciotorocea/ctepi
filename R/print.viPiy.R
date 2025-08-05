print.viPiy <- function (x, digits = max(3L, getOption("digits") - 3L), ...) 
{
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  cat("Verity intervals for the ATE:\n")
  print.default(format(x$ATE, digits = digits), print.gap = 2L, 
                quote = FALSE)
  cat("\n")
  invisible(x)
}

