#' @title normal_curve_shade
#'
#' @param x  a sequence of the range of the x-axis
#' @param mean this is the mean defined for the normal curve
#' @param sd this is the standard deviation defined for the normal curve
#' @param col this is the color for the shaded area
#' @param VerticalSplit the point where the x-axis should split vertically
#' @param shadeFrom The point on the x-axis where the shade should start from
#' @param shadeTo The point on the x-axis where the shade should end at
#' @param plot_type specifies the type of plot if it is base of ggplot
#' @param ... the is for any other parameters not predefined but can be defined in a plot function
#'
#' @return No returns, just creates a plot
#'
#' @export
#'
#'@details: This is the function that performs a normal curve and also shade the a specified area
#'
#' @examples x <- seq(1.5, 10.5, 0.05 )
#'           the_normal_curve(x=x, mean = 6, sd = 2, col = "blue", VerticalSplit = 7, shadeFrom = 7, shadeTo = 10.5, plot_type = "ggplot")
#'@import ggplot2
#'


the_normal_curve <- function(x, mean, sd, col, VerticalSplit, shadeFrom, shadeTo, plot_type = "base", ...) {

  # Check if 'x' is a sequence
  if (!(class(x) %in% c("integer", "numeric"))) {
    stop("x has to be a sequence")
  }

  y <- dnorm(x, mean = mean, sd = sd)

  if (plot_type == "base") {
    # Base R plot
    plot(x, y, type = "l", ...)
    abline(h = 0)
    abline(v = VerticalSplit)
    xfill <- seq(from = shadeFrom, to = shadeTo, by = 0.01)
    polygon(x = c(xfill, rev(xfill)), y = c(rep(0, length(xfill)), dnorm(rev(xfill), mean = mean, sd = sd)), col = col)
  } else if (plot_type == "ggplot") {
    # ggplot
    df <- data.frame(x = x, y = y)
    ggplot(df, aes(x = x, y = y)) +
      geom_line() +
      geom_vline(xintercept = VerticalSplit) +
      geom_ribbon(aes(ymax = ifelse(x >= shadeFrom & x <= shadeTo, y, 0), ymin = 0), fill = col, alpha = 0.5) +
      theme_minimal()
  } else {
    stop("Invalid plot_type. Please choose 'base' or 'ggplot'.")
  }
}

