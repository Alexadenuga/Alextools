#' @title the_spark_plot Just Another Spark-line plot function
#'
#' @param dat the data that has to be a dataframe
#' @param trans level of transparency of the line
#' @param col colour of the plot
#' @param label_x label name for the x-axis
#' @param label_y label name for the y-axis
#' @param the_title title of the plot
#'
#' @return No returns, just creates a plot
#'
#' @export
#'
#'
#'
#' @details: takes a dataframe and turns it into a sparkline plot where only the first line is not transparent to a specified opacity level
#'
#' @examples dat <- data.frame(matrix(runif(50*3, 0, 1 ), 50))
#'           the_spark_plot(dat=dat, trans = 0.3, col = "red", label_x = "Data point", label_y = "Value", the_title = "Sparkline Plot")
#'
the_spark_plot <- function(dat, trans, col, label_x, label_y, the_title, ...) {

  # Check if 'dat' is a dataframe
  if (!is.data.frame(dat)) {
    stop("Input 'dat' must be a dataframe.")
  }

  plot(1, type = "n", xlim = c(1, nrow(dat)), ylim = c(0, max(dat)), xlab = label_x, ylab = label_y, xaxt = "n")

  # Plot the first column (X1) as a normal line
  lines(1:nrow(dat), dat[, ncol(dat)], col = col, ...)

  # Plot the remaining columns as transparent lines
  for (i in 1:ncol(dat)) {
    lines(1:nrow(dat), dat[, i], col = rgb(0, 0, 0, alpha = trans), ...)
  }

  # Add a title
  title(the_title)

  # Add axis labels
  axis(1, at = 1:nrow(dat), labels = 1:nrow(dat))
}
