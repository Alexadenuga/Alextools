test_that("Testing for class of the plot if correct", {
  x <- seq(1.5, 10.5, 0.05 )
  the_plot <- the_normal_curve(x=x, mean = 6, sd = 2,
                   col = "blue", VerticalSplit = 7,
                   shadeFrom = 7, shadeTo = 10.5,
                   plot_type = "ggplot")

  expect_equal(class(the_plot)[2], "ggplot")

})

