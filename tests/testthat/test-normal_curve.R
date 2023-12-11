
test_that("Testing for plot-type error", {
  x <- seq(1.5, 10.5, 0.05)
  # Wrap the plotting function in a function to test for an error
  expect_error(the_normal_curve(x = x, mean = 6, sd = 2,
                                col = "blue", VerticalSplit = 7,
                                shadeFrom = 7, shadeTo = 10.5,
                                plot_type = "l"))
})
