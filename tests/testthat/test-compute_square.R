test_that("square function tests", {

  # Test case 1: Check if the square of 5 is 25
  result1 <- compute_square(5)
  expect_equal(result1, 25, info = "Square of 5 should be 25")

  # Test case 2: Check if the square of -3 is 9
  result2 <- compute_square(-3)
  expect_equal(result2, 9, info = "Square of -3 should be 9")

  # Test case 3: Check if the square of 0 is 0
  result3 <- compute_square(0)
  expect_equal(result3, 0, info = "Square of 0 should be 0")

  # You can add more test cases here...
})
