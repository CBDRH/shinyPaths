test_that("multiplication works", {
  expect_equal(is.ggplot(drawPath(dag = ggdag::confounder_triangle(x_y_associated = TRUE), 
                        adj = c('z'), 
                        path = "x <- z -> y")), TRUE)
})
