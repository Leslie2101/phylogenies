test_that("full triangle plot works", {
  df <- data.frame(x = 1:10,
                   y1 = 1:10,
                   y2 = 1:10,
                   z1 = 1:10,
                   z2 = 1:10)
  p <- make_full_triangle_plot(df, x = as.factor(x),
                               y_1= y1, y_2= y2,
                               z_1 = z1, z_2 = z2)

  expect_true(inherits(p, "ggplot"))
  expect_equal(p$data, df)
})


test_that("half triangle plot works", {
  df <- data.frame(x = 1:5,
                   y1 = 1:5,
                   y2 = 1:5,
                   z1 = seq(10, 50, length.out=5),
                   z2 = seq(50, 10, length.out=5))
  p <- make_half_triangle_plot(df, x = as.factor(x),
                               y_1= y1, y_2= y2,
                               z_1 = z1, z_2 = z2)

  expect_true(inherits(p, "ggplot"))
  expect_equal(p$data, df)
})



