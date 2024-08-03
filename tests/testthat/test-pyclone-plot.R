devtools::load_all()

test_that("pyclone summarise and estimate work", {
  data <- readRDS(test_path("fixtures", "sample_pyclone_result.rds"))
  expected_df <- data.frame(
    cluster_id = c(0, 2, 1),
    freq = c(11, 5, 96),
    countDriver = c(2, 0, 11),
    cellular_prevalence = c(0.9951, 0.4823, 0.1709),
    K = c(0.5128, 0.3114, 0.1758)
  )
  expected_df = as_tibble(expected_df)
  expect_equal(summarise_pyclone_data(data), expected_df)
})


test_that("pyclone donut plot works", {
  data <- readRDS(test_path("fixtures", "sample_pyclone_result.rds"))
  p <- make_pyclone_donut_plot(data)
  expect_true(inherits(p, "ggplot"))
  # expect_equal(p$data, data)

  # Check that the aesthetics are correctly mapped
  # expect_equal(p$mapping$x, quo(mpg))
  # expect_equal(p$mapping$y, quo(wt))
})
