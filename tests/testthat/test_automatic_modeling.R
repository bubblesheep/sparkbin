context("Automatic Modeling")
require(broom)
# basic char tests -----------
test_that("results of autmotic_modeling?", {
  set.seed(10)
  g <- sparkbin_auto_model(sc, df1, 'z', minIV = 0.05)
  expect_equal(g$auc,.8171019, 0, tolerance=1e-6)
  
  set.seed(10)
  g <- sparkbin_auto_model(sc, df1, 'z', minIV = 0.1)
  expect_equal(g$auc,.8205762, 0, tolerance=1e-6)
  
  # NOTE: THIS IS A BUG
  # g <- sparkbin_auto_model(sc, df1, 'z', minIV = 0.001)
})