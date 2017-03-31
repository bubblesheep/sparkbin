context("Automatic Modeling")
require(broom)
# basic char tests -----------
test_that("results of autmotic_modeling?", {
  set.seed(10)
  g <- sparkbin_auto_model(sc, df1, 'z', minIV = 0.05)
  expect_equal(g$auc,0.8171019, 0, tolerance=1e-6)
  
  set.seed(10)
  g <- sparkbin_auto_model(sc, df1, 'z', minIV = 0.1)
  expect_equal(g$auc,0.8188419, 0, tolerance=1e-6)
  
  # NOTE: This is a bug
  g <- sparkbin_auto_model(sc, df1, 'z', minIV = 0.0001)
  expect_equal(g$auc,0.8188419, 0, tolerance=1e-6)
  
  # expect a NULL return when all bins have IV = 0 
  set.seed(10)
  g <- sparkbin_auto_model(sc, df1 %>% select(b,d,z), 'z', minIV = 0.1)
  expect_equal(g, NULL)
})