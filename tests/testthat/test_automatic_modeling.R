# context("Automatic Modeling")
# 
# sc <- testthat_spark_connection()
# 
# # basic char tests -----------
# test_that("results of autmotic_modeling?", {
#   set.seed(10)
#   g <- sparkbin_auto_model(sc, df1, 'z', minIV = 0.1)
#   expect_equal(g$auc,0.941, 0, tolerance=1e-3)
# 
#   set.seed(10)
#   g <- sparkbin_auto_model(sc, df1, 'z', minIV = 0.05)
#   expect_equal(g$auc,0.941, 0, tolerance=1e-3)
# 
#   # NOTE: This is a bug
#   # g <- sparkbin_auto_model(sc, df1, 'z', minIV = 0.0001)
#   # expect_equal(g$auc,0.8188419, 0, tolerance=1e-6)
# 
#   # expect a NULL return when all bins have IV = 0
#   set.seed(10)
#   g <- sparkbin_auto_model(sc, df1 %>% select(b,d,z), 'z', minIV = 0.1)
#   expect_equal(g, NULL)
# })
