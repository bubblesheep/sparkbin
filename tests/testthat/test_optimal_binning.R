context("Optimal Binning")

# basic char tests -----------
test_that("bin_tree returning nomialbin?", {
  expect_is(bin_tree(create_df1_nominalbin('b', .01)), 'nominalbin')
  expect_is(bin_tree(create_df1_nominalbin('b', .05)), 'nominalbin')
  expect_is(bin_tree(create_df1_nominalbin('b', .1)), 'nominalbin')

  expect_is(bin_tree(create_df1_nominalbin('d', .01)), 'nominalbin')
  expect_is(bin_tree(create_df1_nominalbin('d', .05)), 'nominalbin')
  expect_is(bin_tree(create_df1_nominalbin('d', .1)), 'nominalbin')

  expect_is(bin_tree(create_df1_nominalbin('f', .01)), 'nominalbin')
  expect_is(bin_tree(create_df1_nominalbin('f', .05)), 'nominalbin')
  expect_is(bin_tree(create_df1_nominalbin('f', .1)), 'nominalbin')
})

# basic interval test ---------
# TODO: warnings are getting generated
#       i think it's from optimal_binning.R#46
#       cuts <- sort(cuts)
test_that("bin_tree returning intervalbin?", {
  expect_is(bin_tree(create_df1_intervalbin('a', 5)), 'intervalbin')
  expect_is(bin_tree(create_df1_intervalbin('a', 10)), 'intervalbin')
  expect_is(bin_tree(create_df1_intervalbin('a', 100)), 'intervalbin')

  expect_is(bin_tree(create_df1_intervalbin('c', 5)), 'intervalbin')
  expect_is(bin_tree(create_df1_intervalbin('c', 10)), 'intervalbin')
  expect_is(bin_tree(create_df1_intervalbin('c', 100)), 'intervalbin')

  expect_is(bin_tree(create_df1_intervalbin('e', 5)), 'intervalbin')
  expect_is(bin_tree(create_df1_intervalbin('e', 10)), 'intervalbin')
  expect_is(bin_tree(create_df1_intervalbin('e', 100)), 'intervalbin')
})




