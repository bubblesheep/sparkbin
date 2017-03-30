context("Initial Binning")

# basic char tests -----------
test_that("bin_init_char returning nomialbin?", {
  expect_is(bin_init_char(df1_tbl, 'b', 'z', .001), 'nominalbin')
  expect_is(bin_init_char(df1_tbl, 'b', 'z', .01), 'nominalbin')
  expect_is(bin_init_char(df1_tbl, 'b', 'z', .1), 'nominalbin')

  expect_is(bin_init_char(df1_tbl, 'd', 'z', .001), 'nominalbin')
  expect_is(bin_init_char(df1_tbl, 'd', 'z', .01), 'nominalbin')
  expect_is(bin_init_char(df1_tbl, 'd', 'z', .1), 'nominalbin')


  expect_is(bin_init_char(df1_tbl, 'f', 'z', .001), 'nominalbin')
  expect_is(bin_init_char(df1_tbl, 'f', 'z', .01), 'nominalbin')
  expect_is(bin_init_char(df1_tbl, 'f', 'z', .1), 'nominalbin')
})

test_that("bin_init_num returning intervalbin?", {
  expect_is(bin_init_num(df1_tbl, 'c', 'z', 5), 'intervalbin')
  expect_is(bin_init_num(df1_tbl, 'c', 'z', 10), 'intervalbin')
  expect_is(bin_init_num(df1_tbl, 'c', 'z', 100), 'intervalbin')

  expect_is(bin_init_num(df1_tbl, 'a', 'z', 5), 'intervalbin')
  expect_is(bin_init_num(df1_tbl, 'a', 'z', 10), 'intervalbin')
  expect_is(bin_init_num(df1_tbl, 'a', 'z', 100), 'intervalbin')

  expect_is(bin_init_num(df1_tbl, 'e', 'z', 5), 'intervalbin')
  expect_is(bin_init_num(df1_tbl, 'e', 'z', 10), 'intervalbin')
  expect_is(bin_init_num(df1_tbl, 'e', 'z', 100), 'intervalbin')
})