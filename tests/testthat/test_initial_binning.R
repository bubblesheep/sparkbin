context("Initial Binning")
# create df for testing -------
set.seed(1)
n <- 1000
# generate some data
df1 <- data.frame(a = runif(n),
                  b = sample(letters, n, replace=T),
                  c = rpois(n, 2),
                  d = sample(c('A', 'B', 'C'), n, replace = T, p = c(.005, .9, .095)),
                  e = 2,
                  f = 'F',
                  z = ifelse(runif(n) > .2, 1, 0))
# generate some missing values
for(i in 1:(ncol(df1)-1)){
  m <- round( runif(1, min=0, max=n/10) )
  index <- sample(1:n, m)
  df1[index,i] <- NA
}

# load to test sc
df1_tbl <- testthat_tbl('df1')

# basic char tests -----------
test_that("returning nomialbin?", {
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

# not working yet...
# test_that("returning intervalbin?", {
#   expect_is(bin_init_num(df1_tbl, 'c', 'z', 5), 'intervalbin')
# })