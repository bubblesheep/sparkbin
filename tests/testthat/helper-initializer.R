require(sparklyr)

if(sum(spark_installed_versions()$spark=='2.0.0') < 1){
  sparklyr::spark_install(version='2.0.0')
}

testthat_spark_connection <- function(version = NULL) {

  # generate connection if none yet exists
  connected <- FALSE
  if (exists(".testthat_spark_connection", envir = .GlobalEnv)) {
    sc <- get(".testthat_spark_connection", envir = .GlobalEnv)
    connected <- sparklyr::connection_is_open(sc)
  }

  if (!connected) {
    version <- Sys.getenv("SPARK_VERSION", unset = "2.0.0")
    setwd(tempdir())
    sc <- sparklyr::spark_connect(master = "local", config = sparklyr::spark_config())
    assign(".testthat_spark_connection", sc, envir = .GlobalEnv)
  }


  # retrieve spark connection
  get(".testthat_spark_connection", envir = .GlobalEnv)
}

testthat_tbl <- function(name) {
  sc <- testthat_spark_connection()
  tbl <- tryCatch(dplyr::tbl(sc, name), error = identity)
  if (inherits(tbl, "error")) {
    data <- eval(as.name(name), envir = parent.frame())
    tbl <- dplyr::copy_to(sc, data, name = name)
  }
  tbl
}

skip_unless_verbose <- function(message = NULL) {
  message <- message %||% "Verbose test skipped"
  verbose <- Sys.getenv("SPARKLYR_TESTS_VERBOSE", unset = NA)
  if (is.na(verbose)) skip(message)
  invisible(TRUE)
}

test_requires <- function(...) {

  for (pkg in list(...)) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      fmt <- "test requires '%s' but '%s' is not installed"
      skip(sprintf(fmt, pkg, pkg))
    }
  }

  invisible(TRUE)
}

# create df for testing =========================
generate_testing_df <- function(n=1000){
  set.seed(1)
  # TODO: make colnames more interpretable
  # TODO: should try to create fields that posed challenges
  # NOTE: all fields have NA, see below
  # NOTE: currently these tests are using small data
  df1 <- data.frame(a = runif(n),
                    b = sample(letters, n, replace=T),
                    c = rpois(n, 2),
                    d = sample(c('A', 'B', 'C', '   ', ' ', ''), n, replace = T, p = c(.005, .4, .095, .1, .2, .3)),
                    e = 2,
                    f = 'TEXT')
df1$z = ifelse(df1$a^2 + df1$c/max(df1$c, na.rm=T) + rnorm(n, 0,.1)  > .6, 1, 0)
df1$z[is.na(df1$z)] <- 0
  # a: numeric
  # b: char
  # c: integer
  # d: whitespace entries, categories with small proportion
  # e: single value numeric
  # f: single value char
  # z: binary target variable

  # convert factor to character for testing purposes
  cols <- sapply(df1, class)
  for(i in 1:length(cols)){
    if(cols[i]=='factor'){
      df1[,i] <- as.character(df1[,i])
    }
  }

  # generate some missing values
  for(i in 1:(ncol(df1)-1)){
    m <- round( runif(1, min=0, max=n/10) )
    index <- sample(1:n, m)
    df1[index,i] <- NA
  }

  return(df1)
}

# load to test sc ================================
df1 <- generate_testing_df()
df1_tbl <- testthat_tbl('df1')
# print(df1_tbl)

# helpers for test_optimal_binning ===============

# create nomimal bins in local R memory
create_df1_nominalbin <- function(colname, minp){

  # colname = 'd'
  # minp = .01
  index <- is.na(df1[,colname]) | str_trim(df1[,colname])==''
  df1[index,colname] <- NA

  ifelse_expr <- sprintf("%s = ifelse(is.na(%s), '_Missing_', ifelse(pct < %s, '_Other_', %s))",
                         colname, colname, minp, colname)
  obj_df <- df1 %>%
    group_by_(colname) %>%
    summarise(good = sum(z == 0),
              bad = sum(z == 1)) %>%
    ungroup() %>%
    mutate(pct = (good + bad) / sum(good + bad)) %>%
    mutate_(.dots = setNames(list(ifelse_expr), colname)) %>%
    group_by_(colname) %>%
    summarise(good = sum(good),
              bad = sum(bad))

  obj <- list(
    xlevels = unlist(obj_df[,colname], use.names = F),
    ylevels = unlist(obj_df[,colname], use.names = F),
    good = obj_df$good,
    bad = obj_df$bad,
    minp = minp
  )

  class(obj) <- "nominalbin"
  return(obj)
}

create_df1_intervalbin <- function(colname, bins){
  # colname <- 'c'
  # bins <- 100

  # rename for ease
  d <- df1 %>% rename_('colname'=colname) %>% select(colname, z)

  # count missing
  Missing <- c(
    good = sum(df1$z[is.na(d$colname)]==0),
    bad = sum(df1$z[is.na(d$colname)]==1)
  )

  # n-tile for non-missing counts
  d <- d %>% filter(!is.na(colname))
  cuts <- c(-Inf, unique(quantile(d$colname, 1:(bins-1)/bins, na.rm = T)), Inf)
  d$q <- cut(d$colname, cuts)

  cnts <- d %>% group_by(q) %>%
    summarise(good = sum(z == 0),
              bad = sum(z == 1))

  obj <- list(cuts = cuts,
              good = unlist(cnts$good),
              bad = unlist(cnts$bad),
              Missing = Missing)

  class(obj) <- "intervalbin"
  return(obj)
}
