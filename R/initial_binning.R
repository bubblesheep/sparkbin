#-----------------------------------------------------------------------------#
# INITIAL BINNING
#-----------------------------------------------------------------------------#

# Numeric binning--------------------------------------------------------------

#' Initial binning for numeric variables
#' @param sdf Spark data frame
#' @param x the name of the numeric variable to bin
#' @param y the name of the target variable, which should take only
#'  values 0 and 1, no missing should be allowed
#' @param init_bins number of initial bins
#' @return a numeric binning object with class name "intervalbin" with the
#' following fields:
#' cuts - the break points (left end should be -Inf, right end should be Inf)
#' good - number of good (0) in each interval
#' bad - number of bad (1) in each interval
#' missing - a list with two components for missing values: good count and
#' bad count
#'
#' EXAMPLE returned object
#' obj <- list(cuts = c(-Inf, 1.5, 10, Inf),
#'             good = c(200, 300, 500),
#'             bad = c(100, 300, 900),
#'             _Missing_ = c(good = 50, bad = 50))
#' @export
#'
bin_init_num <- function(sdf, x, y, init_bins = 100) {

  # rename columns for dplyr *_ dynamic references
  sdf <- sdf %>% rename_('var'=x, 'target'=y) %>% select(var, target)

  # get missing counts
  # TODO: make this prettier
  # expr <- paste0('is.na(var) & target==1')
  missing_count <- sdf %>% filter(is.na(var) & target==0) %>% summarise(n=n()) %>% collect()
  missing_good <- missing_count$n

  # expr <- paste0('is.na(', x, ') & target==0')
  missing_count <- sdf %>% filter(is.na(var) & target==1) %>% summarise(n=n()) %>% collect()
  missing_bad <- missing_count$n

  # calcualte n-tiles
  sdf <- sdf %>% mutate(var = as.numeric(var)) %>% filter(!is.na(var))

  sdf_q <- ft_quantile_discretizer(sdf, 'var', 'var_cut', init_bins)

  # get counts
  sdf_cut_counts <- sdf_q %>%
    group_by(var_cut) %>%
    summarise(cuts_lower = min(var),
              cuts_higher = max(var),
              n = n(),
              bad = sum(target),
              good = n() - sum(target)) %>%
    ungroup() %>%
    arrange(var_cut)
  # pull results to local
  sdf_cuts_local <- collect(sdf_cut_counts)

  # format final object
  obj <- list(cuts = c(-Inf, sdf_cuts_local$cuts_lower[-1], Inf),
              good = sdf_cuts_local$good,
              # n = sdf_cuts_local$n,
              bad = sdf_cuts_local$bad,
              Missing = c('good' = missing_good, 'bad' = missing_bad))

  class(obj) <- "intervalbin"
  return(obj)
}

# Character binning------------------------------------------------------------

#' Initial binning for character variables
#' @param sdf Spark data frame
#' @param x the character variable to bin
#' @param y the target variable, which should take only values 0 and 1,
#' no missing should be allowed
#' @param minp the minimum frequency of a level, if the frequency is less than
#' minp then the corresponding level will be grouped to '_Other_'
#' @return a character binning object with class name "nominalbin" with the
#' following fields:
#' xlevels - including major levels (> minp) and two special levels
#' "_Missing_" and "_Other_"
#' ylevels - mapped new levels, in the initial binning it should be same as
#' xlevels
#' good - number of good in each xlevel
#' bad - number of bad in each xlevel
#'
#' EXAMPLE of returned object
#' obj <- list(xlevels = c("A", "B", "_Missing_", "_Other_"),
#'             ylevels = c("A", "B", "_Missing_", "_Other_"),
#'             good = c(200, 200, 200, 200),
#'             bad = c(100, 200, 300, 400),
#'             minp = 0.01)
#' @export
#'
bin_init_char <- function(sdf, x, y, minp = 0.01) {

  # rename columns for dplyr *_ dynamic references
  sdf <- sdf %>% rename_('var'=x, 'target'=y) %>% select(var, target)

  # missing counts first
  missing_df <- sdf %>%
    filter(is.na(var) | trim(var)=='') %>%
    summarise(n=n(), bad = sum(target), good = n() - sum(target)) %>%
    collect()

  missing_df[is.na(missing_df)] <- 0

  # non-missing counts
  counts <- sdf %>%
    filter(!is.na(var) & !trim(var)=='') %>%
    group_by(var) %>%
    summarise(count = n(),
              bad = sum(target),
              good = n() - sum(target)) %>%
    collect()

  counts <- mutate(counts,  prop = count/sum(count))

  # combine small (n < minp) groups into other
  obj_df <- counts %>%
    mutate(var_2 = ifelse(counts$prop > minp, counts$var, "_Other_")) %>%
    group_by(var_2) %>%
    summarise(count = sum(count),
              bad = sum(bad),
              good = sum(good)) %>%
    rbind(c('_Missing_', missing_df$n, missing_df$bad, missing_df$good)) %>%
    filter(count > 0) # remove categories with no count

  # format result
  obj <- list(xlevels = obj_df$var_2,
              ylevels = obj_df$var_2,
              good = as.numeric(obj_df$good),
              bad = as.numeric(obj_df$bad),
              minp = minp)

  class(obj) <- "nominalbin"
  return(obj)
}


#' Initial binning for all variables
#' @param sdf Spark data frame
#' @param x the variable to bin
#' @param y the target variable, which should take only values 0 and 1,
#' no missing should be allowed
#' @param init_bins number of initial bins for numeric variables
#' for character variables, init_bins is equal to 1/minp
#' where minp is the minimum frequency of a level, if the frequency is less than
#' minp then the corresponding level will be grouped to '_Other_'
#' @return for numeric variable x, it returns a numeric binning object with class name "intervalbin" with the
#' following fields:
#' cuts - the break points (left end should be -Inf, right end should be Inf)
#' good - number of good (0) in each interval
#' bad - number of bad (1) in each interval
#' missing - a list with two components for missing values: good count and
#' bad count
#' for character variable x, it returns a character binning object with class name "nominalbin" with the
#' following fields:
#' xlevels - including major levels (> minp) and two special levels
#' "_Missing_" and "_Other_"
#' ylevels - mapped new levels, in the initial binning it should be same as
#' xlevels
#' good - number of good in each xlevel
#' bad - number of bad in each xlevel
#'
#' EXAMPLE of returned object
#' obj <- list(xlevels = c("A", "B", "_Missing_", "_Other_"),
#'             ylevels = c("A", "B", "_Missing_", "_Other_"),
#'             good = c(200, 200, 200, 200),
#'             bad = c(100, 200, 300, 400),
#'             minp = 0.01)
#' @export
#'

bin_init <- function(sdf, x, y, init_bins = 100){
  jobj <- spark_dataframe(sdf %>% select_(x))
  type <- invoke(invoke(invoke(jobj, "schema"), "fields")[[1]], "dataType")
  if(grepl("([Ff]loat)|([Dd]ouble)|([Ii]nteger)|([Ll]ong)|([Ss]hort)", invoke(type, "toString")) == TRUE){
    bin_init_num(sdf, x, y, init_bins)
  }else{
    bin_init_char(sdf, x, y, minp = 1/init_bins)
  }
}


