#-----------------------------------------------------------------------------#
# EXAMPLES OF USING FUNCTIONS AND FOR FUNCTION DEVELOPMENT AND TESTING
#-----------------------------------------------------------------------------#

# Data source: lending club data
# https://www.kaggle.com/wendykan/lending-club-loan-data

library(sparklyr)
library(dplyr)

# Create Spark connection and load data----------------------------------------
sc <- spark_connect("local", config = spark_config())

lending_tbl <- spark_read_csv(sc, "lending", "sampledata/loan.csv")

# Only select a subset of features for this excersise
sdf <- lending_tbl %>%
  mutate(target = ifelse(
    loan_status %in% c("Current", "Issued", "Fully Paid"), 0, 1)) %>%
  select(annual_inc, delinq_2yrs, dti, emp_length,
         grade, home_ownership, installment,  purpose, sub_grade,
         term, target) %>%
  sdf_register("sdf")

tbl_cache(sc, "sdf")

# Build a numeric initial binning using R
d <- sdf %>% select(annual_inc, target) %>% collect()

cuts <- c(-Inf, unique(quantile(d$annual_inc, (1:99)/100, na.rm = T)), Inf)

d <- d %>%
  mutate(q = cut(annual_inc, cuts))

cnts <- d%>% group_by(q) %>%
  summarise(good = sum(target == 0),
            bad = sum(target == 1))

good <- cnts$good[!is.na(cnts$q)]

bad <- cnts$bad[!is.na(cnts$q)]

Missing <- c(good = cnts[is.na(cnts$q), ]$good,
             bad = cnts[is.na(cnts$q), ]$bad)
obj <- list(cuts = cuts,
            good = good,
            bad = bad,
            Missing = Missing)

class(obj) <- "intervalbin"

obj <- bin_tree(obj)

# Build a character initial binning using R

dd <- sdf %>%
  select(purpose = grade, target) %>%
  collect()

dd <- dd %>%
  group_by(purpose) %>%
  summarise(good = sum(target == 0),
            bad = sum(target == 1)) %>%
  ungroup() %>%
  mutate(pct = (good + bad) / sum(good + bad)) %>%
  mutate(p = ifelse(is.na(purpose) | purpose == "", "_Missing_",
                    ifelse(pct < 0.01, "_Other_", purpose))) %>%
  group_by(p) %>%
  summarise(good = sum(good),
            bad = sum(bad))

obj <- list(
  xlevels = dd$p,
  ylevels = dd$p,
  good = dd$good,
  bad = dd$bad,
  minp = 0.01
)

class(obj) <- "nominalbin"

