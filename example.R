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



