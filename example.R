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
  select(id, annual_inc, delinq_2yrs, dti, emp_length,
         grade, home_ownership, installment,  purpose, sub_grade,
         term, target) %>%
  sdf_register("sdf")

tbl_cache(sc, "sdf")


# initial_binning examples -------------------------------

# get initial cuts for the numeric variable (dti)
df <- bin_init_num(sdf, 'dti', 'target', 100)
df

# get initial cuts for integer variable 
# sdf <- sdf %>% mutate(dti_integer = row_number()) # create an integer variable to test
df <- bin_init_num(sdf, 'id', 'target', 100)
df

# get initial cuts for column with some missing values
df <- bin_init_num(sdf, 'annual_inc', 'target', 100)
df
