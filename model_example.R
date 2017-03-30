# Load packages----------------------------------------------------------------
library(sparklyr)
library(dplyr)
library(sparkbin)

# Create Spark connection and load data----------------------------------------
sc <- spark_connect("local", config = spark_config())
lending_tbl <- spark_read_csv(sc, "lending", "sampledata/loan.csv")

# Only select a subset of features for this excersise
sdf <- lending_tbl %>%
  mutate(target = ifelse(
    loan_status %in% c("Current", "Issued", "Fully Paid"), 0, 1)) %>%
  select(annual_inc, delinq_2yrs, dti, emp_length,
         home_ownership, installment, purpose, sub_grade, total_rec_late_fee,
         term, target) %>%
  sdf_register("sdf")

tbl_cache(sc, "sdf")

# specify numeric features and character features
features <- sapply(sdf_schema(sdf %>% select(-target)), function(x) x$type)
num_features <- names(features)[features %in% c("IntergerType", "DoubleType")]
char_features <- names(features)[features == "StringType"]

# Train/Test split-------------------------------------------------------------
partitions <- sdf_partition(sdf, training = 0.6, test = 0.4)
train <- partitions$train
test <- partitions$test

# Binning----------------------------------------------------------------------
binobjs <- list()
for (feature in num_features) {
  binobjs[[feature]] <- bin_tree(bin_init_num(train, feature, "target"))
}
for (feature in char_features) {
  binobjs[[feature]] <- bin_tree(bin_init_char(train, feature, "target"))
}

# check check
plot(binobjs[["term"]])
plot(binobjs[["home_ownership"]])

# Feature selection-----------#
# Only select features with information value greater than 0.01
binobjs <- binobjs[sapply(binobjs, IV) > 0.01]

# Transformation and modeling--------------------------------------------------
train_transformed <- bin_transform_batch(train, binobjs)
formula <- paste0("target~", paste0("ft_", names(binobjs), collapse = "+"))
formula <- as.formula(formula)
fit <- ml_logistic_regression(train_transformed %>%
                                select(target, contains("ft_")),
                              formula)

# Evaluate in test data--------------------------------------------------------
pred <- sdf_predict(fit, test %>% bin_transform_batch(binobjs))

ml_binary_classification_eval(pred, "target", "probability")
