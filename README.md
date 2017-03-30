## Optimal Binning For Spark Using Sparklyr

### Proposed Steps

1. A prepared data with target (0/1) and various candidate features that do not need to be pre-processed
2. **Initial Binning**: We perform initial binning into a number of fine classes by equal count in sparklyr. We can define classes for the output for better structure definition.
3. **Optimal Binning**: The data from initial binning are small enough so we can do optimal binning in local R using Tree based methods. The output of optimal binning should be in same class and same structure as initial binning
4. **Binning Transformation**: We use the optimal binning objects to transform the spark data frame using sparklyr and ready to do modeling
5. The transformed data can be fitted using logistic regression using mlib in Spark


### Binning Class Data Structure

The binning class data structure is essential in the whole process.

##### Numeric Binning (intervalbin class)
The intervalbin class contains the following fields

- **cuts**: the cutting points from -Inf to Inf, example `c(-Inf, 1.5, 10, Inf)`
- **good**: the vector of counts for good (0) of the buckets created by cuts, example `c(200, 300, 500)`
- **bad**: the vector of counts for bad (1) of the buckets created by cuts, example `c(100, 300, 900)`
- **Missing**: the good/bad counts for missing values, example `c(good = 50, bad = 50)`

An example of creating binning object for numeric is
```
obj <- list(cuts = c(-Inf, 1.5, 10, Inf),
             good = c(200, 300, 500),
             bad = c(100, 300, 900),
             Missing = c(good = 50, bad = 50))

class(obj) <- "intervalbin"
```

#### Character Binning (nominalbin class)
The nominalbin class contains the following fields

- **xlevels**: The original levels of the character vector, special handling should be taken. 1. missing values are set to level \_Missing\_. 2. The levels having frequency below **minp** are set to level \_Other\_. Example `c("A", "B", "_Missing_", "_Other_")`
- **ylevels**: The mapped new level, which should be coarser than xlevels. For the initial binnig step, xlevels and ylevels should be same. Example: `c("A", "G", "G", "G")`
- **good**: the count of good for each level in xlevels
- **bad**: the count of bad for each level in xlevels
- **minp**: the threshold to set a level to Other

An example of creating binning object for character is

```
obj <- list(xlevels = c("A", "B", "_Missing_", "_Other_"),
             ylevels = c("A", "B", "_Missing_", "_Other_"),
             good = c(200, 200, 200, 200),
             bad = c(100, 200, 300, 400),
             minp = 0.01)

class(obj) <- "nominalbin"
```

#### Quick Example

```
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
         home_ownership, installment,  purpose, sub_grade,
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
```
