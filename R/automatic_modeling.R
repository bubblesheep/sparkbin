#-----------------------------------------------------------------------------#
# AUTOMATIC MODELING USING SPARKBIN PACKAGE
#-----------------------------------------------------------------------------#
#' Automatic modeling using sparkbin package
#'
#' @param sc spark connection
#' @param sdf spark data frame or data frame in R
#' @param y the target variable with values 0/1
#' @param minIV the minium information value for a variable to keep
#' @param ... additional arguments for future
#'
#' @export
#'
sparkbin_auto_model <- function(sc, sdf, y, minIV = 0.03, ...) {
  # Check if sdf is data frame or spark data frame
  require(sparklyr)
  if (inherits(sdf, "data.frame")) {
    sdf <- copy_to(sc, sdf, "_mytempsdftable_", overwrite = TRUE)
  }
  # Check numeric and character features
  features <- sapply(sdf_schema(sdf), function(x) x$type)
  features <- features[names(features) != y]
  num_features <- names(features)[features %in% c("IntegerType", "DoubleType")]
  char_features <- names(features)[features == "StringType"]
  # Partition data
  partitions <- sdf_partition(sdf, training = 0.6, test = 0.4)
  train <- partitions$train
  test <- partitions$test
  # Binning
  binobjs <- list()
  for (feature in num_features) {
    binobjs[[feature]] <- bin_tree(bin_init_num(train, feature, y))
  }
  for (feature in char_features) {
    binobjs[[feature]] <- bin_tree(bin_init_char(train, feature, y))
  }
  binobjs <- binobjs[sapply(binobjs, IV) > minIV]
  # Modeling
  train_transformed <- bin_transform_batch(train, binobjs)
  formula <- sprintf("%s~%s", y, paste0("ft_", names(binobjs), collapse = "+"))
  formula <- as.formula(formula)
  fit <- ml_logistic_regression(train_transformed %>%
                                  select(one_of(y), contains("ft_")),
                                formula)
  # Evaluation
  pred <- sdf_predict(fit, test %>% bin_transform_batch(binobjs))
  auc <- ml_binary_classification_eval(pred, y, "probability")
  # Output
  list(binobjs = binobjs,
       auc = auc,
       fit = fit)
}
