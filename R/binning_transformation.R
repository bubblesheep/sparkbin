#-----------------------------------------------------------------------------#
# BINNING TRANSFORMATION USING OPTIMAL BINNING DATA
#-----------------------------------------------------------------------------#

# Transformation Function For Numeric Binning Class----------------------------

#' Optimal binning transformation for numeric features
#'
#' @param sdf spark data frame
#' @param binobj numeric binning object with class "intervalbin"
#' @param input.col input feature name
#' @param output.col output feature name
#' @return a new spark data frame with transformed features
#'
#' @export
bin_transform_num <- function(sdf, binobj, input.col,
                              output.col = paste0("ft_", input.col)) {
  newsdf <- sdf %>%
    ft_bucketizer(input.col, "tempxyzabc", splits = binobj$cuts) %>%
    mutate(tempxyzabc = as.character(as.integer(tempxyzabc)),
           tempxyzabc = ifelse(is.na(tempxyzabc), "Missing", tempxyzabc)) %>%
    rename_(.dots = setNames(list("tempxyzabc"), output.col))
  newsdf
}

# Transformation Function For Character Binning Class--------------------------

#' Optimal binning transformation for character features
#'
#' @param sdf spark data frame
#' @param binobj character binning object with class "nominalbin"
#' @param input.col input feature name
#' @param output.col output feature name
#' @return a new spark data frame with transformed features
#'
#' @export
bin_transform_char <- function(sdf, binobj, input.col,
                               output.col = paste0("ft_", input.col)) {
  mapd <- data.frame(original_level = binobj$xlevels,
                     dest_level = binobj$ylevels,
                     stringsAsFactors = FALSE)
  temp_tbl <- paste0(input.col, "temp_map")
  temp_map <- copy_to(sc, mapd, temp_tbl, overwrite = TRUE)

  newsdf <- sdf %>%
    mutate_(tempxyabc = sprintf("ifelse(is.na(%s), '_Missing_', %s)",
                                input.col, input.col)) %>%
    mutate(tempxyabc = ifelse(!(tempxyabc %in% binobj$xlevels), "_Other_", tempxyabc)) %>%
    left_join(temp_map, by = c("tempxyabc" = "original_level")) %>%
    rename_(.dots = setNames(list("dest_level"), output.col)) %>%
    select(-tempxyabc)
  newsdf
}

#' Transformation Function For Binning Object
#'
#' @param sdf spark data frame
#' @param binobj character binning object with class "nominalbin"
#' @param input.col input feature name
#' @param output.col output feature name
#'
#' @export
#'
bin_transform <- function(sdf, binobj, input.col,
                          output.col = paste0("ft_", input.col)) {
  if (class(binobj) == "nominalbin") {
    return(bin_transform_char(sdf, binobj, input.col, output.col))
  }
  if (class(binobj) == "intervalbin") {
    return(bin_transform_num(sdf, binobj, input.col, output.col))
  }
}

#' Transform Function using a list of Binning Object
#'
#' @param sdf spark data frame
#' @param binobjs binning objects
#'
#' @export
#'
bin_transform_batch <- function(sdf, binobjs) {
  newsdf <- sdf
  for (name in names(binobjs)) {
    newsdf <- newsdf %>% bin_transform(binobjs[[name]], name)
  }
  newsdf
}


