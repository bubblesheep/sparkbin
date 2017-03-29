#-----------------------------------------------------------------------------#
# CLASS METHODS FOR BINNING CLASS
#-----------------------------------------------------------------------------#
print_table <- function(d) {
  require(dplyr)
  require(stringr)
  require(scales)
  f1 <- function(x) ifelse(is.na(x), "", comma(x))
  f2 <- function(x) ifelse(is.na(x), "", format(x, digits = 1, nsmall = 1))
  f3 <- function(x) ifelse(is.na(x), "", round(x, 3))
  f4 <- function(x) ifelse(is.na(x), "", format(x, digits = 3, nsmall = 3))
  d <- d %>%
    mutate_each(funs(f1), good, bad) %>%
    mutate_each(funs(f2), pgood, pbad) %>%
    mutate_each(funs(f3), WoE, IV) %>%
    mutate_each(funs(f4), WoE, IV)
  names(d) <- c("", "Good", "Bad", "%Good", "%Bad", "WoE", "IV")
  cl <- sapply(d, function(x) max(nchar(x)))
  cl <- pmax(cl, nchar(names(d)))
  cl[-1] <- cl[-1] + 2
  names(d) <- str_pad(names(d), cl)
  for (i in seq(cl)) d[[i]] <- str_pad(d[[i]], cl[i])
  n <- sum(sapply(d, function(x) max(nchar(x))))
  cat(rep("-", n), "\n", sep = "")
  cat(names(d), "\n", sep = "")
  cat(rep("-", n), "\n", sep = "")
  for (i in seq(nrow(d) - 1)) {
    cat(unlist(d[i, ]), "\n", sep = "")
  }
  cat(rep("-", n), "\n", sep = "")
  cat(unlist(d[i + 1, ]), "\n", sep = "")
  cat(rep("-", n), "\n", sep = "")
}

# Summary table----------------------------------------------------------------
#' Generate binning table data
#'
#' @param x binning object
#'
#' @export
#'
bin_table <- function(x) {
  UseMethod("bin_table")
}


#' Generate binning table data for intervalbin class
#'
#' @param x binning object
#'
#' @export
#'
bin_table.intervalbin <- function(x) {
  group <- sprintf("[%s,%s)", x$cuts[-length(x$cuts)], x$cuts[-1])
  d <- data.frame(name = group, good = x$good, bad = x$bad,
                  stringsAsFactors = FALSE)
  if (length(x$Missing) > 0) {
    dm <- data.frame(name = "Missing",
                     good = x$Missing['good'],
                     bad = x$Missing['bad'],
                     stringsAsFactors = FALSE)
    d <- rbind(d, dm)
  }
  d <- d %>%
    mutate(pgood = good / sum(good) * 100,
           pbad = bad / sum(bad) * 100,
           WoE = log(pgood / pbad),
           IV = (pgood - pbad) * WoE / 100) %>%
    mutate(WoE = ifelse(!is.finite(WoE), 0, WoE),
           IV = ifelse(!is.finite(IV), 0, IV))
}

#' Generate binning table data for nominalbin class
#'
#' @param x binning object
#'
#' @export
#'
bin_table.nominalbin <- function(x) {
  require(dplyr)
  d <- with(x, {
    data.frame(name = ylevels,
               good = good,
               bad = bad,
               stringsAsFactors = FALSE)
  })
  d <- d %>% group_by(name) %>%
    summarise_each(funs(sum), good, bad)
  d <- d %>%
    mutate(pgood = good / sum(good) * 100,
           pbad = bad / sum(bad) * 100,
           WoE = log(pgood / pbad),
           IV = (pgood - pbad) * WoE / 100) %>%
    mutate(WoE = ifelse(!is.finite(WoE), 0, WoE),
           IV = ifelse(!is.finite(IV), 0, IV))
  d
}

# Print Methods----------------------------------------------------------------
#' Function to print intervalbin object
#'
#' @param x the intervalbin object
#'
#' @export
#'
print.intervalbin <- function(x) {
  d <- bin_table(x)
  d2 <- d %>%
    summarise(good = sum(good),
              bad = sum(bad)) %>%
    mutate(name = "Total") %>%
    mutate(pgood = 100,
           pbad = 100,
           WoE = 0,
           IV = sum(d$IV))
  d <- rbind(d, d2)
  print_table(d)
}

#' Function to print nominalbin object
#'
#' @param x the nominal object
#'
#' @export
#'
print.nominalbin <- function(x) {
  d <- bin_table.nominalbin(x)
  d2 <- d %>%
    summarise(good = sum(good),
              bad = sum(bad)) %>%
    mutate(name = "Total") %>%
    mutate(pgood = 100,
           pbad = 100,
           WoE = 0,
           IV = sum(d$IV))
  d <- rbind(d, d2)
  print_table(d)
}

# Plot Methods-----------------------------------------------------------------
#' Plot intervalbin class
#'
#' @param x intervabin class object
#'
#' @export
#'
plot.intervalbin <- function(x) {
  require(ggplot2)
  require(dplyr)
  d <- bin_table(x)
  d <- d %>%
    mutate(name = factor(name, levels = name))
  d <- d %>%
    arrange(name) %>%
    rename(woe = WoE)
  mx <- max(d$good + d$bad)
  d_woe <- d %>% select(name, woe) %>%
    mutate(woe = mx * (woe - min(woe)) / (max(woe) - min(woe)),
           group = 1)
  d_count_good <- d %>% select(name, count = good) %>% mutate(group = "Good")
  d_count_bad <- d %>% select(name, count = bad) %>% mutate(group = "Bad")
  d_count <- rbind(d_count_good, d_count_bad)
  ggplot() +
    geom_line(aes(name, woe), data = d_woe, group = 1) +
    geom_bar(aes(name, count, group = group, fill = group),
             data = d_count, stat = "identity",
             position = "stack", width = .75) +
    geom_line(aes(name, woe), data = d_woe, group = 1,
              color = "maroon", size = 1) +
    xlab("") + ylab("Count") +
    scale_y_continuous(labels = scales::comma) +
    theme_bw() +
    theme(legend.position = "top",
          legend.title = element_blank(),
          legend.key = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
}

#' Plot nominalbin class
#'
#' @param x nominalbin class object
#'
#' @export
#'
plot.nominalbin <- function(x) {
  require(ggplot2)
  require(dplyr)
  d <- bin_table(x)
  d <- d %>%
    arrange(WoE) %>%
    mutate(name = factor(name, levels = name))
  d <- d %>%
    arrange(name) %>%
    rename(woe = WoE)
  mx <- max(d$good + d$bad)
  d_woe <- d %>% select(name, woe) %>%
    mutate(woe = mx * (woe - min(woe)) / (max(woe) - min(woe)),
           group = 1)
  d_count_good <- d %>% select(name, count = good) %>% mutate(group = "Good")
  d_count_bad <- d %>% select(name, count = bad) %>% mutate(group = "Bad")
  d_count <- rbind(d_count_good, d_count_bad)
  ggplot() +
    geom_line(aes(name, woe), data = d_woe, group = 1) +
    geom_bar(aes(name, count, group = group, fill = group),
             data = d_count, stat = "identity",
             position = "stack", width = .75) +
    geom_line(aes(name, woe), data = d_woe, group = 1,
              color = "maroon", size = 1) +
    xlab("") + ylab("Count") +
    scale_y_continuous(labels = scales::comma) +
    theme_bw() +
    theme(legend.position = "top",
          legend.title = element_blank(),
          legend.key = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
}
