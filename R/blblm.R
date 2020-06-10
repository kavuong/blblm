#' @import purrr
#' @import stats
#' @import parallel
#' @import furrr
#' @import tidyverse
#' @import tibble
#' @import future
#' @import utils
#' @import tidyr
#' @importFrom magrittr %>%
#' @importFrom "utils" "capture.output"
#' @details
#' Linear Regression with Little Bag of Bootstraps
"_PACKAGE"

## quiets concerns of R CMD check re: the .'s that appear in pipelines
# from https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
utils::globalVariables(c(".", "fit"))

#' Little Bag of Bootstraps with Linear Regression
#'
#' This function splits data up into m subsamples.
#' On each sample, B bootstrap trials are run to find estimates of the linear regression coefficients and standard deviation.
#' All those estimates, as well as the formula, are contained in the object returned.
#'
#' @param formula The linear regression formula, in the form of y ~ x1 + x2 + ...
#' @param data Dataset you would like to analyze
#' @param m The number of subsamples you want the data to be split into, default is 10
#' @param B The number of Bootstrap trials you want to run on each subsample, default is 5000
#'
#' @return An object that contains the regression formula ($formula) and all of the estimates from each Bootstrap trial in a vector.
#' @export
#' @examples
#' fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
blblm <- function(formula, data, m = 10, B = 5000) {
  data_list <- split_data(data, m)
  estimates <- future_map(
    data_list,
    ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}
#' Little Bag of Bootstraps with Logistic Regression
#' Same as blblm but with logistic regression
#' Also note that confint and predict are not implemented
#'
#' @param formula The linear regression formula, in the form of y ~ x1 + x2 + ...
#' @param data Dataset you would like to analyze
#' @param m The number of subsamples you want the data to be split into, default is 10
#' @param B The number of Bootstrap trials you want to run on each subsample, default is 5000
#'
#' @return An object that contains the regression formula ($formula) and all of the estimates from each Bootstrap trial in a vector.
#' @export
#' @examples
#' mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
#' pred_log <- blblm_log(admit ~ gre + gpa + rank, data = mydata)
#'
#' @export
blblm_log <- function(formula, data, m = 10, B = 5000) {
  data_list <- split_data(data, m)
  estimates <- future_map(
    data_list,
    ~ log_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}

#' Little Bag of Bootstraps with Linear Regression (Single File)
#'
#' This function performs in the same way as blblm, but using a single file instead of a predefined dataset.
#' Also this function does NOT split files into subsamples, it assumes your "subsample" is the single file.
#'
#' @param formula The linear regression formula, in the form of y ~ x1 + x2 + ...
#' @param data File name in CSV format you want to analyze
#' @param B The number of Bootstrap trials you want to run on each subsample, default is 5000

#' @return An object that contains the regression formula ($formula) and all of the estimates from each Bootstrap trial in a vector.
#' @export
#' @examples
#' # dir.create("files", showWarnings = FALSE)
#' set.seed(141)
#' 1:10 %>% walk(function(i) {
#'   dt <- tibble(x = rnorm(5), y = rnorm(5))
#'   write_csv(dt, file.path("files", sprintf("file%02d.csv", i)))
#' })
#' file_names <- file.path("files", list.files("files"))
#' setting up files to use - single file that puts together all multiple files
#' write.csv(file_names %>% lapply(read_csv) %>% bind_rows, 'files/total.csv')
#' blblm_single_file(y ~ x, 'files/total.csv', B = 10)
blblm_single_file <- function(formula, data, B = 5000) {
  # ADDED: note that data has to be one file name in CSV
  data_proc <- read.csv(data)
  estimates <- lm_each_subsample(formula = formula, data = data_proc, n = nrow(data_proc), B = B)
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}

#' Little Bag of Bootstraps with Linear Regression (Multiple Files)
#' Same as blblm but with a vector of file names, the data from each file will be its own subsample
#'
#' @param formula The linear regression formula, in the form of y ~ x1 + x2 + ...
#' @param data Vector of file names, each files serves as a subsample
#' @param B The number of Bootstrap trials you want to run on each subsample, default is 5000
#'
#'
#' @export
#' @examples
#' set.seed(141)
#' 1:10 %>% walk(function(i) {
#'   dt <- tibble(x = rnorm(5), y = rnorm(5))
#'   write_csv(dt, file.path("files", sprintf("file%02d.csv", i)))
#' })
#' file_names <- file.path("files", list.files("files"))
#' fit_multi <- blblm_multi_file(y ~ x, file_names)
blblm_multi_file <- function(formula, data, B = 5000) {
  # ADDED: note that data has to be a vector of file names in CSV
  data_list <- map(data, ~ read.csv(.))
  names(data_list) <- 1:length(data_list)
  estimates <- future_map(
    data_list,
    ~ lm_each_subsample(formula = formula, data = ., n = nrow(.), B = B))
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}


#' Splitting data
#' split data into m parts of approximated equal sizes
#'
#' @param data Dataframe containing data you want to split
#' @param m Number of parts you want to split data into
split_data <- function(data, m) {
  idx <- sample.int(m, nrow(data), replace = TRUE)
  data %>% split(idx)
}


#' Linear Regression on Subsamples
#' Compute the estimates
#'
#' @param formula Formula of linear regression
#' @param data Subsample data
#' @param n Number of rows in data
#' @param B The number of Bootstrap trials you want to run on each subsample
lm_each_subsample <- function(formula, data, n, B) {
  replicate(B, lm_each_boot(formula, data, n), simplify = FALSE)
}
#' Logistic regression on Subsamples
#' Compute the estimates (log regression)
#'
#' @param formula Formula of logistic regression
#' @param data Subsample data
#' @param n Number of rows in data
#' @param B The number of Bootstrap trials you want to run on each subsample
log_each_subsample <- function(formula, data, n, B) {
  replicate(B, log_each_boot(formula, data, n), simplify = FALSE)
}


#' Linear Regression for a Bootstrap Trial
#' Compute the linear regression estimates for a blb dataset
#'
#' @param formula Formula of linear regression
#' @param data Subsample data
#' @param n Number of rows in data
lm_each_boot <- function(formula, data, n) {
  freqs <- rmultinom(1, n, rep(1, nrow(data)))
  lm1(formula, data, freqs)
}

#' Logistic Regression for a Bootstrap Trial
#' Compute the logistic regression estimates for a blb dataset
#'
#' @param formula Formula of logistic regression
#' @param data Subsample data
#' @param n Number of rows in data
log_each_boot <- function(formula, data, n) {
  freqs <- rmultinom(1, n, rep(1, nrow(data)))
  log1(formula, data, freqs)
}

#' Linear Model for blblm
#' estimate the linear regression estimates based on given the number of repetitions
#'
#' @param formula Formula of linear regression
#' @param data Data from a particular Bootstrap trial
#' @param freqs Numeric vector that describes how often each sample should be included in the model (elements should add up to the total number of rows in the data)
lm1 <- function(formula, data, freqs) {
  # drop the original closure of formula,
  # otherwise the formula will pick a wrong variable from the global scope.
  environment(formula) <- environment()
  fit <- lm(formula, data, weights = freqs)
  list(coef = blbcoef(fit), sigma = blbsigma(fit))
}

#' Logistic Model for blblm
#' estimate the logistic regression estimates based on given the number of repetitions
#'
#' @param formula Formula of logistic regression
#' @param data Data from a particular Bootstrap trial
#' @param freqs Numeric vector that describes how often each sample should be included in the model (elements should add up to the total number of rows in the data)
log1 <- function(formula, data, freqs) {
  # drop the original closure of formula,
  # otherwise the formula will pick a wront variable from the global scope.
  environment(formula) <- environment()
  fit <- glm(formula, data, weights = freqs, family = "binomial")
  list(coef = blbcoef(fit), sigma = blbsigma(fit))
}

#' Blblm Coefficients
#' compute the coefficients from fit
#'
#' @param fit Blblm object
blbcoef <- function(fit) {
  coef(fit)
}

#' Blblm Standard Deviation
#' compute sigma from fit
#'
#' @param fit Blblm object
blbsigma <- function(fit) {
  p <- fit$rank
  y <- model.extract(fit$model, "response")
  e <- fitted(fit) - y
  w <- fit$weights
  sqrt(sum(w * (e^2)) / (sum(w) - p))
}

#' Printing blblm Object
#' Outputs the formula associated with a blblm object
#'
#' @param x Blblm object
#' @param ... Any other arguments
#' @export
#' @examples
#' fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
#' print(fit) # mpg ~ wt * hp
#' @method print blblm
print.blblm <- function(x, ...) {
  cat("blblm model:", capture.output(x$formula))
  cat("\n")
}

#' Standard deviation/confidence intervals of blblm model
#' Returns standard deviation estimate and confidence interval estimate based on given significance level (if applicable)
#' @param object Blblm object you would like to read (linear regression)
#' @param confidence Boolean indicating whether you want just standard deviation (False) or also the lower + upper confidence intervals
#' @param level If confidence is True, this level indicates the significance level of your confidence intervals
#' @param ... Any other arguments
#' @export
#' @examples
#' fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
#' sigma(fit)
#' @method sigma blblm
sigma.blblm <- function(object, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  sigma <- mean(future_map_dbl(est, ~ mean(map_dbl(., "sigma"))))
  if (confidence) {
    alpha <- 1 - 0.95
    limits <- est %>%
      map_mean(~ quantile(future_map_dbl(., "sigma"), c(alpha / 2, 1 - alpha / 2))) %>%
      set_names(NULL)
    return(c(sigma = sigma, lwr = limits[1], upr = limits[2]))
  } else {
    return(sigma)
  }
}

#' Coefficients of blblm
#' Returns regression coefficients of blblm object calculated from averaging out all coefficient estimates
#'
#' @param object Blblm object you want to find coefficients of
#' @param ... Any other arguments
#' @export
#' @examples
#' fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
#' coef(fit)
#' @method coef blblm
coef.blblm <- function(object, ...) {
  est <- object$estimates
  map_mean(est, ~ map_cbind(., "coef") %>% rowMeans())
}

#' Confidence intervals of blblm
#' Returns regression coefficients of blblm object calculated from averaging out all coefficient estimates
#'
#' @param object Blblm object you want to find coefficients of
#' @param parm Attributes that you want to find significance level of. By default is NULL
#' @param level Significance level of your confidence intervals
#' @param ... Any other arguments
#' @export
#' @examples
#' fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
#' confint(fit)
#' @export
#' @method confint blblm
confint.blblm <- function(object, parm = NULL, level = 0.95, ...) {
  if (is.null(parm)) {
    parm <- attr(terms(fit$formula), "term.labels")
  }
  alpha <- 1 - level
  est <- object$estimates
  out <- map_rbind(parm, function(p) {
    map_mean(est, ~ map_dbl(., list("coef", p)) %>% quantile(c(alpha / 2, 1 - alpha / 2)))
  })
  if (is.vector(out)) {
    out <- as.matrix(t(out))
  }
  dimnames(out)[[1]] <- parm
  out
}

#' Predicting blblm
#' Outputs predictions based on coefficient estimates of blblm object
#'
#' @param object A blblm object
#' @param new_data A data frame object that contains the new data you would like to predict on
#' @param confidence Boolean indicating whether you want confidence intervals or not. By default it is False
#' @param level Significance level of your confidence intervals (if applicable)
#' @param ... Any other arguments
#'
#' @export
#' @examples
#' fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
#' predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)))
#' @method predict blblm
predict.blblm <- function(object, new_data, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), new_data)
  if (confidence) {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>%
               apply(1, mean_lwr_upr, level = level) %>%
               t())
  } else {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>% rowMeans())
  }
}


mean_lwr_upr <- function(x, level = 0.95) {
  alpha <- 1 - level
  c(fit = mean(x), quantile(x, c(alpha / 2, 1 - alpha / 2)) %>% set_names(c("lwr", "upr")))
}

#' @param ... Any other arguments
map_mean <- function(.x, .f, ...) {
  (future_map(.x, .f, ...) %>% reduce(`+`)) / length(.x)
}

#' @param ... Any other arguments
map_cbind <- function(.x, .f, ...) {
  future_map(.x, .f, ...) %>% reduce(cbind)
}

map_rbind <- function(.x, .f, ...) {
  future_map(.x, .f, ...) %>% reduce(rbind)
}

NULL
