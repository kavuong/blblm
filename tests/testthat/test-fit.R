test.cores <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
# code you can run in a separate R script for testing:

# setting up files to use - multiple files
# dir.create("files", showWarnings = FALSE)
# set.seed(141)
# 1:10 %>% walk(function(i) {
#   dt <- tibble(x = rnorm(5), y = rnorm(5))
#   write_csv(dt, file.path("files", sprintf("file%02d.csv", i)))
# })

# setting up files to use - single file that puts together all multiple files
# write.csv(file_names %>% lapply(read_csv) %>% bind_rows, 'files/total.csv')

file_names <- file.path("files", list.files("files"))[1:10]

test_that("multiple cores for blblm", {
  plan(multiprocess, workers = 4)
  expect_is(test.cores, "blblm")
  plan(sequential)
})

test_that("multiple cores for sigma", {
  sigma_bef <- sigma(test.cores)
  plan(multiprocess, workers = 4)
  sigma_aft <- sigma(test.cores)
  plan(sequential)
  expect_equal(sigma_bef, sigma_aft)
})

test_that("multiple cores for confint", {
  confint_bef <- predict(test.cores, data.frame(wt = c(2.5, 3), hp = c(150, 170)))
  plan(multiprocess, workers = 4)
  confint_aft <- predict(test.cores, data.frame(wt = c(2.5, 3), hp = c(150, 170)))
  plan(sequential)
  expect_identical(confint_bef, confint_aft)
})

test_that("multiple cores for confint with confidence", {
  confint_bef <- predict(test.cores, data.frame(wt = c(2.5, 3), hp = c(150, 170)),confidence = T)
  plan(multiprocess, workers = 4)
  confint_aft <- predict(test.cores, data.frame(wt = c(2.5, 3), hp = c(150, 170)), confidence = T)
  plan(sequential)
  expect_identical(confint_bef, confint_aft)
})

test_that("BLBLM multiple files - function blblm_multi_file", {
  # more efficient to use parallelization
  plan(multiprocess, workers = 4)
  # testing function
  fit_multi <- blblm_multi_file(y ~ x, file_names)
  plan(sequential)
  expect_is(fit_multi, "blblm")
})

test_that("BLBLM single file - blblm_single_file function", {
  fit_single <- blblm_single_file(y ~ x, '../../files/total.csv')
  expect_is(fit_single, "blblm")
})

# removing test directory
unlink("files", recursive = T)