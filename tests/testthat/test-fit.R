library(furrr)
test.cores <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)

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