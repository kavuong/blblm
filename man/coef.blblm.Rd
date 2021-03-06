% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blblm.R
\name{coef.blblm}
\alias{coef.blblm}
\title{Coefficients of blblm
Returns regression coefficients of blblm object calculated from averaging out all coefficient estimates}
\usage{
\method{coef}{blblm}(object, ...)
}
\arguments{
\item{object}{Blblm object you want to find coefficients of}

\item{...}{Any other arguments}
}
\description{
Coefficients of blblm
Returns regression coefficients of blblm object calculated from averaging out all coefficient estimates
}
\examples{
fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
coef(fit)
}
