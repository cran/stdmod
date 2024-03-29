% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vcov_stdmod.R
\name{vcov.std_selected}
\alias{vcov.std_selected}
\title{The 'vcov' Method for a 'std_selected' Class Object}
\usage{
\method{vcov}{std_selected}(object, type, ...)
}
\arguments{
\item{object}{The output of \code{\link[=std_selected]{std_selected()}} or \code{\link[=std_selected_boot]{std_selected_boot()}}.}

\item{type}{The type of variance-covariance matrix. If set to \code{"lm"},
returns the results of the \code{\link[stats:vcov]{stats::vcov()}} method for the output of
\code{\link[=lm]{lm()}}. If set to \code{"boot"},
the variance-covariance matrix of the bootstrap estimates
is returned.
Default depends on \code{object}. If bootstrap estimates were stored,
then the default is \code{"boot"}. Otherwise, the default is \code{"lm"}.}

\item{...}{Arguments to be passed to \code{\link[stats:vcov]{stats::vcov()}}.}
}
\value{
A matrix of the variances and covariances of the parameter estimates.
}
\description{
Compute the variance-covariance matrix
of estimates in the output of \code{\link[=std_selected]{std_selected()}} or
\code{\link[=std_selected_boot]{std_selected_boot()}}.
}
\details{
If bootstrapping was used to form the confidence intervals,
users can request the variance-covariance matrix of the bootstrap estimates.
}
\examples{
# Load a sample data set

dat <- test_x_1_w_1_v_1_cat1_n_500
head(dat)

# Do a moderated regression by lm
lm_raw <- lm(dv ~ iv*mod + v1 + cat1, dat)

# Standardize all variables except for categorical variables.
# Interaction terms are formed after standardization.
lm_std <- std_selected(lm_raw, to_scale = ~ .,
                               to_center = ~ .)

# VCOV of lm output
vcov(lm_std)

# Standardize all variables as in std_selected above, and compute the
# nonparametric bootstrapping percentile confidence intervals.
lm_std_boot <- std_selected_boot(lm_raw,
                                 to_scale = ~ .,
                                 to_center = ~ .,
                                 conf = .95,
                                 nboot = 100)
# In real analysis, nboot should be at least 2000.

# VCOV of bootstrap estimates, default when bootstrap was conducted
vcov(lm_std_boot)

# For OLS VCOV
vcov(lm_std_boot, type = "lm")

}
\author{
Shu Fai Cheung \url{https://orcid.org/0000-0002-9871-9448}
}
