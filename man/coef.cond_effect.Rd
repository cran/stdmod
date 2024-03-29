% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coef_condeff.R
\name{coef.cond_effect}
\alias{coef.cond_effect}
\title{Conditional Effect in a 'cond_effect'-Class Object}
\usage{
\method{coef}{cond_effect}(object, ...)
}
\arguments{
\item{object}{The output of \code{\link[=cond_effect]{cond_effect()}} or \code{\link[=cond_effect_boot]{cond_effect_boot()}}.}

\item{...}{Optional arguments. Ignored by the function.}
}
\value{
A numeric vector: The estimates of the conditional
effects in a \code{cond_effect}-class object.
}
\description{
Return the estimates of the conditional
effects
in the output of
\code{\link[=cond_effect]{cond_effect()}} or \code{\link[=cond_effect_boot]{cond_effect_boot()}}.
}
\details{
It just extracts and returns the column of
conditional effects in a \code{cond_effect}-class object.
}
\examples{

# Load a sample data set

dat <- test_x_1_w_1_v_1_cat1_n_500

# Do a moderated regression by lm
lm_raw <- lm(dv ~ iv*mod + v1 + cat1, dat)
summary(lm_raw)
out <- cond_effect(lm_raw, x = iv, w = mod)
out
coef(out)

lm_std <- std_selected(lm_raw, to_standardize = ~ iv + mod)
out <- cond_effect(lm_std, x = iv, w = mod)
out
coef(out)

# Categorical moderator
lm_cat <- lm(dv ~ iv*cat1 + v1, dat)
summary(lm_cat)
out <- cond_effect(lm_cat, x = iv, w = cat1)
out
coef(out)

}
\author{
Shu Fai Cheung \url{https://orcid.org/0000-0002-9871-9448}
}
