#' @title Confidence Intervals for a 'stdmod_lavaan' Class Object
#'
#' @description Return the confidence interval of the standardized
#'              moderation effect
#'              in the output of [stdmod_lavaan()].
#'
#' @details If bootstrapping is used to form the confidence interval by
#' [stdmod_lavaan()],
#' users can request the percentile confidence interval of
#' using the stored bootstrap estimate.
#'
#' @return
#'  A one-row matrix of the confidence intervals.
#'
#' @param object The output of [stdmod_lavaan()].
#' @param parm Ignored. Always return the bootstrap confidence interval
#'              of the standardized moderation effect.
#' @param level The level of confidence, default is .95, returning the
#'               95% confidence interval.
#' @param ...  Additional arguments. Ignored by the function.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#'
#' @examples
#'
#' # Load a test data of 500 cases
#' dat <- test_mod1
#' library(lavaan)
#'
#' mod <-
#' "
#' med ~ iv + mod + iv:mod + cov1
#' dv ~ med + cov2
#' "
#' fit <- sem(mod, dat)
#' coef(fit)
#'
#' # Compute the standardized moderation effect and
#' # its confidence interval based on nonparametric bootstrapping
#' # Fit the model with bootstrap confidence intervals
#' # At least 2000 bootstrap samples should be used
#' # in real research. 50 is used here only for
#' # illustration.
#' fit <- sem(mod, dat, se = "boot", bootstrap = 50,
#'            iseed = 89574)
#' out_boot <- stdmod_lavaan(fit = fit,
#'                           x = "iv",
#'                           y = "med",
#'                           w = "mod",
#'                           x_w = "iv:mod",
#'                           boot_ci = TRUE)
#' confint(out_boot)
#'
#' @export


confint.stdmod_lavaan <- function(object, parm, level = .95, ...) {
    if (isTRUE(object$call$boot_ci)) {
        out0 <- boot::boot.ci(object$boot_out,
                            type = "perc",
                            conf = level)$percent[4:5]
      } else {
        warning("Bootstrapping output not in the object.")
        out0 <- c(NA, NA)
      }
    # Borrowed from stats::confint()
    probs <- c((1 - level) / 2, 1 - (1 - level) / 2)
    cnames <- paste(format(100 * probs,
                           trim = TRUE,
                           scientific = FALSE,
                           digits = 2), "%")
    rnames <- paste0(object$call$y, "~", object$call$x_w)
    out <- array(data = out0,
                 dim = c(1, 2),
                 dimnames = list(rnames, cnames))
    out
  }
