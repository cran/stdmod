#' @title Standardized Moderation Effect in a 'stdmod_lavaan' Class Object
#'
#' @description Return the estimate of the standardized
#'              moderation effect
#'              in the output of [stdmod_lavaan()].
#'
#' @details It just extracts and returns the element `stdmod`.
#'
#' @return
#'  A scalar: The estimate of the standardized moderation effect.
#'
#' @param object The output of [stdmod_lavaan()].
#' @param ...  Optional arguments. Ignored by the function.
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
#' # Compute the standardized moderation effect
#' out_noboot <- stdmod_lavaan(fit = fit,
#'                             x = "iv",
#'                             y = "med",
#'                             w = "mod",
#'                             x_w = "iv:mod")
#' coef(out_noboot)
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
#' coef(out_boot)
#'
#' @export


coef.stdmod_lavaan <- function(object, ...) {
    object$stdmod
  }
