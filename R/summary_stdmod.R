#' @title Summary Method for a 'std_selected' Class Object
#'
#' @description Summarize the results of [std_selected()] or
#'             [std_selected_boot()].
#'
#' @return
#'  An object of class `summary.std_selected`, with
#'  bootstrap confidence intervals added if present in the object.
#'  The object is a list. Its main element `coefficients` is similar to
#'  the
#'  coefficient table in the [summary()] printout of [lm()].
#'  This object is for printing summary information of the results
#'  from [std_selected()] or [std_selected_boot()].
#'
#' @param object The output of [std_selected()] or [std_selected_boot()].
#' @param ...  Additional arguments. Ignored by this function.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#'
#' # Load a sample data set
#'
#' dat <- test_x_1_w_1_v_1_cat1_n_500
#'
#' # Do a moderated regression by lm
#' lm_raw <- lm(dv ~ iv*mod + v1 + cat1, dat)
#' summary(lm_raw)
#'
#' # Standardize all variables except for categorical variables.
#' # Interaction terms are formed after standardization.
#' lm_std <- std_selected(lm_raw, to_scale = ~ .,
#'                                to_center = ~ .)
#' summary(lm_std)
#'
#' # With bootstrapping
#' # nboot = 100 just for illustration. nboot >= 2000 should be used in read
#' # research.
#' lm_std_boot <- std_selected_boot(lm_raw, to_scale = ~ .,
#'                                          to_center = ~ .,
#'                                          nboot = 100)
#' summary(lm_std_boot)
#'
#' @export

summary.std_selected <- function(object, ...) {
    out <- stats::summary.lm(object, ...)
    out$scaled_terms <- object$scaled_terms
    out$centered_terms <- object$centered_terms
    out$scaled_by <- object$scaled_by
    out$centered_by <- object$centered_by
    out$nboot <- object$nboot
    if (!is.null(object$boot_ci)) {
      out$coefficients <- cbind(out$coefficients[, 1, drop = FALSE],
                                object$boot_ci,
                                out$coefficients[, -1])
      }
    class(out) <- c("summary.std_selected", class(out))
    out
  }
