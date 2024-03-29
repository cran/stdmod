#' @title Confidence Intervals for a 'std_selected' Class Object
#'
#' @description Return the confidence intervals of estimates
#'              in the output of [std_selected()] or [std_selected_boot()].
#'
#' @details If bootstrapping is used to form the confidence interval by
#' [std_selected_boot()],
#' users can request the percentile confidence intervals of
#' the bootstrap estimates. This method does not do the bootstrapping itself.
#'
#' @return
#'  A matrix of the confidence intervals.
#'
#' @param object The output of [std_selected()] or [std_selected_boot()].
#' @param parm The parameters (coefficients) for which confidence intervals
#'            should be returned.
#'            If missing, the confidence intervals of all parameters will be
#'            returned.
#' @param level The level of confidence. For the confidence intervals returned
#'             by [lm()], default is .95, i.e., 95%. For the bootstrap
#'             percentile confidence
#'             intervals, default is the level used in calling
#'             [std_selected_boot()]. If a level different from that in the
#'             original
#'             call is specified, `full_output` needs to be set in the call
#'             to [std_selected_boot()] such that the original bootstrapping
#'             output
#'             is stored.
#' @param type The type of the confidence intervals. If est to `"lm"`,
#'            returns the confidence interval given by the [confint()] method
#'            of [lm()].
#'            If set to `"boot"`, the bootstrap percentile confidence intervals
#'            are returned.
#'             Default is `"boot"` if bootstrap estimates are stored in
#'             `object`, and `"lm"` if bootstrap estimates are not stored.
#' @param ...  Arguments to be passed to [summary.lm()].
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
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
#' lm_std <- std_selected(lm_raw, to_center = ~ .,
#'                                to_scale = ~ .)
#' # Alternative: use to_standardize as a shortcut
#' # lm_std <- std_selected(lm_raw, to_standardize = ~ .)
#' summary(lm_std)
#'
#' confint(lm_std)
#'
#' # Use to_standardize as a shortcut
#' lm_std2 <- std_selected(lm_raw, to_standardize = ~ .)
#' # The results are the same
#' confint(lm_std)
#' confint(lm_std2)
#' all.equal(confint(lm_std), confint(lm_std2))
#'
#' # With bootstrapping
#' # nboot = 100 just for illustration. nboot >= 2000 should be used in read
#' # research.
#' set.seed(89572)
#' lm_std_boot <- std_selected_boot(lm_raw, to_scale = ~ .,
#'                                          to_center = ~ .,
#'                                          nboot = 100)
#' summary(lm_std_boot)
#'
#' # Bootstrap percentile intervals, default when bootstrap was conduced
#'
#' confint(lm_std_boot)
#'
#' # Force OLS confidence intervals
#'
#' confint(lm_std_boot, type = "lm")
#'
#' # Use to_standardize as a shortcut
#' set.seed(89572)
#' lm_std_boot2 <- std_selected_boot(lm_raw, to_standardize = ~ .,
#'                                           nboot = 100)
#' # The results are the same
#' confint(lm_std_boot)
#' confint(lm_std_boot2)
#' all.equal(confint(lm_std_boot), confint(lm_std_boot2))
#'
#' @export


confint.std_selected <- function(object, parm, level = .95, type, ...) {
    if (!missing(type)) {
        if (!(type %in% c("lm", "boot"))) {
            stop("type must be either lm or boot.")
          }
      } else {
        if (is.null(object$boot_est)) {
            type <- "lm"
          } else {
            type <- "boot"
          }
      }
    if (type == "boot") {
        if (is.null(object$boot_est)) {
            stop("Bootstrap estimates not available in the object.")
          }
        if ((level != object$conf) && is.null(object$boot_out)) {
            stop(paste("level is different form conf in",
                       "std_selected_boot() but full_output is FALSE."))
          }
        if (level == object$conf) {
            out <- object$boot_ci
          } else {
            p <- nrow(object$boot_ci)
            out <- t(sapply(seq_len(p), function(x) {
                        boot::boot.ci(object$boot_out, conf = level,
                                      type = "perc", index = x)$percent[4:5]
                      }))
            rownames(out) <- names(object$boot_out$t0)
          }
        tmp <- object
        class(tmp) <- "lm"
        colnames(out) <- colnames(stats::confint(tmp, level = level))
        if (!missing(parm)) {
            out <- out[rownames(out) %in% parm, , drop = FALSE]
          }
        return(out)
      }
    if (type == "lm") {
        object$boot_est <- NULL
        NextMethod()
      }
  }
