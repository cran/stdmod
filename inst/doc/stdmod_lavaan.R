## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width  = 6,
  fig.height = 4,
  fig.align = "center"
)

## ----setup--------------------------------------------------------------------
library(stdmod) # For computing the standardized moderation effect conveniently
library(lavaan) # For doing path analysis in lavaan.

## ----load_dataset-------------------------------------------------------------
data(test_mod1)
round(head(test_mod1, 3), 3)

## ----mod_sem------------------------------------------------------------------
mod <-
"
med ~ iv + mod + iv:mod + cov1
dv ~ med + cov2
"
fit <- sem(mod, test_mod1, fixed.x = FALSE)
summary(fit)

## -----------------------------------------------------------------------------
standardizedSolution(fit)[3, ]

## -----------------------------------------------------------------------------
fit_iv_mod_std <- stdmod_lavaan(fit = fit,
                                x = "iv",
                                y = "med",
                                w = "mod",
                                x_w = "iv:mod")
fit_iv_mod_std

## ----echo = FALSE-------------------------------------------------------------
if (file.exists("egl_out_boot.rds")) {
    fit_iv_mod_std_ci <- readRDS("egl_out_boot.rds")
  } else {
    set.seed(860947)
    fit_iv_mod_std_ci <- stdmod_lavaan(fit = fit,
                                        x = "iv",
                                        y = "med",
                                        w = "mod",
                                        x_w = "iv:mod",
                                        boot_ci = TRUE,
                                        R = 2000,
                                        parallel = "snow",
                                        ncpus = 6)
    saveRDS(out_boot, "egl_out_boot.rds")
  }

## ----eval = FALSE-------------------------------------------------------------
#  set.seed(860947)
#  fit_iv_mod_std_ci <- stdmod_lavaan(fit = fit,
#                                      x = "iv",
#                                      y = "med",
#                                      w = "mod",
#                                      x_w = "iv:mod",
#                                      boot_ci = TRUE,
#                                      R = 2000,
#                                      parallel = "snow",
#                                      ncpus = 6)
#  fit_iv_mod_std_ci

## ----echo = FALSE-------------------------------------------------------------
fit_iv_mod_std_ci

