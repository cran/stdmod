library(testthat)
library(stdmod)

context("Check standardized moderation effect")

dat <- test_x_1_w_1_v_2_n_500

transform0 <- function(data, vars) {
    for (x in vars) {
        data[x] <- scale(data[[x]])[, 1]
      }
    data
  }

lm_raw <- lm(dv ~ iv*mod + v1 + v2, dat)
lm_zx  <- lm(dv ~ iv*mod + v1 + v2, transform0(dat, c("iv")))
lm_zw  <- lm(dv ~ iv*mod + v1 + v2, transform0(dat, c("mod")))
lm_zy  <- lm(dv ~ iv*mod + v1 + v2, transform0(dat, c("dv")))
lm_zxzw  <- lm(dv ~ iv*mod + v1 + v2, transform0(dat, c("iv", "mod")))
lm_zxzy  <- lm(dv ~ iv*mod + v1 + v2, transform0(dat, c("iv", "dv")))
lm_zyzw  <- lm(dv ~ iv*mod + v1 + v2, transform0(dat, c("dv", "mod")))
lm_zall  <- lm(dv ~ iv*mod + v1 + v2, transform0(dat, c("iv", "dv", "mod")))

stdmod_x <- stdmod(lm_raw, x = iv, y = dv, w = mod,
                   x_rescale = TRUE,  y_rescale = FALSE, w_rescale = FALSE)
stdmod_y <- stdmod(lm_raw, x = iv, y = dv, w = mod,
                   x_rescale = FALSE, y_rescale = TRUE,  w_rescale = FALSE)
stdmod_w <- stdmod(lm_raw, x = iv, y = dv, w = mod,
                   x_rescale = FALSE, y_rescale = FALSE, w_rescale = TRUE )
stdmod_xw <- stdmod(lm_raw, x = iv, y = dv, w = mod,
                   x_rescale = TRUE , y_rescale = FALSE, w_rescale = TRUE )
stdmod_yw <- stdmod(lm_raw, x = iv, y = dv, w = mod,
                   x_rescale = FALSE, y_rescale = TRUE , w_rescale = TRUE )
stdmod_xy <- stdmod(lm_raw, x = iv, y = dv, w = mod,
                   x_rescale = TRUE , y_rescale = TRUE,  w_rescale = FALSE)
stdmod_xyw <- stdmod(lm_raw, x = iv, y = dv, w = mod,
                     x_rescale = TRUE, y_rescale = TRUE, w_rescale = TRUE)
stdmod_xyw2 <- stdmod(lm_raw, x = iv, y = dv, w = mod)

test_that("Standardize x", {
    expect_equivalent(
        stdmod_x, coef(lm_zx)["iv:mod"]
      )
  })

test_that("Standardize y", {
    expect_equivalent(
        stdmod_y, coef(lm_zy)["iv:mod"]
      )
  })

test_that("Standardize w", {
    expect_equivalent(
        stdmod_w, coef(lm_zw)["iv:mod"]
      )
  })

test_that("Standardize xy", {
    expect_equivalent(
        stdmod_xy, coef(lm_zxzy)["iv:mod"]
      )
  })

test_that("Standardize xw", {
    expect_equivalent(
        stdmod_xw, coef(lm_zxzw)["iv:mod"]
      )
  })

test_that("Standardize yw", {
    expect_equivalent(
        stdmod_yw, coef(lm_zyzw)["iv:mod"]
      )
  })

test_that("Standardize x, y, and w", {
    expect_equivalent(
        stdmod_xyw, coef(lm_zall)["iv:mod"]
      )
  })

test_that("Standardize x, y, and w with default", {
    expect_equivalent(
        stdmod_xyw2, coef(lm_zall)["iv:mod"]
      )
  })

