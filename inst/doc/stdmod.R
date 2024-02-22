## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width  =  6,
  fig.height =  4,
  fig.align = "center"
)

## -----------------------------------------------------------------------------
library(stdmod)
dat <- sleep_emo_con
head(dat, 3)

## -----------------------------------------------------------------------------
colnames(dat)[2:4] <- c("sleep", "cons", "emot")
head(dat, 3)

## -----------------------------------------------------------------------------
lm_out <- lm(sleep ~ age + gender + emot * cons,
             dat = dat)
summary(lm_out)

## -----------------------------------------------------------------------------
lm_stdall <- std_selected(lm_out,
                          to_standardize = ~ .)

## -----------------------------------------------------------------------------
summary(lm_stdall)

## ----echo = FALSE-------------------------------------------------------------
if (file.exists("stdmod_lm_stdall_boot.rds")) {
    lm_stdall_boot <- readRDS("stdmod_lm_stdall_boot.rds")
  } else {
    set.seed(870432)
    lm_stdall_boot <- std_selected_boot(lm_out,
                            to_scale = ~ .,
                            to_center = ~ .,
                            nboot = 5000)
    saveRDS(lm_stdall_boot, "stdmod_lm_stdall_boot.rds", compress = "xz")
  }

## ----eval = FALSE-------------------------------------------------------------
#  set.seed(870432)
#  lm_stdall_boot <- std_selected_boot(lm_out,
#                          to_scale = ~ .,
#                          to_center = ~ .,
#                          nboot = 5000)

## -----------------------------------------------------------------------------
summary(lm_stdall_boot)

## ----echo = FALSE-------------------------------------------------------------
tmp <- summary(lm_stdall_boot)$coefficients

## -----------------------------------------------------------------------------
lm_std1 <- std_selected(lm_out,
                        to_standardize = ~ emot + cons)

## -----------------------------------------------------------------------------
summary(lm_std1)

## ----echo = FALSE-------------------------------------------------------------
if (file.exists("stdmod_lm_std1_boot.rds")) {
    lm_std1_boot <- readRDS("stdmod_lm_std1_boot.rds")
  } else {
    set.seed(870432)
    lm_std1_boot <- std_selected_boot(lm_out,
                            to_scale = ~ emot + cons,
                            to_center = ~ emot + cons,
                            nboot = 5000)
    saveRDS(lm_std1_boot, "stdmod_lm_std1_boot.rds", compress = "xz")
  }

## ----eval = FALSE-------------------------------------------------------------
#  set.seed(870432)
#  lm_std1_boot <- std_selected_boot(lm_out,
#                          to_scale = ~ emot + cons,
#                          to_center = ~ emot + cons,
#                          nboot = 5000)

## -----------------------------------------------------------------------------
summary(lm_std1_boot)

## ----echo = FALSE-------------------------------------------------------------
tmp <- summary(lm_std1_boot)$coefficients

