## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width  = 6,
  fig.height = 4,
  fig.align = 'center'
)

## ----setup--------------------------------------------------------------------
library(stdmod)

## ----load_dataset-------------------------------------------------------------
data(sleep_emo_con)
head(sleep_emo_con, 3)

## -----------------------------------------------------------------------------
colnames(sleep_emo_con)[3:4] <- c("cons", "emot")
head(sleep_emo_con, 3)

## ----mod_reg------------------------------------------------------------------
lm_raw <- lm(sleep_duration ~ age + gender + emot * cons,
             data = sleep_emo_con)
summary(lm_raw)

## -----------------------------------------------------------------------------
plotmod(lm_raw,
        x = "emot",
        w = "cons",
        x_label = "Emotional Stability",
        w_label = "Conscientiousness",
        y_label = "Sleep Duration")

## -----------------------------------------------------------------------------
lm_w_centered <- std_selected(lm_raw,
                              to_center = ~ cons)
printCoefmat(summary(lm_w_centered)$coefficients, digits = 3)

## -----------------------------------------------------------------------------
lm_xw_centered <- std_selected(lm_raw,
                               to_center = ~ emot + cons)
printCoefmat(summary(lm_xw_centered)$coefficients, digits = 3)

## -----------------------------------------------------------------------------
lm_xw_std <- std_selected(lm_raw,
                          to_center = ~ emot + cons,
                          to_scale  = ~ emot + cons)
printCoefmat(summary(lm_xw_std)$coefficients, digits = 3)

## -----------------------------------------------------------------------------
plotmod(lm_xw_std,
        x = "emot",
        w = "cons",
        x_label = "Emotional Stability",
        w_label = "Conscientiousness",
        y_label = "Sleep Duration")

## -----------------------------------------------------------------------------
lm_xwy_std <- std_selected(lm_raw,
                           to_center = ~ emot + cons + sleep_duration,
                           to_scale  = ~ emot + cons + sleep_duration)
printCoefmat(summary(lm_xwy_std)$coefficients, digits = 3)

## -----------------------------------------------------------------------------
plotmod(lm_xwy_std,
        x = "emot",
        w = "cons",
        x_label = "Emotional Stability",
        w_label = "Conscientiousness",
        y_label = "Sleep Duration")

## -----------------------------------------------------------------------------
lm_all_std <- std_selected(lm_raw,
                           to_center = ~ .,
                           to_scale  = ~ .)
printCoefmat(summary(lm_all_std)$coefficients, digits = 3)

## -----------------------------------------------------------------------------
library(lm.beta) # For generating the typical standardized solution
lm_usual_std <- lm.beta(lm_raw)
printCoefmat(summary(lm_usual_std)$coefficients, digits = 3)

## ----echo = FALSE-------------------------------------------------------------
if (file.exists("eg_lm_xwy_std_ci.rds")) {
    lm_xwy_std_ci <- readRDS("eg_lm_xwy_std_ci.rds")
  } else {
    set.seed(58702)
    lm_xwy_std_ci <- std_selected_boot(lm_raw,
        to_center = ~ emot + cons + sleep_duration,
        to_scale  = ~ emot + cons + sleep_duration,
        nboot = 2000)
    saveRDS(lm_xwy_std_ci, "eg_lm_xwy_std_ci.rds")
  }

## ----eval = FALSE-------------------------------------------------------------
#  set.seed(58702)
#  lm_xwy_std_ci <- std_selected_boot(lm_raw,
#                                     to_center = ~ emot + cons + sleep_duration,
#                                     to_scale  = ~ emot + cons + sleep_duration,
#                                     nboot = 2000)

## -----------------------------------------------------------------------------
summary(lm_xwy_std_ci)

## ----echo = FALSE-------------------------------------------------------------
tmp <- summary(lm_xwy_std_ci)$coefficients

