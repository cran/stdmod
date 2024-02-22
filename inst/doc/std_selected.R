## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width  = 6,
  fig.height = 4,
  fig.align = "center"
)

## ----setup--------------------------------------------------------------------
library(stdmod)

## ----load_dataset-------------------------------------------------------------
data(sleep_emo_con)
head(sleep_emo_con, 3)

## ----shorten_names------------------------------------------------------------
colnames(sleep_emo_con)[3:4] <- c("cons", "emot")
head(sleep_emo_con, 3)

## ----mod_reg------------------------------------------------------------------
lm_raw <- lm(sleep_duration ~ age + gender + emot * cons,
             data = sleep_emo_con)
summary(lm_raw)

## ----std_selected_lm_raw_plot-------------------------------------------------
plotmod(lm_raw,
        x = "emot",
        w = "cons",
        x_label = "Emotional Stability",
        w_label = "Conscientiousness",
        y_label = "Sleep Duration")

## ----lm_w_centered------------------------------------------------------------
lm_w_centered <- std_selected(lm_raw,
                              to_center = ~ cons)
printCoefmat(summary(lm_w_centered)$coefficients, digits = 3)

## ----lm_xw_centered-----------------------------------------------------------
lm_xw_centered <- std_selected(lm_raw,
                               to_center = ~ emot + cons)
printCoefmat(summary(lm_xw_centered)$coefficients, digits = 3)

## ----lm_xw_std----------------------------------------------------------------
lm_xw_std <- std_selected(lm_raw,
                          to_standardize = ~ emot + cons)

## ----lm_xw_std_coef-----------------------------------------------------------
printCoefmat(summary(lm_xw_std)$coefficients, digits = 3)

## ----std_selected_lm_xw_std_plot----------------------------------------------
plotmod(lm_xw_std,
        x = "emot",
        w = "cons",
        x_label = "Emotional Stability",
        w_label = "Conscientiousness",
        y_label = "Sleep Duration")

## ----lm_xwy_std---------------------------------------------------------------
lm_xwy_std <- std_selected(lm_raw,
                           to_standardize = ~ emot + cons + sleep_duration)
printCoefmat(summary(lm_xwy_std)$coefficients, digits = 3)

## ----std_selected_lm_xwy_std_plot---------------------------------------------
plotmod(lm_xwy_std,
        x = "emot",
        w = "cons",
        x_label = "Emotional Stability",
        w_label = "Conscientiousness",
        y_label = "Sleep Duration")

## ----lm_all_std---------------------------------------------------------------
lm_all_std <- std_selected(lm_raw,
                           to_standardize = ~ .)
printCoefmat(summary(lm_all_std)$coefficients, digits = 3)

## ----lm_beta------------------------------------------------------------------
library(lm.beta) # For generating the typical standardized solution
packageVersion("lm.beta")
lm_usual_std <- lm.beta(lm_raw)
printCoefmat(summary(lm_usual_std)$coefficients, digits = 3)

## ----echo = FALSE, eval = TRUE------------------------------------------------
if (file.exists("eg_lm_xwy_std_ci.rds")) {
    lm_xwy_std_ci <- readRDS("eg_lm_xwy_std_ci.rds")
  } else {
    set.seed(58702)
    lm_xwy_std_ci <- std_selected_boot(lm_raw,
        to_center = ~ emot + cons + sleep_duration,
        to_scale  = ~ emot + cons + sleep_duration,
        nboot = 2000)
    saveRDS(lm_xwy_std_ci, "eg_lm_xwy_std_ci.rds", compress = "xz")
  }

## ----lm_xwy_std_ci_summary----------------------------------------------------
summary(lm_xwy_std_ci)

## ----echo = FALSE-------------------------------------------------------------
tmp <- summary(lm_xwy_std_ci)$coefficients

