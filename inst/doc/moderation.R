## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width  = 6,
  fig.height = 4,
  fig.align = "center"
)

## ----setup--------------------------------------------------------------------
library(stdmod) # For computing the standardized moderation effect conveniently

## ----load_dataset-------------------------------------------------------------
data(sleep_emo_con)
head(sleep_emo_con, 3)

## -----------------------------------------------------------------------------
colnames(sleep_emo_con)[3:4] <- c("cons", "emot")
head(sleep_emo_con, 3)

## ----mod_reg------------------------------------------------------------------
lm_out <- lm(sleep_duration ~ age + gender + emot * cons,
             data = sleep_emo_con)
summary(lm_out)
plotmod(lm_out,
        x = "emot",
        w = "cons",
        x_label = "Emotional Stability",
        w_label = "Conscientiousness",
        y_label = "Sleep Duration")

## -----------------------------------------------------------------------------
lm_stdall <- std_selected(lm_out,
                          to_standardize = ~ .)

## -----------------------------------------------------------------------------
summary(lm_stdall)

## ----mod_reg_stdall-----------------------------------------------------------
plotmod(lm_stdall,
        x = "emot",
        w = "cons",
        x_label = "Emotional Stability",
        w_label = "Conscientiousness",
        y_label = "Sleep Duration")

## -----------------------------------------------------------------------------
library(lm.beta) # For generating the typical standardized solution
packageVersion("lm.beta")
lm_beta <- lm.beta(lm_out)
summary(lm_beta)

## ----echo = FALSE, eval = TRUE------------------------------------------------
if (file.exists("eg2_lm_xwy_std_ci.rds")) {
    lm_xwy_std_ci <- readRDS("eg2_lm_xwy_std_ci.rds")
  } else {
    set.seed(649017)
    lm_xwy_std_ci <- std_selected_boot(lm_out, to_center = ~ .,
                                              to_scale  = ~ .,
                                              nboot = 2000)
    saveRDS(lm_xwy_std_ci, "eg2_lm_xwy_std_ci.rds", compress = "xz")
  }

## -----------------------------------------------------------------------------
summary(lm_xwy_std_ci)

## ----echo = FALSE-------------------------------------------------------------
tmp <- summary(lm_xwy_std_ci)$coefficients

