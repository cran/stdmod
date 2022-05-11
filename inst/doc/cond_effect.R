## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width  = 6,
  fig.height = 4,
  fig.align = 'center'
)

## ----echo = FALSE-------------------------------------------------------------
library(stdmod)
data(sleep_emo_con)
lm_out <- lm(sleep_duration ~ age + gender +
                              emotional_stability * conscientiousness,
                              sleep_emo_con)
cond_out <- cond_effect(output = lm_out,
                        x = "emotional_stability",
                        w = "conscientiousness")
print(cond_out, table_only = TRUE)

## ----echo = FALSE-------------------------------------------------------------
data(sleep_emo_con)
lm_out <- lm(sleep_duration ~ age + gender +
                              emotional_stability * conscientiousness,
                              sleep_emo_con)
lm_std <- std_selected(lm_out,
                       to_center = ~ .,
                       to_scale = ~ .)
cond_std <- cond_effect(output = lm_std,
                        x = "emotional_stability",
                        w = "conscientiousness")
print(cond_std, table_only = TRUE)

## -----------------------------------------------------------------------------
library(stdmod)
data(sleep_emo_con)
lm_out <- lm(sleep_duration ~ age + gender +
                              emotional_stability * conscientiousness,
                              sleep_emo_con)
cond_out <- cond_effect(output = lm_out,
                        x = "emotional_stability",
                        w = "conscientiousness")
cond_out

## -----------------------------------------------------------------------------
print(cond_out, table_only = TRUE)

## -----------------------------------------------------------------------------
data(sleep_emo_con)
lm_out <- lm(sleep_duration ~ age + gender +
                              emotional_stability * conscientiousness,
                              sleep_emo_con)
cond_out <- cond_effect(output = lm_out,
                        x = "emotional_stability",
                        w = "conscientiousness",
                        w_method = "percentile")
print(cond_out, title = FALSE, model = FALSE)

## -----------------------------------------------------------------------------
set.seed(61452)
sleep_emo_con$city <- sample(c("Alpha", "Beta", "Gamma"),
                               nrow(sleep_emo_con), replace = TRUE)
lm_cat <- lm(sleep_duration ~ age + gender + emotional_stability*city,
                              sleep_emo_con)
cond_out <- cond_effect(lm_cat,
                        x = "emotional_stability",
                        w = "city")
print(cond_out, title = FALSE, model = FALSE)

## -----------------------------------------------------------------------------
lm_out <- lm(sleep_duration ~ age + gender +
                              emotional_stability * conscientiousness,
                              sleep_emo_con)
# Standardize all variables and do the moderated regression again
lm_std <- std_selected(lm_out,
                       to_center = ~ .,
                       to_scale = ~ .)
# `nboot` is the sufficient. Set it to at least 2000 in real analysis
cond_std <- cond_effect_boot(output = lm_std,
                             x = "emotional_stability",
                             w = "conscientiousness",
                             nboot = 500)
print(cond_std, model = FALSE, title = FALSE, level_info = FALSE)

