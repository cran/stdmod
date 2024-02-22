## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width  = 6,
  fig.height = 4,
  fig.align = "center"
)

## ----plotmod_example, echo = FALSE--------------------------------------------
library(stdmod)
data(sleep_emo_con)
lm_raw <- lm(sleep_duration ~ age + gender +
                              emotional_stability * conscientiousness,
             sleep_emo_con)
plotmod(lm_raw,
        x = "emotional_stability",
        w = "conscientiousness",
        x_label = "Emotional Stability",
        w_label = "Conscientiousness",
        y_label = "Sleep Duration")

## ----plotmod_example2, echo = FALSE-------------------------------------------
plotmod(lm_raw,
        x = "emotional_stability",
        w = "conscientiousness",
        x_label = "Emotional Stability",
        w_label = "Conscientiousness",
        y_label = "Sleep Duration",
        graph_type = "tumble")

## ----plotmod_example_raw------------------------------------------------------
library(stdmod)
data(sleep_emo_con)
lm_raw <- lm(sleep_duration ~ age + gender +
                              emotional_stability * conscientiousness,
             sleep_emo_con)
plotmod(lm_raw,
        x = "emotional_stability",
        w = "conscientiousness")

## ----plotmod_example_raw_numeric----------------------------------------------
lm_raw <- lm(sleep_duration ~ age + gender +
                              emotional_stability * conscientiousness,
             sleep_emo_con)
plotmod(lm_raw,
        x = "emotional_stability",
        w = "conscientiousness",
        w_method = "percentile")

## ----plotmod_example_raw_cat--------------------------------------------------
set.seed(61452)
sleep_emo_con$city <- sample(c("Alpha", "Beta", "Gamma"),
                             nrow(sleep_emo_con),
                             replace = TRUE)
lm_cat <- lm(sleep_duration ~ age + gender +
                              emotional_stability * city,
             sleep_emo_con)
plotmod(lm_cat,
        x = "emotional_stability",
        w = "city")

## ----plotmod_tumble-----------------------------------------------------------
lm_raw <- lm(sleep_duration ~ age + gender +
                              emotional_stability * conscientiousness,
             sleep_emo_con)
plotmod(lm_raw,
        x = "emotional_stability",
        w = "conscientiousness",
        graph_type = "tumble")

## ----plotmod_annotate---------------------------------------------------------
lm_raw <- lm(sleep_duration ~ age + gender +
                              emotional_stability * conscientiousness,
             sleep_emo_con)
plotmod(lm_raw,
        x = "emotional_stability",
        w = "conscientiousness",
        x_label = "EMO",
        w_label = "CON",
        y_label = "SLEEP")

## ----plotmod_title------------------------------------------------------------
lm_raw <- lm(sleep_duration ~ age + gender +
                              emotional_stability * conscientiousness,
             sleep_emo_con)
plotmod(lm_raw,
        x = "emotional_stability",
        w = "conscientiousness",
        title = "EMO Effects For Low/High CON")

## ----plotmod_line_width-------------------------------------------------------
lm_raw <- lm(sleep_duration ~ age + gender +
                              emotional_stability * conscientiousness,
             sleep_emo_con)
plotmod(lm_raw,
        x = "emotional_stability",
        w = "conscientiousness",
        point_size = 8,
        line_width = 2)

## ----plotmod_cond-------------------------------------------------------------
lm_raw <- lm(sleep_duration ~ age + gender +
                              emotional_stability * conscientiousness,
             sleep_emo_con)
p <- plotmod(lm_raw,
              x = "emotional_stability",
              w = "conscientiousness")
library(ggplot2)
p + theme(plot.subtitle = element_blank())

## ----plotmod_levels-----------------------------------------------------------
lm_raw <- lm(sleep_duration ~ age + gender +
                              emotional_stability * conscientiousness,
             sleep_emo_con)
p <- plotmod(lm_raw,
             x = "emotional_stability",
             w = "conscientiousness")
p + theme(plot.caption = element_blank())

## ----plotmod_any_std----------------------------------------------------------
lm_raw <- lm(sleep_duration ~ age + gender +
                              emotional_stability * conscientiousness,
             sleep_emo_con)
lm_std <- std_selected(lm_raw,
                       to_standardize = ~ emotional_stability + conscientiousness)
# Before Version 0.2.6.3 of stdmod, to_center and to_scale need to be used:
# lm_std <- std_selected(lm_raw,
#                        to_center = ~ emotional_stability + conscientiousness,
#                        to_scale = ~ emotional_stability + conscientiousness)

## ----plotmod_std_selected-----------------------------------------------------
plotmod(lm_std,
        x = "emotional_stability",
        w = "conscientiousness")

## ----plotmod_tweak------------------------------------------------------------
lm_raw <- lm(sleep_duration ~ age + gender +
                              emotional_stability * conscientiousness,
             sleep_emo_con)
p <- plotmod(lm_std,
              x = "emotional_stability",
              w = "conscientiousness")
p + scale_color_manual(values = c("blue", "red")) +
    theme_classic()

