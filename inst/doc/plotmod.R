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
lm_raw <- lm(sleep_duration ~ age + gender +
                              emotional_stability * conscientiousness,
             sleep_emo_con)
plotmod(lm_raw,
        x = "emotional_stability",
        w = "conscientiousness",
        x_label = "Emotional Stability",
        w_label = "Conscientiousness",
        y_label = "Sleep Duration")

## ----echo = FALSE-------------------------------------------------------------
plotmod(lm_raw,
        x = "emotional_stability",
        w = "conscientiousness",
        x_label = "Emotional Stability",
        w_label = "Conscientiousness",
        y_label = "Sleep Duration",
        graph_type = "tumble")

## -----------------------------------------------------------------------------
library(stdmod)
data(sleep_emo_con)
lm_raw <- lm(sleep_duration ~ age + gender +
                              emotional_stability * conscientiousness,
             sleep_emo_con)
plotmod(lm_raw,
        x = "emotional_stability",
        w = "conscientiousness")

## -----------------------------------------------------------------------------
lm_raw <- lm(sleep_duration ~ age + gender +
                              emotional_stability * conscientiousness,
             sleep_emo_con)
plotmod(lm_raw,
        x = "emotional_stability",
        w = "conscientiousness",
        w_method = "percentile")

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
lm_raw <- lm(sleep_duration ~ age + gender +
                              emotional_stability * conscientiousness,
             sleep_emo_con)
plotmod(lm_raw,
        x = "emotional_stability",
        w = "conscientiousness",
        graph_type = "tumble")

## -----------------------------------------------------------------------------
lm_raw <- lm(sleep_duration ~ age + gender +
                              emotional_stability * conscientiousness,
             sleep_emo_con)
plotmod(lm_raw,
        x = "emotional_stability",
        w = "conscientiousness",
        x_label = "EMO",
        w_label = "CON",
        y_label = "SLEEP")

## -----------------------------------------------------------------------------
lm_raw <- lm(sleep_duration ~ age + gender +
                              emotional_stability * conscientiousness,
             sleep_emo_con)
plotmod(lm_raw,
        x = "emotional_stability",
        w = "conscientiousness",
        title = "EMO Effects For Low/High CON")

## -----------------------------------------------------------------------------
lm_raw <- lm(sleep_duration ~ age + gender +
                              emotional_stability * conscientiousness,
             sleep_emo_con)
plotmod(lm_raw,
        x = "emotional_stability",
        w = "conscientiousness",
        point_size = 8,
        line_width = 2)

## -----------------------------------------------------------------------------
lm_raw <- lm(sleep_duration ~ age + gender +
                              emotional_stability * conscientiousness,
             sleep_emo_con)
p <- plotmod(lm_raw,
              x = "emotional_stability",
              w = "conscientiousness")
library(ggplot2)
p + theme(plot.subtitle = element_blank())

## -----------------------------------------------------------------------------
lm_raw <- lm(sleep_duration ~ age + gender +
                              emotional_stability * conscientiousness,
             sleep_emo_con)
p <- plotmod(lm_raw,
             x = "emotional_stability",
             w = "conscientiousness")
p + theme(plot.caption = element_blank())

## -----------------------------------------------------------------------------
lm_raw <- lm(sleep_duration ~ age + gender +
                              emotional_stability * conscientiousness,
             sleep_emo_con)
lm_std <- std_selected(lm_raw,
                       to_center = ~ emotional_stability + conscientiousness,
                       to_scale = ~ emotional_stability + conscientiousness)
plotmod(lm_std,
        x = "emotional_stability",
        w = "conscientiousness")

## -----------------------------------------------------------------------------
lm_raw <- lm(sleep_duration ~ age + gender +
                              emotional_stability * conscientiousness,
             sleep_emo_con)
p <- plotmod(lm_std,
              x = "emotional_stability",
              w = "conscientiousness")
p + scale_color_manual(values = c("blue", "red")) +
    theme_classic()

