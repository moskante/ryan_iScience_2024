# Load libraries----------------------------------------------------------------

library(tidyverse)  
library(lme4)

# load data and custom functions -----------
load("final_position_exp1_exp2.RData")
source("scripts/preprocessing_custom_functions_exp1_exp2.R", echo=FALSE)

# Data pre-processing -------------------------------------------------------
# transform to long tibble across all _y and _x
final_position_axis <- c("x", "y") %>%
  map(.f = pivot_by_axis) %>%
  bind_rows() %>%
  dplyr::select(id_name, id, visual_ctrl, axis, gain, everything())

# compute mean and SD by id, axis, target
summary_by_participant <- final_position_axis %>%
  dplyr::filter(gain == "0") %>% # Standardize with respect to gain 0. 
  mutate(thimble_abs_speed_peak = abs(thimble_speed_peak)) %>%
  group_by(id, visual_ctrl, axis, target) %>%
  summarise(
    speed_mean = mean(thimble_abs_speed_peak), # speed is positive
    speed_sd = sd(thimble_abs_speed_peak),
    velocity_mean = mean(thimble_speed_peak), # velocity is signed
    velocity_sd = sd(thimble_speed_peak),
    position_mean = mean(thimble_pos),
    position_sd = sd(thimble_pos)
  ) %>%
  ungroup()  

# compute the zscore
final_position_zscore <- final_position_axis %>% 
  mutate(thimble_abs_speed_peak = abs(thimble_speed_peak)) %>%
  left_join(summary_by_participant, 
            by = c("id","axis", "target", "visual_ctrl")) %>% # id is unique for each experiment
  rowwise() %>% 
  mutate(
    thimble_speed_peak_zscore = (thimble_abs_speed_peak - speed_mean)/speed_sd,
    thimble_pos_zscore = (thimble_pos - position_mean)/position_sd,
    thimble_velocity_peak_zscore = (thimble_speed_peak - velocity_mean)/velocity_sd # thimble_speed_peak is signed otherwise is abs_speed
  ) %>%
  mutate(target_fct = factor(target)) %>%
  mutate(target_fct = recode(target_fct, `45` = "left", `0` = "center", `-45` = "right")) %>%
  mutate(target_fct = fct_relevel(target_fct, "left", "center", "right"))