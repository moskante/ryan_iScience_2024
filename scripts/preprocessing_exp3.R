library(lme4)
library(lmerTest)
library(tidyverse)

load("final_position_exp3.RData")
source("scripts/preprocessing_custom_functions_exp3.R", echo=FALSE)

final_position_axis <- c("x", "y") %>%
  map(.f = pivot_by_axis) %>%
  bind_rows() %>%
  mutate(                                                                       # Remove this line to standardize w resp to participant
    thimble_abs_speed_peak = abs(thimble_speed_peak)
  ) %>%
  dplyr::select(id, condition, block, axis, gain, everything()) 

# compute mean and SD by id, axis, target
summary_by_participant <- final_position_axis %>%                               
  dplyr::filter(condition == "gel") %>% # Standardized with respect to gel 
  group_by(id,  axis, target) %>% 
  summarise(
    speed_mean = mean(thimble_abs_speed_peak), # speed is always positive
    speed_sd = sd(thimble_abs_speed_peak),
    velocity_mean = mean(thimble_speed_peak), # speed (without abs) is the signed velocity
    velocity_sd = sd(thimble_speed_peak),
    acc_mean = mean(thimble_acc_peak), # speed (without abs) is the signed velocity
    acc_sd = sd(thimble_acc_peak),
    position_mean = mean(thimble_pos),
    position_sd = sd(thimble_pos)
  ) %>%
  ungroup()  

# compute the zscore
final_position_zscore <- final_position_axis %>% 
  left_join(summary_by_participant, by = c("id", "axis", "target")) %>% # "condition" id is unique for each experiment
  rowwise() %>% 
  mutate(
    thimble_speed_peak_zscore = (thimble_abs_speed_peak - speed_mean)/speed_sd,
    thimble_pos_zscore = (thimble_pos - position_mean)/position_sd,
    thimble_velocity_peak_zscore = (thimble_speed_peak - velocity_mean)/velocity_sd, # thimble_speed_peak is signed otherwise is abs_speed
    thimble_acc_peak_zscore = (thimble_acc_peak - acc_mean)/acc_sd,
    target_fct = factor(target),
    gain_fct = as.factor(gain),
    force_shear_abs = abs(force_peak)
  ) %>%
  mutate(target_fct = recode(target_fct, `45` = "left", `-45` = "right")) %>%
  mutate(target_fct = fct_relevel(target_fct, "left", "right"),
         gain_fct = fct_relevel(gain_fct, "-0.7", "0.7")) %>%
  dplyr::filter(
    trial_time > 1, # remove shortest trials
    thimble_velocity_peak_zscore < 3, # remove outliers |z-score| > 3
    thimble_velocity_peak_zscore > -3
  )



final_position_zscore_summary <- final_position_zscore %>%
  group_by(id, gain, condition, axis) %>%
  summarize(mean_speed_zscore = mean(thimble_speed_peak_zscore),
            sd_speed_zscore = sd(thimble_speed_peak_zscore),
            mean_pos_zscore = mean(thimble_pos_zscore),
            sd_pos_zscore = sd(thimble_pos_zscore)
  )

