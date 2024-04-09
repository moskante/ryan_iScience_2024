# Load libraries----------------------------------------------------------------

library(tidyverse)  
library(lme4)

load("final_position_exp1_exp2.RData")
source("scripts/custom_functions.R", echo=TRUE)
# Data pre-processing -------------------------------------------------------

# cotangent of angle theta
final_position <- final_position %>%
  mutate(
    cot_theta_des = target_pos_x/target_pos_y,
    cot_theta = thimble_pos_x/thimble_pos_y
  ) 

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

# Models: Velocity (signed speed) ------------------------------------------------------

# Linear mixed model of the relationship between velocity and gain and target 
# position. The summary of these relationships are saved in a list that is for 
# experiment 1 and 2; x and y axis
axis_vector <- c("x", "y")
lmm_stimulus_matrix <- expand_grid(visual_ctrl = c(0,1), axis = axis_vector)

lmm_velocity_fit <- map2(.x = lmm_stimulus_matrix[["axis"]],
                         .y = lmm_stimulus_matrix[["visual_ctrl"]],
                         .f = lmer_axis,
                         arg_formula = thimble_velocity_peak_zscore ~ target_fct * gain + (gain|id_name),
                         arg_data = dplyr::filter(final_position_zscore, thimble_velocity_peak_zscore < 3, # remove outliers |z-score| > 3
                                                  thimble_velocity_peak_zscore > -3))
names_lmm_speed <- names(lmm_velocity_fit) <- c("exp1_x", "exp1_y", "exp2_x", "exp2_y")
lmm_velocity_summary <- map(lmm_velocity_fit, .f = summary, .id = names(lmm_velocity_fit))

# Models: Final Position ---------------------------------------------------------------

lmm_pos_fit <- map2(.x = lmm_stimulus_matrix[["axis"]],
                    .y = lmm_stimulus_matrix[["visual_ctrl"]],
                    .f = lmer_axis,
                    arg_formula = thimble_pos ~ target_fct * gain + (gain|id_name),
                    arg_data = final_position_axis)
names(lmm_pos_fit) <- c("exp1_x", "exp1_y", "exp2_x", "exp2_y")

lmm_pos_summary <- map(lmm_pos_fit, .f = summary, .id = names(lmm_pos_fit))

