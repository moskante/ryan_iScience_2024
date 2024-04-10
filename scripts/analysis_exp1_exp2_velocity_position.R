source("scripts/preprocessing_exp1_exp2.R", echo = FALSE)
axis_vector <- c("x", "y")
# Models: Velocity (signed speed) ------------------------------------------------------

# Linear mixed model of the relationship between velocity and gain and target 
# position. The summary of these relationships are saved in a list that is for 
# experiment 1 and 2; x and y axis
lmm_stimulus_matrix <- expand_grid(visual_ctrl = c(0,1), axis = axis_vector)

lmm_velocity_fit <- map2(.x = lmm_stimulus_matrix[["axis"]],
                         .y = lmm_stimulus_matrix[["visual_ctrl"]],
                         .f = lmer_axis,
                         arg_formula = thimble_velocity_peak_zscore ~ target_fct * gain + (gain|id_name),
                         arg_data = dplyr::filter(final_position_zscore, thimble_velocity_peak_zscore < 3, # remove outliers |z-score| > 3
                                                  thimble_velocity_peak_zscore > -3))

names(lmm_velocity_fit) <- c("exp1_x", "exp1_y", "exp2_x", "exp2_y")
lmm_velocity_summary <- map(lmm_velocity_fit, .f = summary, .id = names(lmm_velocity_fit))

# Models: Final Position ---------------------------------------------------------------

lmm_pos_fit <- map2(.x = lmm_stimulus_matrix[["axis"]],
                    .y = lmm_stimulus_matrix[["visual_ctrl"]],
                    .f = lmer_axis,
                    arg_formula = thimble_pos ~ target_fct * gain + (gain|id_name),
                    arg_data = final_position_axis)

names(lmm_pos_fit) <- c("exp1_x", "exp1_y", "exp2_x", "exp2_y")
lmm_pos_summary <- map(lmm_pos_fit, .f = summary, .id = names(lmm_pos_fit))

