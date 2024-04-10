source("scripts/preprocessing_exp1_exp2.R", echo = FALSE)
axis_vector <- c("x", "y")
lmm_stimulus_matrix <- expand_grid(visual_ctrl = c(0,1), axis = axis_vector)

### Velocity, all data -----
lmm_velocity_fit_ranef <- map2(.x = lmm_stimulus_matrix[["axis"]],
                         .y = lmm_stimulus_matrix[["visual_ctrl"]],
                         .f = lmer_axis,
                         arg_formula = thimble_velocity_peak_zscore ~ target_fct * gain + (target_fct *gain|id_name),
                         arg_data = dplyr::filter(final_position_zscore, thimble_velocity_peak_zscore < 3, # remove outliers |z-score| > 3
                                                  thimble_velocity_peak_zscore > -3))
# Supplementary Figures 7,8,11,12
lmm_vel_barplot_ranef <- map2(.x = lmm_velocity_fit_ranef,
                                       .y = c("exp1_x", "exp1_y", "exp2_x", "exp2_y"),
                                       .f = barplot_lmm_vel_pos,
                                       velocity = TRUE)

### Position, all data ----
lmm_pos_fit_ranef <- map2(.x = lmm_stimulus_matrix[["axis"]],
                          .y = lmm_stimulus_matrix[["visual_ctrl"]],
                          .f = lmer_axis,
                          arg_formula = thimble_pos ~ target_fct * gain + (target_fct * gain|id_name),
                          arg_data = final_position_axis)

# Supplementary Figures 9,10,13,14
lmm_vel_barplot_ranef <- map2(.x = lmm_pos_fit_ranef,
                              .y = c("exp1_x", "exp1_y", "exp2_x", "exp2_y"),
                              .f = barplot_lmm_vel_pos,
                              velocity = FALSE)
