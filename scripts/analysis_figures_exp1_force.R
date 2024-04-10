source("scripts/preprocessing_exp1_exp2.R", echo = FALSE)

force_threshold <- c(x = 0.25, y = 0.21)
axis <- c("x", "y")

final_position_subset_force <- final_position_zscore %>%
  dplyr::filter(visual_ctrl == 0, 
                abs(tactile_speed_force_peak) < 400
  ) %>% # no visual control and remove faster trials
  mutate(
    gain_fct = as.factor(gain),
    force_shear_abs = abs(force_peak)
  )

final_position_force_threshold <- list(x = NA, y = NA)
final_position_summary <- list(x = NA, y = NA)
scatterplot_velocity_by_shear <- list( x = NA, y = NA)
summary_velocity_by_shear <- list( x = NA, y = NA)
scatterplot_force <- list(x = NA, y = NA)
density_force <- list(x = NA, y = NA)
histogram_force <- list(x = NA, y = NA)
histogram_force_all <- list(x = NA, y = NA)

# Data subsets ------
for(i in axis){
  
  final_position_force_threshold[[i]] <- final_position_subset_force %>%
    dplyr::filter(axis == i, force_shear_abs < force_threshold [[i]]) %>%
    mutate(
      gain_fct = fct_relevel(gain_fct, "0", "0.7", "-0.7")
    )
  
  # Supplementary Table 1, 2
  final_position_summary[[i]] <- final_position_subset_force %>% 
    dplyr::filter(axis == i) %>%
    mutate(force_level = ifelse(force_shear_abs < force_threshold[[i]], "low", "high")) %>%
    group_by(force_level, gain_fct) %>%
    summarise(
      n_trials = n(),
      n_trials_prop = n()/765, 
      mean_shear = mean(force_shear_abs, na.rm = T),
      sd_shear = sd(force_shear_abs, na.rm = T)
    ) %>%
    ungroup()
  
  # Data for Supplementary figure 2
  summary_velocity_by_shear[[i]] <- final_position_subset_force %>%
    dplyr::filter( axis == i) %>% 
    dplyr::filter(trial_time > 1, thimble_velocity_peak_zscore < 3, thimble_velocity_peak_zscore > -3 ) %>% 
    mutate(force_level = ifelse(force_shear_abs < force_threshold[[i]], "low", "high")) %>%
    group_by(force_level, target_fct, gain)  %>%
    summarize(
      thimble_vel_mean_zscore = round(mean(thimble_velocity_peak_zscore), 2),
      thimble_vel_sd_zscore = sd(thimble_velocity_peak_zscore),
      counts = n()
    ) %>%
    ungroup()

  
}

# Force - analysis -------------

## Shear force in subset data ----- 

lmm_shear_by_speed <- list(x = NA, y = NA)
lmm_shear_force <- list(x = NA, y = NA)


for(i in axis){
  
  # inference - shear force by predictors
  lmm_shear_by_speed[[i]] <- lmerTest::lmer(
    formula = force_shear_abs  ~ abs(tactile_speed_force_peak) * gain_fct + force_peak_z + (gain_fct|id_name),
    data = dplyr::filter(final_position_subset_force, axis == i )
  )
  
  # inference - shear force by predictors
  lmm_shear_force[[i]] <- lmerTest::lmer(
    formula = force_shear_abs  ~ gain_fct + (1|id_name),
    data = final_position_force_threshold[[i]]
  )
}

lmm_shear_subset_summary <- map(lmm_shear_force, .f = summary, .id = names(lmm_shear_force))

## Velocity by gain in subset data -----

lmer_axis_force_low <- function(arg_axis, arg_shear, arg_formula, arg_data,
                            plot = FALSE, response = "pos"){
  
  tmp <- arg_data %>%
    dplyr::filter(axis == arg_axis, force_shear_abs < arg_shear) %>%
    mutate(target_fct = relevel(target_fct, "center"))
  
  lmm_fit <- lmerTest::lmer(formula = arg_formula, data = tmp)
  
  return(lmm_fit)
}

lmer_axis_force_high <- function(arg_axis, arg_shear, arg_formula, arg_data,
                                plot = FALSE, response = "pos"){
  
  tmp <- arg_data %>%
    dplyr::filter(axis == arg_axis, force_shear_abs >=  arg_shear) %>%
    mutate(target_fct = relevel(target_fct, "center"))
  
  lmm_fit <- lmerTest::lmer(formula = arg_formula, data = tmp)
  
  return(lmm_fit)
}

lmm_stimulus_matrix_2 <- tibble(
  shear_threshold = force_threshold,
  axis = axis
)
names_lmm <- c("exp1_x", "exp1_y")

# Subset - low force
lmm_velocity_fit_by_shear_low <- map2(.x = lmm_stimulus_matrix_2[["axis"]],
                                  .y = lmm_stimulus_matrix_2[["shear_threshold"]],
                                  .f = lmer_axis_force_low,
                                  arg_formula = thimble_velocity_peak_zscore ~ target_fct * gain + (gain|id_name),
                                  arg_data = dplyr::filter(final_position_subset_force,
                                                           thimble_velocity_peak_zscore < 3, # remove outliers |z-score| > 3
                                                           thimble_velocity_peak_zscore > -3))
names(lmm_velocity_fit_by_shear_low) <- names_lmm
# Supplementary tables 3,4
lmm_velocity_summary_by_shear_low <- map(lmm_velocity_fit_by_shear_low, .f = summary, .id = names_lmm)

# Subset - high force
lmm_velocity_fit_by_shear_high <- map2(.x = lmm_stimulus_matrix_2[["axis"]],
                                  .y = lmm_stimulus_matrix_2[["shear_threshold"]],
                                  .f = lmer_axis_force_high,
                                  arg_formula = thimble_velocity_peak_zscore ~ target_fct * gain + (gain|id_name),
                                  arg_data = dplyr::filter(final_position_subset_force,
                                                           thimble_velocity_peak_zscore < 3, # remove outliers |z-score| > 3
                                                           thimble_velocity_peak_zscore > -3))
names(lmm_velocity_fit_by_shear_high) <- names_lmm

# Supplementary tables 5,6
lmm_velocity_summary_by_shear_high <- map(lmm_velocity_fit_by_shear_high, .f = summary, .id = names(lmm_velocity_fit_by_shear_high))


## Position by gain in subset data -----
# Subset - low force
lmm_pos_fit_by_shear_low <- map2(.x = lmm_stimulus_matrix_2[["axis"]],
                             .y = lmm_stimulus_matrix_2[["shear_threshold"]],
                             .f = lmer_axis_force_low,
                             arg_formula = thimble_pos ~ target_fct * gain + (gain|id_name),
                             arg_data = final_position_subset_force)
names(lmm_pos_fit_by_shear_low) <- names_lmm
# Supplementary tables 7,8
lmm_pos_summary_by_shear_low <- map(lmm_pos_fit_by_shear_low, .f = summary, .id = names(lmm_pos_fit_by_shear_low))

# Subset - high force
lmm_pos_fit_by_shear_high <- map2(.x = lmm_stimulus_matrix_2[["axis"]],
                                 .y = lmm_stimulus_matrix_2[["shear_threshold"]],
                                 .f = lmer_axis_force_high,
                                 arg_formula = thimble_pos ~ target_fct * gain + (gain|id_name),
                                 arg_data = final_position_subset_force)
names(lmm_pos_fit_by_shear_high) <- names_lmm
# Supplementary tables 9,10
lmm_pos_summary_by_shear_high <- map(lmm_pos_fit_by_shear_high, .f = summary, .id = names(lmm_pos_fit_by_shear_high))

# Force - figures -------------

for(i in axis){
  
  scatterplot_force[[i]] <-
    ggplot(data = dplyr::filter(final_position_subset_force, axis == i),
           mapping = aes(y = force_shear_abs, x = abs(tactile_speed_force_peak),
                         color =  gain_fct )) + #   force_peak_z 
    geom_point(size = 1) +
    # geom_smooth(method = "lm") +
    geom_hline(yintercept = force_threshold [[i]])+
    coord_cartesian(xlim = c(0,400), ylim = c(0,1))+
    #facet_wrap( ~ id_name ) +
    facet_wrap( ~ gain_fct ) +
    scale_x_continuous(breaks = seq(0, 300, by = 150))+
    labs(y = "|Force Peak| [N]", x = "Tactile Speed [mm/s]") +
    theme(legend.position = "null", text = element_text(size = 12))
  
  density_force[[i]] <- 
    ggplot(data = dplyr::filter(final_position_subset_force, 
                                axis == i, force_shear_abs < force_threshold [[i]]),
           mapping = aes(x = force_shear_abs, color = gain_fct)) + # , color = as.factor(id)
    geom_density(linewidth = 1)  +
    labs(title = "Force Peak", x = "Force [N]", y = "Density")  +
    theme(text = element_text(size = 10),
          plot.title = element_text(hjust = 0.5),
          legend.position = "none")
  
  # Supplementary Figure 1
  histogram_force[[i]] <- 
    ggplot(data = dplyr::filter(final_position_subset_force, 
                                axis == i, force_shear_abs < force_threshold [[i]]),
           mapping = aes(x = force_shear_abs, after_stat(density), color = gain_fct)) + # , color = as.factor(id)
    geom_freqpoly()+
    labs(x = "Force [N]", y = "Density")  +
    theme(text = element_text(size = 10),
          plot.title = element_text(hjust = 0.5),
          legend.position = "none")
  
  # Figure 7
  histogram_force_all[[i]] <- 
    ggplot(data = dplyr::filter(final_position_subset_force, axis == i),
           mapping = aes(x = force_shear_abs, after_stat(density), color = gain_fct)) + # , color = as.factor(id)
    geom_freqpoly()+
    labs(title = "Force Peak", x = "Force [N]", y = "Density")  +
    theme(text = element_text(size = 10),
          plot.title = element_text(hjust = 0.5),
          legend.position = "none")
  
  # Supplementary figure 2
  scatterplot_velocity_by_shear[[i]] <- 
    ggplot(data = summary_velocity_by_shear[[i]],
           mapping = aes(x = gain, y = thimble_vel_mean_zscore, color = force_level )) +
    geom_errorbar(aes(ymin = thimble_vel_mean_zscore - thimble_vel_sd_zscore,
                      ymax = thimble_vel_mean_zscore + thimble_vel_sd_zscore),
                  width=.1) +
    geom_point(size = 1) +
    geom_smooth(method = "lm", se = FALSE) +
    facet_grid(force_level~target_fct) +
    coord_cartesian(ylim = c(-2, 2)) +
    scale_x_continuous(breaks = c(-0.7,0,0.7), limits = c(-0.8,0.8)) +
    theme( legend.position = "none") +
    labs(x = "Gain", y = "Velocity Peak [z-score]")
  
}

lmm_pos_barplot_by_shear_ranef <- map2(.x = lmm_pos_fit_by_shear_ranef,
                                       .y = c("exp1_x", "exp1_y"),
                                       .f = barplot_lmm_vel_pos,
                                       velocity = FALSE)

## Barplots - models ------

### Velocity, low force subset -----
lmm_velocity_fit_by_shear_low_ranef <- map2(.x = lmm_stimulus_matrix_2[["axis"]],
                                      .y = lmm_stimulus_matrix_2[["shear_threshold"]],
                                      .f = lmer_axis_force_low,
                                      arg_formula = thimble_velocity_peak_zscore ~ target_fct * gain + (target_fct * gain|id_name),
                                      arg_data = dplyr::filter(final_position_subset_force,
                                                               thimble_velocity_peak_zscore < 3, # remove outliers |z-score| > 3
                                                               thimble_velocity_peak_zscore > -3))
# Supplementary Figures 3,4
lmm_vel_barplot_by_shear_ranef <- map2(.x = lmm_velocity_fit_by_shear_low_ranef,
                                       .y = c("exp1_x", "exp1_y"),
                                       .f = barplot_lmm_vel_pos,
                                       velocity = TRUE)

### Position, low force subset ----
lmm_pos_fit_by_shear_low_ranef <- map2(.x = lmm_stimulus_matrix_2[["axis"]],
                                 .y = lmm_stimulus_matrix_2[["shear_threshold"]],
                                 .f = lmer_axis_force_low,
                                 arg_formula = thimble_pos ~ target_fct * gain + (target_fct * gain|id_name),
                                 arg_data = final_position_subset_force)

# Supplementary Figures 5,6
lmm_pos_barplot_by_shear_ranef <- map2(.x = lmm_pos_fit_by_shear_low_ranef,
                                       .y = c("exp1_x", "exp1_y"),
                                       .f = barplot_lmm_vel_pos,
                                       velocity = FALSE)

