library(tidyverse)
load("trajectories_exp3.RData")
load("final_position_exp3.RData")

# Figure 10 c -------
id_for_plot <- "prba"
conditions <- c("gel", "oil", "thim")

figure_panel <- list()

time_min <- 1

final_position_median <- list()
trajectories_median_trial <- list()

for (this_condition in conditions){
  final_position_id <- final_position %>%
    dplyr::filter(
      id %in% id_for_plot,
      condition == this_condition,
      trial_time > time_min
    )
  
  trajectories_id <- trajectories %>%
    dplyr::filter(id %in% id_for_plot,
                  condition == this_condition,
                  trial_index %in% final_position_id$trial_index ) %>%
    mutate(target_fct = factor(target)) %>%
    mutate(target_fct = recode(target_fct, `45` = "left", `-45` = "right")) %>%
    mutate(target_fct = relevel(target_fct, "left"))
  
  
  n_median_vals <- 8
  
  median_vals <- tibble(
    cot_theta = rep(NA, n_median_vals),
    thimble_pos_x = rep(NA, n_median_vals),
    thimble_pos_y = rep(NA, n_median_vals),
    target_fct = rep(NA, n_median_vals),
    gain_x = rep(NA, n_median_vals),
    gain_y = rep(NA, n_median_vals),
    thimble_pos_x_start = 0,
    thimble_pos_y_start = 0,
    condition = rep("null", n_median_vals),
    id = id_for_plot
  )
  
  i <- 0
  
  for(this_target in c("left", "right") ) {
    for(this_gain_x in c(-0.7, 0.7)){
      for(this_gain_y in c(-0.7, 0.7)){
        
        i <- i + 1
        
        final_position_median[[i]] <- final_position_id %>%
          dplyr::filter(
            gain_x == this_gain_x, 
            gain_y == this_gain_y, 
            target_fct == this_target 
          )
        
        median_cot_theta <- median(final_position_median[[i]]$cot_theta)
        
        if (median_cot_theta %in% final_position_median[[i]]$cot_theta){
          # if odd take the trial of the median angle
          median_trial_index <- final_position_median[[i]][final_position_median[[i]]$cot_theta == median_cot_theta, "trial_index"]$trial_index
        }else{
          # if even number of trials use the smaller closer to the median
          median_trial_index <- which( (final_position_median[[i]]$cot_theta - median_cot_theta)  == min( final_position_median[[i]]$cot_theta -  median_cot_theta ))
        }
        
        
        median_vals[i, 1] <- median(final_position_median[[i]]$cot_theta)
        median_vals[i, 2] <- median(final_position_median[[i]]$thimble_pos_x)
        median_vals[i, 3] <- median(final_position_median[[i]]$thimble_pos_y)
        median_vals[i, 4] <- this_target
        median_vals[i, 5] <- this_gain_x
        median_vals[i, 6] <- this_gain_y
        median_vals[i, 9] <- this_condition
      }
    }
  }
  
  # Figure 
  figure_panel[[this_condition]] <- ggplot(data = trajectories_id[trajectories_id$condition == this_condition,] ) +
    geom_point(mapping = aes(x = target_pos_x, y = target_pos_y, color = target_fct), size = 1) +
    geom_path(mapping = aes(x = thimble_pos_x, y = thimble_pos_y, group = trial_index), linewidth = 0.2, color = "grey") +
    geom_segment(data =  median_vals,
                 mapping = aes(x = thimble_pos_x_start, xend = thimble_pos_x,
                               y = thimble_pos_y_start, yend = thimble_pos_y,
                               color = target_fct)) +
    facet_grid(gain_y ~ gain_x) +
    coord_fixed(xlim = c(-50, 50), ylim = c(-10, 70))  +
    labs(x = "Position X [mm]", y = "Position Y [mm]")  +
    scale_y_continuous(breaks=c(0, 25, 50, 75))+
    theme(text = element_text(size = 8), plot.title = element_text(hjust = 0.5), legend.position = "none")
}

