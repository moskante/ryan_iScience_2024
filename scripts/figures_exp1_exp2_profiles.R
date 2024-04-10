library(tidyverse)
library(ggpubr)
load("trajectories_exp1_exp2.RData")


# Velocity profile exp. 1 (Figure 4 a-b) ----
id_for_velocity <- "20210111cory"

trajectories_velocity <- trajectories %>%
  dplyr::filter(id == id_for_velocity) %>%
  pivot_longer(cols = c("thimble_speed_x","thimble_speed_y"),
               names_to = "thimble_speed_axis", names_prefix = "thimble_speed_",
               values_to = "thimble_speed")%>%
  mutate(target_fct = factor(target)) %>%
  mutate(target_fct = recode_factor(target_fct, `45` = "left", `0` = "center",`-45` = "right"))

# X axis
speed_by_gain_x <-
  ggplot(data = dplyr::filter(trajectories_velocity, thimble_speed_axis == "x"), 
         mapping = aes(x = trial_time, y = thimble_speed, color = as.factor(gain_x))) +
  # geom_point(size = 0.1, color = "grey", alpha = 0.5)+
  geom_line(mapping = aes(group = trial_index), linewidth = 0.2, color = "grey") +
  geom_smooth() +
  facet_grid(target_fct ~ gain_x) +
  coord_cartesian(xlim = c(0,2), ylim = c(-150, 150)) +
  labs(x = "Time [s]", y = "Velocity x [mm/s]") +
  theme(text = element_text(size = 10), plot.title = element_text( hjust = 0.5), legend.position = "none")

# Y axis
speed_by_gain_y <-
  ggplot(data = dplyr::filter(trajectories_velocity, thimble_speed_axis == "y"), 
         mapping = aes(x = trial_time, y = thimble_speed, color = as.factor(gain_y))) +
  # geom_point(size = 0.1, color = "grey", alpha = 0.5)+
  geom_line(mapping = aes(group = trial_index), linewidth = 0.2, color = "grey") +
  geom_smooth() +
  facet_grid(target_fct ~ gain_y) +
  coord_cartesian(ylim = c(-10, 150), xlim = c(0,2)) +
  labs(x = "Time [s]", y = "Velocity y [mm/s]") +
  theme(text = element_text(size = 10), plot.title = element_text( hjust = 0.5), legend.position = "none")



# Force profile exp.1 (fig. 6) -----

trajectories_force <- trajectories %>%
  pivot_longer(cols = c("force_x","force_y", "force_z"),
               names_to = "force_axis", names_prefix = "force_",
               values_to = "force")%>%
  mutate(target_fct = factor(target)) %>%
  mutate(target_fct = recode_factor(target_fct, `45` = "left", `0` = "center",`-45` = "right"))


smoothed_trajectory_x <- trajectories_force %>%
  group_by(id, force_axis, gain_x, target_fct) %>%
  mutate(smoothed_value = loess(force ~ trial_time)$fitted)

smoothed_trajectory_y <- trajectories_force %>%
  group_by(id, force_axis, gain_y, target_fct) %>%
  mutate(smoothed_value = loess(force ~ trial_time)$fitted)

shear_by_gain <- list(x = NA, y = NA)

shear_by_gain[["x"]] <-
  ggplot(data = dplyr::filter(smoothed_trajectory_x, force_axis == "x"), 
         mapping = aes(x = trial_time, y = smoothed_value, color = as.factor(gain_x))) +
  # geom_point(size = 0.1, color = "grey", alpha = 0.5)+
  geom_line(mapping = aes(group = id), linewidth = 0.2, color = "grey") +
  geom_smooth() +
  facet_grid(target_fct ~ gain_x) +
  coord_cartesian(xlim = c(0,5), ylim = c(-.5,.5)) +
  labs(x = "Time [s]", y = "Force x [N]") +
  theme(text = element_text(size = 10), plot.title = element_text( hjust = 0.5), legend.position = "none")

# Y axis
shear_by_gain[["y"]]  <-
  ggplot(data = dplyr::filter(smoothed_trajectory_y, force_axis == "y"), 
         mapping = aes(x = trial_time, y = smoothed_value, color = as.factor(gain_y))) +
  # geom_point(size = 0.1, color = "grey", alpha = 0.5)+
  geom_line(mapping = aes(group = id), linewidth = 0.2, color = "grey") +
  geom_smooth() +
  facet_grid(target_fct ~ gain_y) +
  coord_cartesian(xlim = c(0,5), ylim = c(-.5,.5)) +
  labs(x = "Time [s]", y = "Force y [N]") +
  theme(text = element_text(size = 10), plot.title = element_text( hjust = 0.5), legend.position = "none")

# Figure 6
figure_shear_by_gain <- ggarrange(shear_by_gain[["x"]], shear_by_gain[["y"]], labels = c("A", "B"), ncol = 1, nrow = 2)

# Position profile (trajectories), exp. 1 (Fig. 5a) and exp.2 (Fig. 8a) ------

load("final_position_exp1_exp2.RData")

id_for_trajectories <- c("20201215efsc", "20210505maco")

trajectories_position <- trajectories %>%
  dplyr::filter(id %in% id_for_trajectories) %>%
  dplyr::filter(trial_time < 5) %>%
  mutate(target_fct = factor(target)) %>%
  mutate(target_fct = recode(target_fct, `45` = "left", `0` = "center", `-45` = "right")) %>%
  mutate(target_fct = relevel(target_fct, "left"))

final_position_for_trajectories <- final_position %>%
  mutate(
    cot_theta_des = target_pos_x/target_pos_y, # this is fine
    cot_theta = thimble_pos_x/thimble_pos_y
  ) %>%
  dplyr::filter(id %in% id_for_trajectories) %>%
  mutate(target_fct = factor(target)) %>%
  mutate(target_fct = recode(target_fct, `45` = "left", `0` = "center", `-45` = "right")) %>%
  mutate(target_fct = relevel(target_fct, "left"))

median_vals <- tibble(
  cot_theta = rep(NA, 54),
  thimble_pos_x = rep(NA, 54),
  thimble_pos_y = rep(NA, 54),
  visual_ctrl = rep(NA,54),
  target_fct = rep(NA, 54),
  gain_x = rep(NA, 54),
  gain_y = rep(NA, 54),
  thimble_pos_x_start = 0,
  thimble_pos_y_start = 0
)

final_position_median <- list()
trajectories_median_trial <- list()
i <- 0

for (this_visual_ctrl in c(0,1)){
  for(this_target in c("left", "center", "right") ) {
    for(this_gain_x in c(-0.7, 0, 0.7)){
      for(this_gain_y in c(-0.7, 0, 0.7)){
        
        i <- i + 1
        
        final_position_median[[i]] <- final_position_for_trajectories %>%
          dplyr::filter( 
            visual_ctrl == this_visual_ctrl,
            gain_x == this_gain_x, 
            gain_y == this_gain_y, 
            target_fct == this_target 
          )
        
        
        median_cot_theta <- median(final_position_median[[i]]$cot_theta)
        
        median_trial_index <- final_position_median[[i]][final_position_median[[i]]$cot_theta == median_cot_theta, 5]$trial_index
        
        trajectories_median <- trajectories_position %>%
          dplyr::filter(visual_ctrl == this_visual_ctrl, gain_x == this_gain_x, gain_y == this_gain_y, target_fct == this_target) 
        
        trajectories_median_trial[[i]] <- 
          trajectories_median[trajectories_median$trial_index == median_trial_index, ]
        
        median_vals[i, 1] <- median(final_position_median[[i]]$cot_theta)
        median_vals[i, 2] <- median(final_position_median[[i]]$thimble_pos_x)
        median_vals[i, 3] <- median(final_position_median[[i]]$thimble_pos_y)
        median_vals[i, 4] <- this_visual_ctrl
        median_vals[i, 5] <- this_target
        median_vals[i, 6] <- this_gain_x
        median_vals[i, 7] <- this_gain_y
        
      }
    }
  }
}

trajectories_median_tibble <- trajectories_median_trial %>%
  bind_rows() 


# Figure 5a
path_by_gain_exp1 <- ggplot(data = filter(trajectories_position, visual_ctrl == 0)) +
  geom_point(mapping = aes(x = target_pos_x, y = target_pos_y, color = target_fct), size = 1) +
  geom_path(mapping = aes(x = thimble_pos_x, y = thimble_pos_y, group = trial_index), linewidth = 0.2, color = "grey") + 
  geom_segment(data = filter(median_vals, visual_ctrl == 0),
               mapping = aes(x = thimble_pos_x_start, xend = thimble_pos_x, 
                             y = thimble_pos_y_start, yend = thimble_pos_y,
                             color = target_fct)) +
  facet_grid(gain_y ~ gain_x) +
  coord_fixed(xlim = c(-50, 50), ylim = c(-10, 70))  +
  labs(x = "Position X [mm]", y = "Position Y [mm]")  +
  scale_y_continuous(breaks=c(0, 25, 50, 75))+
  theme(text = element_text(size = 10), plot.title = element_text(hjust = 0.5), legend.position = "none")

# Figure 8a
path_by_gain_exp2 <- ggplot(data = filter(trajectories_position, visual_ctrl == 1)) +
  geom_point(mapping = aes(x = target_pos_x, y = target_pos_y, color = target_fct), size = 1) +
  geom_path(mapping = aes(x = thimble_pos_x, y = thimble_pos_y, group = trial_index), linewidth = 0.2, color = "grey") + 
  geom_segment(data = filter(median_vals, visual_ctrl == 1),
               mapping = aes(x = thimble_pos_x_start, xend = thimble_pos_x, 
                             y = thimble_pos_y_start, yend = thimble_pos_y,
                             color = target_fct)) +
  facet_grid(gain_y ~ gain_x) +
  coord_fixed(xlim = c(-50, 50), ylim = c(-10, 70))  +
  labs(x = "Position X [mm]", y = "Position Y [mm]")  +
  scale_y_continuous(breaks=c(0, 25, 50, 75))+
  theme(text = element_text(size = 10), plot.title = element_text(hjust = 0.5), legend.position = "none")
