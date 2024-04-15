source("scripts/preprocessing_exp1_exp2.R", echo = FALSE)

# Scatterplot - velocity (Fig. 4c) -------------------------------
# this prepares data for the scatterplot in figure 4c of manuscript

id_for_scatterplot <- "20210111cory"

summary_participant_velocity <- final_position_zscore %>%
  dplyr::filter(id %in% id_for_scatterplot ) %>% 
  dplyr::filter(trial_time > 1, thimble_velocity_peak_zscore < 3, thimble_velocity_peak_zscore > -3) %>% 
  group_by(id, axis, target_fct, gain) %>%
  summarize(
    thimble_vel_mean_zscore = round(mean(thimble_velocity_peak_zscore), 2),
    thimble_vel_sd_zscore = sd(thimble_velocity_peak_zscore),
    counts = n()
  ) %>%
  ungroup()

scatterplot_velocity <-
    ggplot(data = dplyr::filter(summary_participant_velocity, id == id_for_scatterplot),
           mapping = aes(x = gain, y = thimble_vel_mean_zscore )) +
    geom_errorbar(aes(ymin = thimble_vel_mean_zscore - thimble_vel_sd_zscore,
                      ymax = thimble_vel_mean_zscore + thimble_vel_sd_zscore),
                  width=.1) +
    geom_point(size = 1) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    facet_grid(axis~target_fct) +
    coord_cartesian(ylim = c(-3, 3)) +
    scale_x_continuous(breaks = c(-0.7,0,0.7), limits = c(-0.8,0.8)) +
    labs(x = "Gain", y = "Velocity Peak [z-score]")
  



# Scatterplot - position (Fig. 5b, 8b)-------

# summary for three representative participants
summary_participant_position <- final_position_zscore %>%
  dplyr::filter(id == "20210111cory" | id == "20210224gica" ) %>% 
  dplyr::filter(trial_time > 1 ) %>%
  group_by(id, axis, target, gain) %>%
  summarize(
    thimble_pos_mean = mean(thimble_pos),
    thimble_pos_sd = sd(thimble_pos),
    thimble_pos_mean_zscore = round(mean(thimble_pos_zscore), 2),
    thimble_pos_sd_zscore = sd(thimble_pos_zscore),
    counts = n()
  ) %>%
  ungroup() %>%
  mutate(
    target_fct = factor(target, ordered = TRUE, levels = c("45", "0", "-45"))
  )%>%
  mutate(
    target_fct = recode(target_fct, `45` = "left", `0` = "center", `-45` = "right")
  )

## Scatter for position in Exp 1 (Fig. 5b in manuscript) ----
scatter_final_position_exp1 <-
  ggplot(data = dplyr::filter(summary_participant_position, id =="20210111cory"),
         mapping = aes(x = gain, y = thimble_pos_mean_zscore)) +
  geom_errorbar(aes(ymin = thimble_pos_mean_zscore - thimble_pos_sd_zscore,
                    ymax = thimble_pos_mean_zscore + thimble_pos_sd_zscore),
                width=.1) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  facet_grid(axis~target_fct) +
  scale_x_continuous(breaks = c(-0.7,0,0.7), limits = c(-0.8,0.8)) +
  labs(x = "Gain", y = "Thimble Position [z-score]")

## Scatter for position in Exp 2 (Fig. 8b in manuscript) ----
scatter_final_position_exp2 <-
  ggplot(data = dplyr::filter(summary_participant_position, id =="20210224gica"),
         mapping = aes(x = gain, y = thimble_pos_mean_zscore)) +
  geom_errorbar(aes(ymin = thimble_pos_mean_zscore - thimble_pos_sd_zscore,
                    ymax = thimble_pos_mean_zscore + thimble_pos_sd_zscore),
                width=.1) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  facet_grid(axis~target_fct) +
  scale_x_continuous(breaks = c(-0.7,0,0.7), limits = c(-0.8,0.8)) +
  scale_y_continuous(breaks = c(-2.5,0, 2.5, 5.0), limits = c(-2.5,5.0)) +
  labs(x = "Gain", y = "Thimble Position [z-score]")
