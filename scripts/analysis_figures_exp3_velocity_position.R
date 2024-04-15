source("scripts/preprocessing_exp3.R", echo=FALSE)

# Scatterplots  ------ 

## Speed Fig. 10 a ----
plot_lmm_speed <- ggplot(filter(final_position_zscore_summary, id == "prba"), aes(gain, mean_speed_zscore)) + 
  geom_point() +
  geom_errorbar(aes(ymin = mean_speed_zscore - sd_speed_zscore,
                    ymax = mean_speed_zscore + sd_speed_zscore),
                width=0, position = position_dodge(width = 0.05))+
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  facet_grid(axis~condition) +
  scale_x_continuous(breaks = c(-0.7,0.7), limits = c(-0.8,0.8)) +
  theme(panel.grid.minor = element_blank(), text = element_text(size = 10))+
  xlab("Gain") + ylab("Thimble Absolute Speed [z-score]")

## Position Fig. 10 b ----
plot_lmm_pos <- ggplot(filter(final_position_zscore_summary, id == "prba"), aes(gain, mean_pos_zscore)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_pos_zscore - sd_pos_zscore,
                    ymax = mean_pos_zscore + sd_pos_zscore),
                width=0)+
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  facet_grid(axis~condition) +
  scale_x_continuous(breaks = c(-0.7,0.7), limits = c(-0.8,0.8)) +
  theme(panel.grid.minor = element_blank(), text = element_text(size = 10)) +
  xlab("Gain") + ylab("Thimble Absolute Position [z-score]")

# Models ------

## Speed ----
lmm_speed_fit_by_target    <- map2(.x = c("x", "y"),
                                   .y = c(0,0), # do not filter data by target
                                   .f = lmer_condition,
                                   arg_formula = thimble_speed_peak_zscore ~ condition * gain + (gain|id),
                                   arg_data = final_position_zscore)
names_lmm_abs_speed_by_target <- names(lmm_speed_fit_by_target) <- c("x", "y")
lmm_speed_summary_by_target <- map(lmm_speed_fit_by_target, .f = summary)

## Position ----
lmm_pos_fit_by_target    <- map2(.x =  c("x", "y"),
                                .y = c(0,0), # do not filter data by target
                                .f = lmer_condition,
                                arg_formula = abs(thimble_pos) ~ target_fct +
                                  condition + gain + condition:gain + (gain|id),
                                arg_data = final_position_zscore)
names_lmm_pos_fit_by_target <- names(lmm_pos_fit_by_target) <- c("x", "y")
lmm_pos_summary_by_target <- map(lmm_pos_fit_by_target, .f = summary)

