source("scripts/preprocessing_exp3.R", echo=FALSE)
axis <- c("x", "y")

# Force plot Figure 9 c-d ---------
final_position_summary <- list(x = NA, y = NA)
barplot_force_by_condition <- list(x = NA, y = NA)

for(i in axis){
  
  final_position_summary[[i]] <- final_position_zscore %>%
    dplyr::filter(axis == i) %>%
    group_by(condition, gain_fct) %>%
    summarise(
      n_trials = n(),
      mean_shear = mean(force_shear_abs, na.rm = T),
      sd_shear = sd(force_shear_abs, na.rm = T)
    ) %>%
    ungroup()
  
  # New facet label names 
  dose.labs <-  c("gel", "oil", "thimble")
  names(dose.labs) <-  c("gel", "oil", "thim")
  
  barplot_force_by_condition[[i]] <-
    ggplot(final_position_summary[[i]]) +
    geom_bar( aes(x= gain_fct, y=mean_shear, fill = gain_fct), stat="identity") +
    geom_errorbar( aes(x=gain_fct, ymin=mean_shear-sd_shear, ymax=mean_shear+sd_shear),
                   width=0.4, alpha=0.9, size= 0.4) +
    facet_wrap( ~ condition, labeller = labeller(condition =dose.labs)) +
    labs(x = "Gain", y = "Force [N]") +
    theme(legend.position = "none")
}

# Force models ---------
lmm_shear_force <- list(x = NA, y = NA)

for(i in axis){
  
  lmm_shear_force[[i]] <- lmerTest::lmer(
    formula = force_shear_abs  ~ gain_fct + condition + (1|id),
    data = dplyr::filter(final_position_zscore, axis == i) )
  
}
names(lmm_shear_force) <- c("x", "y")
lmm_shear_force_summary <- map(lmm_shear_force, .f = summary)
