#1. transform to long tibble across all _y and _x
pivot_by_axis <-function(axis_name){
  
  axis_suffix <- str_c("_", axis_name)
  tmp <- final_position %>%
    dplyr::select(id_name, id, visual_ctrl, trial_index, target, trial_time, force_peak_z,
                  ends_with(axis_suffix)) %>%
    mutate(axis = axis_name,
           target_fct = factor(target)) %>%
    mutate(target_fct = recode_factor(target_fct, `0` = "center", `-45` = "right", `45` = "left"))
  names(tmp) <- str_remove_all(names(tmp), axis_suffix)
  return(tmp)
}

#2. run lmm by axis
lmer_axis <- function(arg_axis, arg_vision, arg_formula, arg_data, plot = FALSE, response = "pos"){
  
  tmp <- arg_data %>%
    dplyr::filter(axis == arg_axis & visual_ctrl == arg_vision) %>%
    mutate(target_fct = relevel(target_fct, "center"))
  
  lmm_fit <- lmerTest::lmer(formula = arg_formula, data = tmp)
  
  return(lmm_fit)
}

# make barplots with random-effect slopes
barplot_lmm_vel_pos <- function(lmm_fit, model_name, velocity = TRUE ){
  
  cluster.median = t(apply(X = lme4::ranef(lmm_fit)[[1]], MARGIN = 1, FUN = function(x){x + lme4::fixef(lmm_fit)}))
  cluster.median = rbind(cluster.median, lme4::fixef(lmm_fit))
  n <- nrow(cluster.median)
  rownames(cluster.median)[n] = c("Group")
  
  curr_axis <- str_split(model_name, "_")[[1]][2]
  
  slope_for_barplot <- tibble(
    id = as.factor(rownames(cluster.median)),
    target_center = cluster.median[,"gain"],
    target_right = cluster.median[,"gain"] + cluster.median[,"target_fctright:gain"],
    target_left = cluster.median[,"gain"] + cluster.median[,"target_fctleft:gain"]
  ) %>%
    pivot_longer(
      cols = c("target_center", "target_right", "target_left"),
      names_to = "target",
      names_prefix = "target_",
      values_to = "slope"
    ) %>%
    mutate(
      target = as.factor(target),
      id = relevel(id, "Group")
    )%>%
    mutate(
      target = relevel(target, ref = "left")
    )
  
  if(velocity == TRUE){
    lower_lim = list(x = -0.6, y = -0.1)
    upper_lim = list(x = 1, y = 1)
  }else{
    lower_lim = list(x = -25, y = -5)
    upper_lim = list(x = 35, y = 35)
  }
  
  barplot_slope <-
    ggplot(data=slope_for_barplot, 
           aes(x= id, y=slope, fill=target)) +
    geom_bar(stat="identity", position=position_dodge()) +
    geom_hline(
      data = dplyr::filter(slope_for_barplot, id == "Group"),
      aes(yintercept = slope)) +
    facet_wrap(~ target) +
    labs(x = "Participant", y = "LMM slope", fill = "Target") +
    coord_cartesian(ylim = c(lower_lim[[curr_axis]], upper_lim[[curr_axis]] )) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position = "none")
  
  # change here
  # arg_filename <- str_c("figures/Barplot_", response, "_", arg_axis, "_slope.svg")
  # 
  # svg(arg_filename , width = 8.0, height = 4.2)
  # print(barplot_slope)
  # dev.off()
  
  return(barplot_slope)
}