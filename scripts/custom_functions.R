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

# nonlinear model analysis of motion angle
fit_nls_model <- function(arg_id, do_plot = FALSE){
  
  tmp <- final_position %>%
    filter(id == arg_id)%>%
    filter(cot_theta < 40 & cot_theta > -5) #This filters the outliers 
  
  nls_model_fit <- nls(formula = nls_formula, start = list(w_c = 0.3), algorithm = "port", 
                       lower = 0, upper = 1, data = tmp )
  
  # estimate of w_c and prediction (plot)
  tmp <- tmp %>%
    mutate(cot_theta_pred = predict(nls_model_fit))
  
  if(do_plot == TRUE){
    
    fig <- ggplot(data = tmp) +
      geom_point(mapping = aes(y = cot_theta, x = cot_theta_des)) +
      geom_smooth(mapping = aes(y = cot_theta_pred, x = cot_theta_des), method = "lm", color = "red") +
      facet_grid(gain_y ~ gain_x)+
      #scale_x_continuous(breaks = c(-1.0, 0.0, 1.0)) +
      #scale_y_continuous(breaks = c(-1.0, 0.0, 1.0)) +
      labs(x = expression(paste(plain(cot)  * ( bar(theta) ) ) ), 
           y = expression(paste(plain(cot)  *  (theta))) )
    
    
    
    file_id <- str_c(arg_id, "_angle_figure.png")
    
    ggsave(filename = file_id, plot = fig, path = "figures/figures_angle",
           width = width_small, height = 70 * mm2inch)
    
    #ggsave(filename = file_id, plot = fig, path = "figures/figures_angle",
    #width = width_medium, height = 70 * mm2inch)
  }
  
  return(nls_model_fit)
}

