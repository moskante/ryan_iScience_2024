# Motion angle analysis ------------------------------------------------
library(tidyverse)
load("participants_info_exp1_exp2.RData")
load("final_position_exp1_exp2.RData")

# nonlinear model analysis of motion angle
fit_nls_model <- function(arg_id){
  
  tmp <- final_position %>%
    filter(id == arg_id)%>%
    filter(cot_theta < 40 & cot_theta > -5) #This filters the outliers 
  
  nls_model_fit <- nls(formula = nls_formula, start = list(w_c = 0.3), algorithm = "port", 
                       lower = 0, upper = 1, data = tmp )
  
  return(nls_model_fit)
}

nls_formula <- as.formula(cot_theta ~ cot_theta_des * ((1 - w_c * gain_y)/(1 - w_c * gain_x))) 

# use map to test all participants
nls_model_fit <- map(.x = setNames(participants_info_experiment[["id"]], 
                                   participants_info_experiment[["id"]]),
                     .f = fit_nls_model)

## Compare w_c in Exp. 1 and Exp. 2 (without/with visual feedback) -----
estim_w_c <- map_dfr(.x = nls_model_fit, coefficients, .id = "id") %>%
  inner_join(., y = participants_info_experiment) %>%
  dplyr::select(id, id_name, age, gender, hand_dominance, visual_control, w_c) %>%
  arrange(visual_control)

t.test(w_c ~ visual_control, data = estim_w_c, paired = F)

## Plot observed motion angle (Fig. 5c, 8c) -------

id_for_plot_exp1 <- "20201215efsc" 
id_for_plot_exp2 <- "20210224cory" 

plot_angle <- function(id_for_plot, final_position, nls_model_fit){
  final_position_plot <- final_position %>%
    filter(id == id_for_plot)%>%
    filter(cot_theta < 40 & cot_theta > -5) %>%   #filter outliers 
    mutate(cot_theta_pred = predict(nls_model_fit[[id_for_plot]]))
  
  angle_plot <- ggplot(data = final_position_plot) +
    geom_point(mapping = aes(y = cot_theta, x = cot_theta_des)) +
    geom_smooth(mapping = aes(y = cot_theta_pred, x = cot_theta_des), method = "lm", color = "red") +
    facet_grid(gain_y ~ gain_x)+
    labs(x = expression(paste(plain(cot)  * ( bar(theta) ) ) ), 
         y = expression(paste(plain(cot)  *  (theta))) )
  
  return(angle_plot)
  
}

angle_plot_exp1 <- plot_angle(id_for_plot_exp1, final_position, nls_model_fit)
angle_plot_exp2 <- plot_angle(id_for_plot_exp2, final_position, nls_model_fit)
