library(tidyverse)
# Rating experiment ----- 
load("rating_exp3.RData")

## Statistical inference ----------------------
participants_rating_summary <- participants_rating %>%
  group_by(condition) %>%
  summarise(
    mean_rank = mean(rank),
    sd_rank = sd(rank),
    mean_scale = mean(likert_score),
    sd_scale = sd(likert_score),
    age_mean = mean(age),
    age_sd = sd(age),
    count = n(),
    gender_f = sum(gender == "F")
  )

# statistical inference  for rank
ft_rank <-friedman.test(rank ~ condition|id,  data = participants_rating)

# statistical inference for score
ft_score <-friedman.test(likert_score ~ condition|id,  data = participants_rating)

# Plots, Fig. 9 a-b ------------------------------------------------------
rating_plot_rank <- 
  ggplot(data = participants_rating_summary, 
         mapping = aes(x = condition, y = mean_rank)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin = mean_rank - sd_rank,
                    ymax = mean_rank + sd_rank),
                width=.1) +
  labs(y = "Rank", x = "Condition") 

rating_plot_scale <- 
  ggplot(data = participants_rating_summary, 
         mapping = aes(x = condition, y = mean_scale)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin = mean_scale - sd_scale,
                    ymax = mean_scale + sd_scale),
                width=.1) +
  labs(y = "Likert Score", x = "Condition") 

# Motion angle -----
load("participants_info_exp3.RData")
load("final_position_exp3.RData")

# nonlinear model analysis of motion angle
fit_nls_model <- function(arg_id, arg_condition){
  
  tmp <- final_position %>%
    filter(id == arg_id, condition == arg_condition)%>%
    filter(cot_theta < 40 & cot_theta > -5) #This filters the outliers 
  
  nls_model_fit <- nls(formula = nls_formula, start = list(w_c = 0.3), algorithm = "port", 
                       lower = 0, upper = 1, data = tmp )
  
  return(nls_model_fit)
}



nls_formula <- as.formula(cot_theta ~ cot_theta_des * ((1 - w_c * gain_y)/(1 - w_c * gain_x))) 
nls_model_fit <- map2(
  .x = participants_info[["id"]],
  .y = participants_info[["condition"]],
  .f = fit_nls_model)

estim_w_c <- map_dfr(.x = nls_model_fit, coefficients, .id = "id2") %>%
  inner_join(., y = participants_info) %>%
  dplyr::select(id, age, gender, hand_dominance, condition, w_c) %>%
  arrange(condition)


## Statistical inference ----------------------
# gel vs thimble
t.test( w_c ~  condition, paired = TRUE, data = filter(estim_w_c, condition != "oil"))
# gel vs oil
t.test( w_c ~  condition, paired = TRUE, data = filter(estim_w_c, condition != "thim"))
# note: the distribution of wc in thim is different from zero
t.test(x = dplyr::filter(estim_w_c, condition == "thim")[["w_c"]])

## Plot Fig. 9 e ------
boxplot_weight <- estim_w_c %>%
  mutate(condition = factor(condition, levels = c("thim", "gel", "oil")))%>%
  ggplot(mapping = aes(y = w_c, x = condition))+
  geom_boxplot()+
  labs(x = "Condition", y = "Weight Touch") 



