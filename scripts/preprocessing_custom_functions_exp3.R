pivot_by_axis <-function(axis_name){
  
  axis_suffix <- str_c("_", axis_name)
  tmp <- final_position %>%
    dplyr::select(id, condition, block, trial_index, target, trial_time,
                  force_peak_z, ends_with(axis_suffix)) %>%
    mutate(axis = axis_name,
           target_fct = factor(target)) %>%
    mutate(target_fct = recode_factor(target_fct, `-45` = "right", `45` = "left"))
  names(tmp) <- str_remove_all(names(tmp), axis_suffix)
  return(tmp)
}

lmer_condition <- function(arg_axis, arg_target, arg_formula, arg_data, plot = FALSE, response = "pos"){
  
  if(arg_target == 0){
    tmp <- arg_data %>%
      dplyr::filter(axis == arg_axis) 
  }else{
    tmp <- arg_data %>%
      dplyr::filter(axis == arg_axis & target_fct == arg_target) 
  }
  
  
  lmm_fit <- lmerTest::lmer(formula = arg_formula, data = tmp, na.action = "na.omit")
  
  return(lmm_fit)
}

