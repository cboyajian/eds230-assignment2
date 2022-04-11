

#' Title
#'
#' @return
#' @export
#'
#' @examples
yield_model <- function(climdf, yearforyield, crop, coeffvector=NULL){
  if(crop == "almond"){
  #wrangle dataframe  
  climdf_subset <- climdf %>% 
  filter(year==yearforyield) %>% 
  group_by(month) %>% 
  summarize(t_min=mean(tmin_c),t_max=mean(tmax_c),precip_mean=sum(precip))
  #wrangle dataframe
  T_n2_subset <- climdf_subset %>% 
                 filter(month==2)
  T_n2 <- T_n2_subset[[2]]

  P_1_subset <- climdf_subset %>% 
                filter(month==1)
  P_1 <- P_1_subset[[4]]  
  if (is.null(coeffvector)){
  coeffvector = c(-0.015, -0.0046, -0.07, 0.0043, 0.28)
  }
  varvector <-vector() 
  varvector <-varvector %>%  
              append(T_n2) %>% 
              append(T_n2^2) %>% 
              append(varvector, P_1) %>% 
              append(varvector, P_1^2) %>% 
              append(varvector, 1)
  #yield = coeffvector %*% varvector
  #yield = as.numeric(yield)
  yield = coeffvector[1]*T_n2 + coeffvector[2]*T_n2^2 + coeffvector[3]*P_1 + coeffvector[4]*P_1^2 + coeffvector[5]
  }
  else if(crop == "wine grapes"){
    climdf_subset <- climdf %>% 
      filter(year==yearforyield) %>% 
      group_by(month) %>% 
      summarize(t_min=mean(tmin_c),t_max=mean(tmax_c),precip_mean=sum(precip))
    
    T_n4_subset <- climdf_subset %>% 
      filter(month==4)
    T_n4 <- T_n4_subset[[2]]
    
    P_6_subset <- climdf_subset %>% 
      filter(month==6)
    P_6 <- P_6_subset[[4]]
    
    climdf_subset_prev <- climdf %>% 
      filter(year==(yearforyield-1)) %>% 
      group_by(month) %>% 
      summarize(t_min=mean(tmin_c),t_max=mean(tmax_c),precip_mean=sum(precip))
    
    P_9_subset_prev <- climdf_subset_prev %>% 
      filter(month==6)
    P_9_prev <- P_9_subset_prev[[4]]
    
    coeffvector = c(2.65, -0.17, 4.78, -4.93, -2.24, 1.54, -10.50)
    varvector <-vector()
    varvector <-append(varvector, T_n4)
    varvector <-append(varvector, T_n4^2)
    varvector <-append(varvector, P_6)
    varvector <-append(varvector, P_6^2)
    varvector <-append(varvector, P_9_prev)
    varvector <-append(varvector, P_9_prev^2)
    varvector <-append(varvector, 1)
    #yield = coeffvector %*% varvector
    #yield = as.numeric(yield)
  }
  
}

