

#' Crop Yield Model Based on Lobell et al. 2006
#' @param climdf climate data frame with daily min temperature, max temperature and precipitation 
#' @param yearforyield year to compute yield for (should single year, not a vector)
#' @param crop crop of interest (choice of almonds and wine grapes currently)
#' @return yield anomaly(tons/acre) for a given year
#' @export
#'
#' @examples 
#' #Almond yield for 1998
#' yield_model(climdf = climdf, yearforyield = 1998, crop = "almond")
#' 
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

  
}

