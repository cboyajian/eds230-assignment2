
#' Yield Profit model based on Yield Anomaly
#'
#' @param yield_df data frame output from yield_model function, must include yield anomaly(tons/acre) as yield_anom column
#' @param price_per_ton price should be in US dollars per acre
#' @param opcost_per_acre operating cost in US dollar per acre for crop 
#'
#' @return list with data frame including the computed profit anomaly by year and single mean of profit anomaly across given years
#' @export
#'
#' @examples yld_profdf <- yield_profit(yield_df=climdf, price_per_ton=4000, opcost_per_acre=5000)
yield_profit <- function(yield_df, price_per_ton, opcost_per_acre){
  if(price_per_ton <= 0){
    stop("Price must be greater than 0")
  }
  if(opcost_per_acre <= 0){
    stop("Operating Cost must be greater than 0")
  }
  if(!"yield_anom" %in% colnames(yield_df)){
    stop("Dataframe must contain yield_anom column")
  }
  profit_df <- yield_df %>% 
    mutate(profit_anom = price_per_ton*yield_df$yield_anom - opcost_per_acre)
  return(list(annual = profit_df[,c("year","profit_anom")], mean = mean(profit_df$profit_anom)))
}