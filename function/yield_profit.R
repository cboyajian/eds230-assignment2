
#' Yield Anomaly Profit model
#'
#' @param yield_df dataframe output from yield_model function, must include yield anomaly(tons/acre) as yield_anom column
#' @param price_per_ton price should be in US dollars per acre
#' @param opcost_per_acre operating cost in US dollar per acre for crop 
#'
#' @return
#' @export
#'
#' @examples
yield_profit <- function(yield_df, price_per_ton, opcost_per_acre){
    profit_df <- yield_df %>% 
                  mutate(profit_anom = price_per_ton*yield_df$yield_anom - opcost_per_acre)
    return(list(annual = profit_df[,c("year","profit_anom")], mean = mean(profit_df$profit_anom)))
}