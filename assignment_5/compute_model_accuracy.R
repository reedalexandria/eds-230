#' compute_max_flow
#'
#' Computes the correlation between observed max flow and model max flow for peak coho spawning season dec-january. 
#' Compute percent error between observation and model
#' @param  m  model estimates
#' @param  o  observations
#' @param  spawn_month  months to compute max flow for
#' @param  month months
#' @param  day day
#' @param  water_year water year
#' @return metric, correlation between observed and modeled max flow during spawn months
#' multiplied by the corrected rmse between observed and modeled average annual stream flows
#' 
#'




compute_model_accuracy <- function(m,o,month,day,water_year,spawn_month = c(12,1)){

flow = data.frame(cbind(m,o,spawn_month,month,day,water_year))

#aggregate by month and find the max of observations
obs_max <- flow |> 
  filter(month %in% spawn_month) |> 
  group_by(water_year) |> 
  summarize(max_obs = max(o))


#aggregate by month and find the max of the model output
model_max <- flow |>
  filter(month %in% spawn_month) |> 
  group_by(water_year) |> 
  summarize(max_model = max(m))

#find the correlation between observed and model output
correlation <- cor(model_max$max_model,obs_max$max_obs)

#average for the year

#find the yearly mean for each year in observations
flow_average_o <- flow |> 
  group_by(water_year) |> 
  summarize(mean_o = mean(o))

#find the yearly mean for each year
flow_average_m <- flow |> 
  group_by(water_year) |> 
  summarize(mean_m = mean(m))

rmse <- sqrt(mean((flow_average_o$mean_o - flow_average_m$mean_m)^2))

#find the max and min rmse
combined <- flow_average_m |> 
  left_join(flow_average_o) |> 
  group_by(water_year) |> 
  mutate(rmse = sqrt(mean((mean_o - mean_m)^2))) |> 
  ungroup() |> 
  summarize(max_rmse = max(rmse), min_rmse = min(rmse))

#corrected rmse
corrected_rmse <- (combined$max_rmse - rmse) /(combined$max_rmse - combined$min_rmse)

#combine metrics

metric <- corrected_rmse * correlation  

return(metric)
}