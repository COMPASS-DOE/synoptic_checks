# install.packages("weathR")
library(weathR)
library(dplyr)
library(sf)
library(ggplot2)

# We can fetch forecast temperatures (in degrees fahrenheit) for NYC

point_forecast(lat = 38.8747, lon = -76.5519) %>% ##lat long for TEMPEST control plot
  as.data.frame() %>% 
  mutate(time = as.POSIXct(time)) %>% #convert time to a POSIXct object 
  ggplot(aes(x = time, y = temp)) +
  #facet by data type 
  #Add points for forecast values of temperature
  geom_point(color = "brown") +
  #Add a smoothed line that follows the points
  geom_smooth(method = "loess", span = .15, se = FALSE, color = "indianred") +
  labs(
    title = paste0("Temperature Forecasts for the Week of ", Sys.Date()),
    y = "Temperature (Degrees Fahrenheit)",
    x = "Day"
  ) +
  theme_minimal()


point_forecast(lat = 38.8747, lon = -76.5519) %>% ##lat long for TEMPEST control plot
  as.data.frame() %>% 
  mutate(time = as.POSIXct(time)) %>% #convert time to a POSIXct object 
  ggplot(aes(x = time, y = p_rain)) +
  #facet by data type 
  #Add points for forecast values of temperature
  geom_point(color = "brown") +
  #Add a smoothed line that follows the points
  geom_smooth(method = "loess", span = .15, se = FALSE, color = "indianred") +
  labs(
    title = paste0("Rain Percent Chance for the Week of ", Sys.Date()),
    y = "Precipitation Potential (%)",
    x = "Day"
  ) +
  theme_minimal()
