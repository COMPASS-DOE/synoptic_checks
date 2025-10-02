## This script does the same thing as synoptic_v2.R but for TEMPEST trolls
##
## Peter Regier
## 2022-04-06
##
# ############## #
# ############## #

# 1. Setup ---------------------------------------------------------------------

## Load packages with pacman::p_load
require(pacman)
p_load(tidyverse,
       janitor,
       parsedate,
       lubridate,
       cowplot, 
       purrr,
       readr, 
       googledrive, 
       PNWColors,
       googlesheets4)

# Set ggplot theme
theme_set(theme_bw())

current_directory = "/Users/regi350/Dropbox/TEMPEST_PNNL_Data/Current_data"
archive_directory = "/Users/regi350/Dropbox/TEMPEST_PNNL_Data/Loggernet_Rawdata_Archive"

month_strings <- c(str_sub(str_replace_all(Sys.Date(), "-", ""), 1, 6), 
                   str_sub(str_replace_all(Sys.Date() - months(1), "-", ""), 1, 6))


# 2. Read in data --------------------------------------------------------------

## This function reads in and cleans up each file
read_data <- function(data){
  
  filename <- str_replace(data, "^.*/", "")
  logger <- str_split(filename, "_", simplify = T)[,2]
  sensor <- str_split(filename, "_", simplify = T)[,3]
  #resolution <- str_split(filename, "_", simplify = T)[,4]

  read_delim(file = data, skip = 1) %>% 
    slice(3:n()) %>% 
    clean_names() %>% 
    mutate(datetime = parsedate::parse_date(timestamp)) %>% 
    filter(datetime > "2022-03-01") %>% 
    mutate_at(vars(contains("600")), as.numeric) %>% 
    rename_with(~str_remove(., '600')) %>% 
    #rename_with(~str_remove(., '600[a-z]')) #%>% 
    rename("pressure_psi" = pressure) %>% 
    mutate(pressure_mbar = pressure_psi * 68.948) %>% 
    mutate(logger = logger,
           sensor = sensor) %>%
    dplyr::select(datetime, logger, statname,
                  temperature, salinity, rdo_concen, p_h, p_h_orp,
                  depth, water_density, pressure_mbar, pressure_psi,
                  voltage_ext, battery_int)
}

current_raw <- list.files(current_directory, full.names = T)[grepl("WaterLevel600", list.files(current_directory, full.names = T))] %>% 
  map(read_data) %>%
  bind_rows()

#df_raw <- bind_rows(archive_filtered, current_raw)

df_raw <- current_raw

## Read in data and bind to a single dataframe
# archive_raw <- list.files(raw_data_path, full.names = T) %>% 
#   map(read_data) %>% 
#   bind_rows()
#archive_raw <- list.files(archive_directory, full.names = T)[grepl("WaterLevel", list.files(archive_directory, full.names = T))]

# Combine month strings into a single regex pattern
#month_pattern <- paste(month_strings, collapse = "|")

# Filter the water level files to include only those that match the month strings
# archive_filtered <- archive_raw %>%
#   keep(~ grepl(month_pattern, .x)) %>% 
#   map(read_data) %>%
#   bind_rows()

#x <- str_replace(archive_filtered[1], "^.*/", "")
#str_split(x, "_", simplify = T)[,2]


# 4. Clean data (this is not QC) -----------------------------------------------

well_dimensions <- read_csv("/Users/regi350/OneDrive - PNNL/Documents/projects/compass/synoptic/data/TEMPEST_aquatroll_inventory.csv") %>% 
  clean_names() %>% 
  filter(well_name %in% c("GW9", "GW10", "GW11")) %>% 
  filter(instrument == "TROLL600") %>% 
  dplyr::select(logger_id, well_name, plot, contains("bolt_to"), contains("belowground"), elevation)
  
df_raw_depths <- left_join(df_raw, well_dimensions, by = c("statname" = "logger_id")) %>% 
  filter(!is.na(pressure_mbar))

df <- df_raw_depths %>% 
  mutate(density_gcm3_cor = ifelse(water_density >= 0.98 & water_density <= 1.05, water_density, 1), 
         pressurehead_m = (pressure_mbar * 100) / (density_gcm3_cor * 1000 * 9.80665), 
         wl_below_surface_m = pressurehead_m - (dist_pressure_sensor_belowground_calc / 100)) %>% 
  mutate(flag_out_of_water = ifelse(wl_below_surface_m < ((dist_pressure_sensor_belowground_calc/100) * -1), TRUE,FALSE)) %>% 
  filter(datetime <= Sys.time())

df_trim <- df %>% 
  filter(datetime > Sys.time() - days(7))


# 5. Automated QC --------------------------------------------------------------

## Set columns you want
grouping_vars <- c("datetime", "plot", "flag_out_of_water")
parameters <- c("wl_below_surface_m", "temperature", "salinity", "p_h", "do_mgl", "battery_int", "voltage_ext")

## Create long dataframe
df_long <- df_trim %>% 
  rename("do_mgl" = rdo_concen) %>% 
  select(all_of(grouping_vars), all_of(parameters)) %>% 
  pivot_longer(cols = all_of(parameters))

## Flag things
df_qc <- df_long %>% 
  mutate(flag_error = ifelse(rowSums(. == -99999) > 0, TRUE, FALSE)) %>% 
  mutate(flag_wl = ifelse(name == "wl_below_surface_m" & flag_out_of_water == TRUE,  TRUE, FALSE), 
         flag_temp = ifelse(name == "temperature" & value < -5 | 
                              name == "temperature" & value > 50, TRUE, FALSE), 
         flag_salinity = ifelse(name == "salinity" & value < 0.01 | 
                                  name == "salinity" & value  > 40, TRUE, FALSE), 
         flag_ph = ifelse(name == "p_h" & value < 2 | 
                            name == "p_h" & value  > 12, TRUE, FALSE), 
         flag_do = ifelse(name == "do_mgl" & value < -0.1 | 
                            name == "do_mgl" & value  > 20, TRUE, FALSE)) %>% 
  select(-flag_out_of_water) %>% 
  mutate(flagged = ifelse(rowSums(select(., starts_with("flag"))) > 0, TRUE, FALSE)) %>% 
  mutate(across(where(is.numeric), ~ ifelse(. == -99999, -1, .)))


plot_colors = c("#F4A261", "#5386E4", "#DB162F")

# ## This function creates standardized time-series plots 
ts_plot <- function(var, y_lab){
  
  x <- df_qc %>% filter(name == var)
  
  ggplot(x, aes(datetime, value, color = plot)) +
    geom_line() +
    geom_point(data = x %>% filter(flagged == TRUE), color = "red", alpha = 0.5) +
    #facet_wrap(~plot, nrow = 1, scales = "free_y") +
    labs(x = "", y = y_lab) +
    scale_color_manual(values = plot_colors) +
    scale_x_datetime(date_breaks = "3 days", date_labels = "%m/%d")
}

## Create the time-series plots you want
ts_pressure_plot <- ts_plot("wl_below_surface_m", "WL (m below grd)")
ts_temp_plot <- ts_plot("temperature", "Temp (C)")
ts_sal_plot <- ts_plot("salinity", "Salinity (PSU)")
ts_do_plot <- ts_plot("do_mgl", "DO (mg/L)")
ts_ph_plot <- ts_plot("p_h", "pH")

ts_plots <- plot_grid(ts_pressure_plot, 
                      ts_temp_plot, 
                      ts_sal_plot, 
                      ts_do_plot, 
                      ts_ph_plot, 
                      ncol = 1, align = "hv")

title <- ggdraw() + 
  draw_label(
    paste0("Sensor health check: ", format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
    fontface = 'bold',
    x = 0,
    hjust = -0.25) 

export_location <- paste0("/Users/regi350/OneDrive - PNNL/Documents/projects/compass/synoptic/figures/tempest_checks/", 
                          "tempest_sensor_checkup_", 
                          str_remove_all(as.character(Sys.Date()), "-"), ".pdf")

plot_grid(title, ts_plots, ncol = 1, rel_heights = c(0.1, 1))
ggsave(export_location, width = 6, height = 8)


## Create a plot to calculate the proportion of possible data present
no_days = 7

df_trim <- df %>% 
  filter(datetime > Sys.time() - days(no_days))

min_datetime = min(df_trim$datetime)
max_datetime = max(df_trim$datetime)

max_count <- length(seq(from = min_datetime, 
                        #to = force_tz(now(), tzone = "UTC"), by = "15 min"))
                        to = max_datetime, by = "15 min"))

## Make proportions plot
prop_plot <- df_trim %>% 
  group_by(plot) %>% 
  count() %>%
  mutate(percent = (n / max_count) * 100) %>% 
  mutate(theoretical = 100) %>% 
  mutate(txt_color = ifelse(percent > 50, "upper", "lower")) %>% 
  ggplot(aes(x = plot, fill = plot)) + 
  geom_col(aes(y = theoretical), 
           position = "dodge", alpha = 0.2, show.legend = F)  +
  geom_col(aes(y = percent), position = "dodge", color = "black", show.legend = F) +
  geom_text(aes(y = 50, label = paste0(round(percent, 0), "%"), color = txt_color), 
            position = position_dodge(width = .9), angle = 90, show.legend = F) +
  scale_color_manual(values = c("black", "white"))  +
  scale_fill_manual(values = plot_colors) + 
  labs(x = "", y = "Percent", title = "Data transmission")

power_plot <- df_trim %>% 
  group_by(plot) %>% 
  dplyr::summarize(min = min(voltage_ext, na.rm = T)) %>% 
  mutate(min = ifelse(is.na(min), 0, min)) %>% 
  mutate(theoretical = 12) %>% 
  mutate(txt_color = ifelse(min > 6, "upper", "lower")) %>% 
  ggplot(aes(plot, fill = plot)) + 
  geom_col(aes(y = theoretical), position = "dodge", alpha = 0.2, show.legend = F)  +
  geom_col(aes(y = min), position = "dodge", color = "black") +
  geom_text(aes(y = 6, label = paste0(plot, ": ", round(min, 1), "V"), group = plot, color = txt_color), 
            position = position_dodge(width = .9), angle = 90, show.legend = F) +
  scale_color_manual(values = c("black", "white"))  +
  scale_fill_manual(values = plot_colors) + 
  labs(x = "", y = "Volts (min.)", title = "External Power") 


