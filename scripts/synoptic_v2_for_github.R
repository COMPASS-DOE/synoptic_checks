## This is the second version of the synoptic_check script. Partially re-designing
## how files are read in because GWC_W and now SWH are being royal pains, so that
## code needs to be more robust with some checks n stuff. Also, the figure is 
## uggggly so gonna redesign to increase info content, make it nicer to look at, 
## and hopefully simpler to see key points right away.
##
## Peter Regier, 2022-04-06 (uploaded to Github 2024-09-06)
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

## Set the GDrive folders to find files
current_directory = "https://drive.google.com/drive/folders/1-1nAeF2hTlCNvg_TNbJC0t6QBanLuk6g"
archive_directory = "https://drive.google.com/drive/folders/16zmJrgSm7OHBn-4bDWvvO9E8NeX92nrl"

## Finally, set up some YYYYMM strings for this month and last month, to limit
## the number of files you have to import (will only become more useful as 
## datasets grow)
month_strings <- c(str_sub(str_replace_all(Sys.Date(), "-", ""), 1, 6), 
                   str_sub(str_replace_all(Sys.Date() - months(1), "-", ""), 1, 6))

## Finally, we're going to clean out the directory we're going to download our data
## to, so we can check later that we downloaded the same number of files that we found on GDrive
raw_data_path <- "data/raw_from_gdrive/synoptic_troll/"

## Now, delete all these files (https://stackoverflow.com/questions/9296377/automatically-delete-files-folders)
do.call(file.remove, list(list.files(raw_data_path, full.names = TRUE)))


# 2. Download Google Drive files to local --------------------------------------

## This is KEY: it avoids user input, and will automatically find the token. 
## This has, I think, timed out before. If so, just run drive_auth() and select
## the proper email.
options(gargle_oauth_email = "peter.regier@pnnl.gov")

# Create a list of files (note that archive files get filtered so we don't read
# eeeeeverything in)
files_raw <- bind_rows(drive_ls(current_directory) %>% 
                         filter(grepl("WaterLevel", name)), 
                       drive_ls(archive_directory) %>% 
                         filter(grepl("WaterLevel", name)) %>% 
                         filter(grepl(paste(month_strings, collapse = "|"), name)))


## Count the number of total files
n_total_files = nrow(files_raw)

## Set up a theoretical site-list to check against
site_locations <- expand_grid(site = c("CRC", "OWC", "PTR", "GCW", "GWI", "MSM", "SWH"), 
            location = c("W", "TR", "UP")) 

## calculate the number of files expected (one from current, one from archive)
n_expected = nrow(site_locations) * 2

## First, check that all files are accounted for
file_names <- files_raw %>% 
  separate(name, c("project", "site", "location"), remove = F) %>% 
  select(name, site, location, id)

## Sanity checks
missing_sites <- anti_join(site_locations, 
                           file_names %>% select(site, location), 
                           by = c("site", "location"))

## Create a custom function
drive_download_ <- function(data){
  
  #message(paste("Downloading", data$name))
  
  drive_download(data$id, overwrite = T, path = paste0(raw_data_path, data$name))
}

## Create a function that won't crash if a given file won't read in
safely_drive_download <- safely(drive_download_, NA)

## Use a for-loop to read in files in a way that I can see what's going on
## Download data to local. I tried to map() but for some reasons it doesn't work?
for(i in 1:nrow(file_names)){
  safely_drive_download(file_names %>% slice(i))
}

## Last step before reading data: let's check if files read in match files expected
files_not_downloaded <- setdiff(file_names$name, list.files(raw_data_path))


# 3. Read in data --------------------------------------------------------------

## This function reads in and cleans up each file
read_data <- function(data){
  
  site <- str_split(data, "_", simplify = T)[,5]
  location <- str_split(data, "_", simplify = T)[,6]
  sensor <- str_split(data, "_", simplify = T)[,8]
  
  read_delim(file = data, skip = 1) %>% 
    slice(3:n()) %>% 
    clean_names() %>% 
    mutate(datetime = parsedate::parse_date(timestamp)) %>% 
    filter(datetime > "2022-03-01") %>% 
    mutate_at(vars(contains("600")), as.numeric) %>% 
    rename_with(~str_remove(., '600[a-z]')) %>% 
    rename("pressure_psi" = pressure) %>% 
    mutate(pressure_mbar = pressure_psi * 68.948) %>% 
    #separate(statname, c("project", "site", "location")) %>% 
    mutate(site = site, 
           location = location, 
           sensor = sensor) %>% 
    select(datetime, site, location, sensor, 
           temperature, salinity, rdo_concen, p_h, p_h_orp,
           depth, water_density, pressure_mbar, pressure_psi, 
           voltage_ext, battery_int)
}

## Read in data and bind to a single dataframe
df_raw <- list.files(raw_data_path, full.names = T) %>% 
  map(read_data) %>% 
  bind_rows()


# 4. Clean data (this is not QC) -----------------------------------------------

## Now the big lift: calculate proper depths for the sensors. This takes two 
## additional datasets: 1) barometric pressure from Climavue stations, and 
## 2) well dimensions for the depth of the pressure sensor below ground level.

## Second, read in well dimensions. These are measurements made when installing
## sensors, and are used to calculate the distance below-ground that the 
well_dimensions <- read_sheet("https://docs.google.com/spreadsheets/d/1O4sHvj2FO7EcWEm3WpKEZhFubGn8HCcUsz9EFXhQTXM/edit#gid=0") %>% 
  mutate(location = case_when(transect_location == "Upland" ~ "UP", 
                              transect_location == "Transition" ~ "TR", 
                              transect_location == "Wetland" ~ "W"), 
         ground_to_sensor_cm = ring_to_pressure_sensor_cm - (well_top_to_ground_cm - bolt_to_cap_cm)) %>% 
  dplyr::select(site, location, ground_to_sensor_cm)

## Add well dimensions to dataset: inner_join ensures no uncorrected WL data makes it through
df_raw_depths <- inner_join(df_raw, well_dimensions, by = c("site", "location")) %>% 
  filter(!is.na(pressure_mbar))

## Correct water levels for pressure, density, and well dimensions
## 0.98 based on lower density of freshwater based on L1 readme
## 1.05 is upper bound
df <- df_raw_depths %>% 
  mutate(density_gcm3_cor = ifelse(water_density >= 0.98 & water_density <= 1.05, water_density, 1), 
         pressurehead_m = (pressure_mbar * 100) / (density_gcm3_cor * 1000 * 9.80665), 
         wl_below_surface_m = pressurehead_m - (ground_to_sensor_cm / 100)) %>% 
  ## This is a weird work-around: need to flag here so that we can easily match to spreadsheet
  mutate(flag_out_of_water = ifelse(wl_below_surface_m < ((ground_to_sensor_cm/100) * -1), TRUE,FALSE)) %>% 
  mutate(location = ifelse(site == "OWC" & location == "W", "WC", location), 
         location = ifelse(site == "OWC" & location == "UP", "WTE", location), 
         location = ifelse(site == "SWH" & grepl("600A", sensor), "SWAMP", location)) %>% # https://github.com/COMPASS-DOE/data-workflows/issues/117
  mutate(location = fct_relevel(location, c("UP", "SWAMP", "TR", "WTE", "W", "WC"))) %>% 
  filter(datetime <= Sys.time())

df_trim <- df %>% 
  filter(datetime > Sys.time() - days(7))

ggplot(df, aes(pressure_mbar, pressurehead_m, color = ground_to_sensor_cm)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F)

summary(lm(pressure_mbar ~ pressurehead_m, data = df))

#df_trim %>% ggplot(aes(temperature, site, fill = location)) + geom_boxplot()

# 5. Automated QC --------------------------------------------------------------

## Set columns you want
grouping_vars <- c("datetime", "site", "location", "flag_out_of_water")
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
  mutate(flagged = ifelse(rowSums(select(., starts_with("flag"))) > 0, TRUE, FALSE))


# 6. Create time-series plots --------------------------------------------------

## Set the interval of time you want to look at
no_days = 7

## set color palette
location_colors = PNWColors::pnw_palette("Sunset", length(unique(df$location)))
location_colors = c("#3B1F2B", "green2", "#DB162F", "#F4A261", "#5386E4", "#A7D49B")

# ## This function creates standardized time-series plots 
ts_plot <- function(var, y_lab){
  
  x <- df_qc %>% filter(name == var)
  
  ggplot(x, aes(datetime, value, color = location)) +
    geom_line() +
    geom_point(data = x %>% filter(flagged == TRUE), color = "red", alpha = 0.5) +
    facet_wrap(~site, nrow = 1, scales = "free_y") +
    labs(x = "", y = y_lab) +
    scale_color_manual(values = location_colors) +
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


# 6. Create diagnostic plots ---------------------------------------------------

## Create a plot to calculate the proportion of possible data present
df_trim <- df %>% 
  filter(datetime > Sys.time() - days(no_days))

min_datetime = min(df_trim$datetime)
max_datetime = max(df_trim$datetime)

max_count <- length(seq(from = min_datetime, 
                        #to = force_tz(now(), tzone = "UTC"), by = "15 min"))
                        to = max_datetime, by = "15 min"))

max_counts <- site_locations %>% 
  filter(!(site == "OWC" & location == "UP")) %>% 
  filter(!(site == "OWC" & location == "W")) %>% 
  add_row(site = "OWC", location = "WTE") %>% 
  add_row(site = "OWC", location = "WC") %>% 
  #filter(site != "SWH") %>% 
  mutate(location = fct_relevel(location, c("UP", "TR", "WTE", "W", "WC")))

## Make proportions plot
prop_plot <- df_trim %>% 
  group_by(site, location) %>% 
  count() %>%
  bind_rows(anti_join(max_counts, .)) %>% 
  mutate(n = ifelse(is.na(n), 0, n)) %>% 
  mutate(percent = (n / max_count) * 100) %>% 
  mutate(theoretical = 100) %>% 
  mutate(txt_color = ifelse(percent > 50, "upper", "lower")) %>% 
  ggplot(aes(x = site, fill = location)) + 
  geom_col(aes(y = theoretical), 
           position = "dodge", alpha = 0.2, show.legend = F)  +
  geom_col(aes(y = percent), position = "dodge", color = "black") +
  geom_text(aes(y = 50, label = paste0(round(percent, 0), "%"), group = location, color = txt_color), 
            position = position_dodge(width = .9), angle = 90, show.legend = F) +
  scale_color_manual(values = c("black", "white"))  +
  scale_fill_manual(values = location_colors) + 
  labs(x = "", y = "Percent", title = "Data transmission")

## Make battery voltage plot
battery_plot <- df_trim %>% 
  group_by(site, location) %>% 
  dplyr::summarize(min = min(battery_int, na.rm = T)) %>% 
  bind_rows(anti_join(max_counts, .)) %>% 
  mutate(min = ifelse(is.na(min), 0, min)) %>% 
  mutate(theoretical = 100) %>% 
  mutate(txt_color = ifelse(min > 50, "upper", "lower")) %>% 
  ggplot(aes(site, fill = location)) + 
  geom_col(aes(y = theoretical), position = "dodge", alpha = 0.2, show.legend = F)  +
  geom_col(aes(y = min), position = "dodge", color = "black") +
  geom_text(aes(y = 50, label = paste0(location, ": ", round(min, 0), "%"), group = location, color = txt_color), 
            position = position_dodge(width = .9), angle = 90, show.legend = F) +
  scale_color_manual(values = c("black", "white"))  +
  scale_fill_manual(values = location_colors) + 
  labs(x = "", y = "Percent (min.)", title = "Battery Power")

power_plot <- df_trim %>% 
  group_by(site, location) %>% 
  dplyr::summarize(min = min(voltage_ext, na.rm = T)) %>% 
  bind_rows(anti_join(max_counts, .)) %>% 
  mutate(min = ifelse(is.na(min), 0, min)) %>% 
  mutate(theoretical = 12) %>% 
  mutate(txt_color = ifelse(min > 6, "upper", "lower")) %>% 
  ggplot(aes(site, fill = location)) + 
  geom_col(aes(y = theoretical), position = "dodge", alpha = 0.2, show.legend = F)  +
  geom_col(aes(y = min), position = "dodge", color = "black") +
  geom_text(aes(y = 6, label = paste0(location, ": ", round(min, 1), "V"), group = location, color = txt_color), 
            position = position_dodge(width = .9), angle = 90, show.legend = F) +
  scale_color_manual(values = c("black", "white"))  +
  scale_fill_manual(values = location_colors) + 
  labs(x = "", y = "Volts (min.)", title = "External Power") 

power_plots <- plot_grid(battery_plot, power_plot, ncol = 1, align = "hv")

bar_plots <- plot_grid(prop_plot, power_plots, nrow = 1)


# Create final plots and export

## Add a title to keep track of when the plot was made
title <- ggdraw() + 
  draw_label(
    paste0("Sensor health check: ", format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
    fontface = 'bold',
    x = 0,
    hjust = -0.25
  ) 

## squish all those puppies together into a big, beautiful mess, and write!
plot_grid(title, ts_plots, bar_plots,
          rel_heights = c(0.1, 1, 0.5), ncol = 1)

export_location <- paste0("/Users/regi350/OneDrive - PNNL/Documents/projects/compass/synoptic/figures/synoptic_checks/", 
                          "synoptic_sensor_checkup_", 
                          str_remove_all(as.character(Sys.Date()), "-"), ".pdf")

ggsave(export_location, width = 13, height = 13.5)
