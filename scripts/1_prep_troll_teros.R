## This script preps MSM and GWI data for all available sensors to make single
## csvs that can be used to quickly make plots
##
## Peter Regier
## 2024-01-17

## Run setup file to prep environment
source("scripts/0_setup_ward.R")


# 1. Prep for downloading raw datasets -----------------------------------------

## Set the GDrive folders to find files
current_directory = "https://drive.google.com/drive/folders/1-1nAeF2hTlCNvg_TNbJC0t6QBanLuk6g"
archive_directory = "https://drive.google.com/drive/folders/16zmJrgSm7OHBn-4bDWvvO9E8NeX92nrl"

## Finally, we're going to clean out the directory we're going to download our data
## to, so we can check later that we downloaded the same number of files that we found on GDrive
raw_data_path <- "data/temp_dir_for_gdrive_files/"

## Set site of interest
### NOTES: if downloading GCW, need to remove duplicate file, if downloading
### OWC, need to exclude some files (see synoptic_check.R for details)
site_to_download <- c("MSM|GWI")


# 2. Download Google Drive files to local --------------------------------------

## This is KEY: it avoids user input, and will automatically find the token. 
## This has, I think, timed out before. If so, just run drive_auth() and select
## the proper email.
options(gargle_oauth_email = "peter.regier@pnnl.gov")

# Create a list of files (note that archive files get filtered so we don't read
# eeeeeverything in)
files_raw <- bind_rows(drive_ls(current_directory), 
                       drive_ls(archive_directory))

## First, check that all files are accounted for
file_names <- files_raw %>% 
  separate(name, c("project", "site", "location"), remove = F) %>% 
  select(name, site, location, id) %>% 
  filter(grepl(site_to_download, name))

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


# 3. Read in Troll datasets and process ----------------------------------------

## This function reads in and cleans up each file
read_troll <- function(data){
  
  string <- str_extract(data, "//(.+?)\\.dat") %>%
    str_replace("//", "")
  
  site <- str_split(string, "_", simplify = T)[,2]
  location <- str_split(string, "_", simplify = T)[,3]
  
  read_delim(file = data, skip = 1) %>% 
    slice(3:n()) %>% 
    clean_names() %>% 
    mutate(datetime = parsedate::parse_date(timestamp)) %>% 
    filter(datetime > "2022-03-01") %>% 
    mutate_at(vars(contains("600")), as.numeric) %>% 
    rename_with(~str_remove(., '600[a-z]')) %>% 
    rename("pressure_psi" = pressure) %>% 
    mutate(pressure_mbar = pressure_psi * 68.948) %>% 
    mutate(site = site, 
           location = location) %>% 
    select(datetime, site, location, temperature, salinity, rdo_concen, p_h, p_h_orp,
           depth, water_density, pressure_mbar, pressure_psi, voltage_ext, battery_int)
}

## Read in data and bind to a single dataframe
troll_raw <- list.files(raw_data_path, pattern = "WaterLevel", full.names = T) %>% 
  map(read_troll) %>% 
  bind_rows() %>% 
  drop_na()

well_dimensions <- read_sheet("https://docs.google.com/spreadsheets/d/1O4sHvj2FO7EcWEm3WpKEZhFubGn8HCcUsz9EFXhQTXM/edit#gid=0") %>% 
  mutate(location = case_when(transect_location == "Upland" ~ "UP", 
                              transect_location == "Transition" ~ "TR", 
                              transect_location == "Wetland" ~ "W"), 
         ground_to_sensor_cm = ring_to_pressure_sensor_cm - (well_top_to_ground_cm - bolt_to_cap_cm)) %>% 
  dplyr::select(site, location, ground_to_sensor_cm)

## Add well dimensions to dataset: inner_join ensures no uncorrected WL data makes it through
troll_raw_depths <- inner_join(troll_raw, well_dimensions, by = c("site", "location")) %>% 
  filter(!is.na(pressure_mbar))

## Correct water levels for pressure, density, and well dimensions
troll <- troll_raw_depths %>% 
  mutate(density_gcm3_cor = ifelse(water_density > 0.95, water_density, 1), 
         pressurehead_m = (pressure_mbar * 100) / (density_gcm3_cor * 1000 * 9.80665), 
         wl_below_surface_m = pressurehead_m - (ground_to_sensor_cm / 100)) %>% 
  ## This is a weird work-around: need to flag here so that we can easily match to spreadsheet
  mutate(flag_out_of_water = ifelse(wl_below_surface_m < ((ground_to_sensor_cm/100) * -1), TRUE,FALSE)) %>% 
  mutate(location = fct_relevel(location, c("UP", "TR", "W"))) %>% 
  filter(datetime <= Sys.time())

write_csv(troll, "data/240123_msm_gwi_troll.csv")


################################################################################


# 4. Read in TEROS datasets and process ----------------------------------------

## This function reads in and cleans up each file
read_teros <- function(data){
  
  depths = c("_", "10_", "10_", "10_", "30_", "30_")
  
  string <- str_extract(data, "//(.+?)\\.dat") %>%
    str_replace("//", "")
  
  site <- str_split(string, "_", simplify = T)[,2]
  location <- str_split(string, "_", simplify = T)[,3]
  
  read_delim(file = data, skip = 1) %>% 
    slice(3:n()) %>% 
    clean_names() %>% 
    mutate(datetime = parsedate::parse_date(timestamp)) %>% 
    filter(datetime > "2022-03-01") %>% 
    rename_with(~str_remove(., 'teros_a_|teros_b_|teros_c_')) %>% 
    rename_with(~str_replace(., '_1', '_VWC_raw')) %>% 
    rename_with(~str_replace(., '_2', '_Tsoil')) %>% 
    rename_with(~str_replace(., '_3', '_EC')) %>% 
    mutate_at(vars(contains(c("VWC","Tsoil", "EC"))), as.numeric) %>% 
    mutate(site = site, 
           location = location) %>% 
    dplyr::select(datetime, site, location, contains(c("_VWC", "_Tsoil", "_EC")))
  
}

## This creates a raw dataset
teros_raw <- list.files(raw_data_path, pattern = "TerosTable", full.names = T)  %>% 
  map(read_teros) %>% 
  bind_rows()


## Create three separate dataframes we'll stitch back together
longify_variable <- function(variable){
  teros_raw %>% 
    dplyr::select(datetime, site, location, contains(variable)) %>% 
    pivot_longer(cols = -c(datetime, site, location), 
                 names_to = c("depth", "var"), 
                 names_sep = '_', 
                 values_to = variable) %>% 
    dplyr::select(-var)
}

teros_wide <- longify_variable("VWC") %>% 
  bind_cols(longify_variable("EC") %>% dplyr::select("EC")) %>% 
  bind_cols(longify_variable("Tsoil") %>% dplyr::select("Tsoil")) 

teros_wide2 <- teros_wide %>% 
  mutate(VWC = (0.0003879 * VWC) - 0.6956) %>% 
  mutate(depth_cm = ifelse(location == "W" & depth <= 2, 10, 
                           ifelse(location == "W" & depth > 2 & depth < 5, NA,
                                  ifelse(location == "W" & depth >= 5, 30, 
                                         ifelse(location == "TR" & depth <= 4, 10, 
                                                ifelse(location == "TR"& depth > 4, 30, 
                                                       ifelse(location == "UP" & depth <= 4, 10, 
                                                              ifelse(location == "UP" & depth > 4, 30, NA)))))))) 

teros_cleaned <- teros_wide2 %>% 
  filter(!is.na(depth)) %>% 
  filter(VWC > 0) %>% 
  filter(EC > 0) %>% 
  clean_names()

write_csv(teros_cleaned, "data/240123_msm_gwi_teros.csv")


################################################################################



# 7. Read in EXO datasets and process ------------------------------------------






