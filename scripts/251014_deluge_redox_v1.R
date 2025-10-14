## I'm creating this script to import, clean up, and visualize current DELUGE
## redox sensor data. I will try and document as well as possible throughout, but 
## please don't hesitate to ask me any code questions. There may well be errors, 
## so if something looks wrong, please let me know! I'm bad about using Github 
## but am working on that - if you have questions or comments, please create an 
## issue: https://github.com/COMPASS-DOE/synoptic_checks
##
## Peter Regier, 2025-10-14
## peter.regier@pnnl.gov

# 1. Setup ---------------------------------------------------------------------

## load packages - if you haven't installed pacman run install.packages("pacman")
require(pacman)
p_load(tidyverse, 
       janitor, #clean_names()
       cowplot)

# Set ggplot theme
theme_set(theme_bw())

# Helper dataset - convert sensor # into depth
depth_seq = c(10, 20, 30, 40)

number_to_depth = tibble(sensor = as.character(c(1:24)), 
                         depth_cm = c(depth_seq, 
                                      depth_seq, 
                                      depth_seq, 
                                      depth_seq, 
                                      depth_seq, 
                                      depth_seq))


# 2. Set up data access --------------------------------------------------------

## Set the GDrive folders to find files - updated 9/11/24 after RR switched to new server
## YOU WILL NEED TO CHANGE THESE PATHS TO WHERE DATA CAN BE ACCESSED ON YOUR MACHINE
## If using L1, you should be able to plug and play code (hopefully)
## Note: data are stored in "current" (last ~week) and "archive" (everything else, periodically updated)
current_directory = "/Users/regi350/Dropbox/COMPASS_PNNL_Data/current_data"
archive_directory = "/Users/regi350/Dropbox/COMPASS_PNNL_Data/COMPASS_PNNL_Rawdata_Archive"

## Defines the time-frame of data to ingest
month_strings <- c(str_sub(str_replace_all(Sys.Date(), "-", ""), 1, 6), 
                   str_sub(str_replace_all(Sys.Date() - months(1), "-", ""), 1, 6))

## If you want all data, you can use this instead
# month_strings = "2025"

## Create lists of archived files we're interested in. DLG = Deluge, Redox is .... redox
archive_files <- list.files(archive_directory, full.names = T)[grepl("DLG", list.files(archive_directory, full.names = TRUE)) & 
                                                               grepl("Redox", list.files(archive_directory, full.names = TRUE))]

## Do the same for current files
current_files <- list.files(current_directory, full.names = T)[grepl("DLG", list.files(current_directory, full.names = TRUE)) & 
                                                                 grepl("Redox", list.files(current_directory, full.names = TRUE))]

files_to_read = c(archive_files, current_files)

# 3. Read in data --------------------------------------------------------

## This function pulls info from the file name and lightly formats data after reading in
read_data <- function(file){
  
  ## Pull info about data from filename
  filename <- str_replace(file, "^.*/", "")
  site <- str_split(filename, "_", simplify = T)[,1]
  #logger <- str_split(filename, "_", simplify = T)[,2]
  #depth <- str_split(filename, "_", simplify = T)[,3]
  
  ## Read in data
  read_delim(file = file, skip = 1) %>%
    slice(3:n()) %>%
    clean_names() %>% 
    pivot_longer(cols = starts_with("redox_r"),  # Select columns starting with "redox_r"
                 names_to = "sensor",           # Create the "sensor" column for column names
                 values_to = "redox_mv") %>%        # Create the "redox_mv" column for values
    mutate(redox_mv = as.numeric(redox_mv)) %>% 
    drop_na(redox_mv) %>% 
    separate(sensor, into = c("drop", "ref", "sensor"), sep = "_") %>% 
    mutate(datetime = as_datetime(timestamp)) %>% 
    dplyr::select(-c(record, statname, drop, timestamp)) %>% 
    mutate(site = str_remove(site, "DLG-"))
}

## Read in data from all files ID'ed as useful above
df_all <- files_to_read %>% 
map(read_data) %>% 
  bind_rows()

## Work with a subset - I have included the archive data code above so this script
## can be easily set up to look at the full time-series. Due to the large volume, 
## I'll be focusing only on data from the last 7 days for the remainder of the
## script. If you want ALL data, it's as easy as replacing "df_recent" with "df_all" 
## in all subsequent code and changing month_strings above. If you want to change 
## the time-frame, change the filter() call directly below.
df_recent <- df_all %>% 
  filter(datetime > Sys.Date() - days(7)) %>%  ## Filter to a specific time-frame
  left_join(number_to_depth, by = "sensor") ## Move up to df_all if using more data

# 4. Initial time-series plots -------------------------------------------------

## Initial looks at dataset
df_recent %>% 
  ggplot(aes(datetime, redox_mv, group = sensor, color = ref)) + 
  geom_line() + 
  facet_wrap(~site)
## ggsave("figures/251014_dlg_redox_ts.png", width = 9, height = 6)


# 5. Missing data plot ---------------------------------------------------------

min_datetime = min(df_recent$datetime)
max_datetime = max(df_recent$datetime)

max_count <- length(seq(from = min_datetime, 
                        to = max_datetime, by = "15 min"))

df_bin <- df_recent %>% 
  mutate(datetime = round_date(datetime, "15 min")) %>% ## There are some temporal
  ## shenanigans, for this plot round everything to 15-min intervales
  group_by(datetime, site, sensor, ref) %>% 
  summarize(redox_mv = mean(redox_mv)) %>% 
  mutate(sensor = as.numeric(sensor)) %>% 
  ungroup() 

df_bin %>% 
  group_by(site, sensor, ref) %>% 
  count() %>% 
  mutate(percent_of_data = (n / max_count) * 100) %>% 
  filter(percent_of_data < 99) %>% # only include sensors missing >1%
  ggplot(aes(as.factor(sensor), percent_of_data, fill = as.factor(sensor))) + 
  geom_col(position = "dodge", width = 0.7, color = NA) + 
  facet_wrap(~site, nrow = 1, scales = "free_x")


# 6. Initial QC ----------------------------------------------------------------

## Values close to 5V suggest poor connections (max value is 5V), and even values
## above ~1500 mV are potentially suspicious. Let's flag them, then filter them
## out of plots
mv_threshold = 1500

df_qc <- df_bin %>% 
  mutate(sensor = as.numeric(sensor)) %>% 
  mutate(flag_high_v = case_when(redox_mv > mv_threshold ~ "High mV", 
                                 TRUE ~ NA))

# 7. Flagged data --------------------------------------------------------------

df_qc %>% 
  filter(flag_high_v == "High mV") %>% 
  group_by(site, sensor, ref) %>% 
  count() %>% 
  mutate(percent_flagged = (n / max_count) * 100) %>% 
  filter(percent_flagged > 1) %>% # only include sensors with >1% sus data
  ggplot(aes(as.factor(sensor), percent_flagged, fill = as.factor(sensor))) + 
  geom_col(position = "dodge", width = 0.7, color = NA) + 
  facet_wrap(~site, nrow = 1, scales = "free_x")






