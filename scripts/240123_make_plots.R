## This script looks for good chunks of data and create plots for Nick's paper
## Using GWI which has some cool events
##
## Peter Regier
## 2024-01-17

## Run setup file to prep environment
source("scripts/0_setup_ward.R")


# 1. Read in datasets ----------------------------------------------------------

troll_raw <- read_csv("data/240123_msm_gwi_troll.csv")

## TEROS needs to be binned, so let's calculate averages by depth
teros_raw <- read_csv("data/240123_msm_gwi_teros.csv") %>% 
  select(-depth) %>% 
  group_by(datetime, site, location, depth_cm) %>% 
  summarize(across(where(is.numeric), mean))

sapflow_2022_09 <- read_csv("data/l1_from_gdrive/GWI_2022/GWI_20220901-20220930_L1_v0-9.csv") %>% 
  separate(design_link, sep = "-", into = c("trash", "site", "location"))

sapflow_2022_10 <- read_csv("data/l1_from_gdrive/GWI_2022/GWI_20221001-20221031_L1_v0-9.csv") %>% 
  separate(design_link, sep = "-", into = c("trash", "site", "location"))

sapflow_raw <- bind_rows(sapflow_2022_09, sapflow_2022_10) %>% 
  rename("datetime" = TIMESTAMP) %>% 
  filter(grepl("sapflow", research_name)) %>% 
  rename("sapflow" = value) %>% 
  select(datetime, site, location, sapflow) %>% 
  group_by(datetime, site, location) %>% 
  summarize(mean_sapflow = mean(sapflow, na.rm = T))

precip_raw <- bind_rows(sapflow_2022_09, sapflow_2022_10) %>% 
  rename("datetime" = TIMESTAMP) %>% 
  filter(grepl("wx_rain15", research_name)) %>% 
  #mutate(location = "TR") %>% 
  rename("rain15" = value) %>% 
  select(datetime, site, rain15) 

par <- bind_rows(sapflow_2022_09, sapflow_2022_10) %>% 
  rename("datetime" = TIMESTAMP) %>% 
  filter(grepl("wx_slr_tf15", research_name)) %>% 
  #mutate(location = "TR") %>% 
  rename("slr_tf15" = value) %>% 
  select(datetime, site, slr_tf15) 

plot_grid(par %>% 
            filter(datetime > "2022-09-20" &
                     datetime < "2022-10-10") %>% 
            ggplot(aes(datetime, slr_tf15)) + 
            geom_line(), 
          sapflow_raw %>% 
            filter(datetime > "2022-09-20" &
                     datetime < "2022-10-10") %>% 
            ggplot(aes(datetime, mean_sapflow)) + 
            geom_line(), 
          ncol = 1)
ggsave("figures/240906_par_and_sapflow.png", height = 5, width = 5)


# 2. Merge into a single dataset -----------------------------------------------

df <- inner_join(troll_raw, teros_raw, 
                 by = c("datetime", "site", "location")) %>% 
  full_join(sapflow_raw, 
             by = c("datetime", "site", "location")) %>% 
  filter(rdo_concen >= 0) %>% 
  full_join(precip_raw, 
            by = c("datetime", "site")) 
  
## Check plot
ggplot(df %>% filter(site == "GWI"), 
       aes(datetime, rain15, color = location)) + 
  geom_line()


# 3. Make plots ----------------------------------------------------------------

plot_timeperiod <- function(start, end){
  
  x <- df %>% 
      filter(datetime > start &
               datetime < end) %>% 
    filter(site == "GWI")
  
  p_width = 4
  p_height = 2
  
  p0 <- x %>% filter(location == "UP") %>% 
    ggplot(aes(datetime, mean_sapflow)) + 
    geom_line(show.legend = F)
  ggsave("figures/240125_0_up_sapflow.png", width = p_width, height = p_height)
  ggsave("figures/pdfs/240125_0_up_sapflow.pdf", width = p_width, height = p_height)
  
  p1 <- x %>% filter(location == "UP") %>% 
    ggplot(aes(datetime, vwc, color = as.factor(depth_cm))) + 
    geom_line(show.legend = F)
  ggsave("figures/240125_1_up_vwc.png", width = p_width, height = p_height)
  ggsave("figures/pdfs/240125_1_up_vwc.pdf", width = p_width, height = p_height)
  
  p2 <- x %>% filter(location == "UP") %>% 
    ggplot(aes(datetime, wl_below_surface_m)) + 
    geom_line()
  ggsave("figures/240125_2_up_wl.png", width = p_width, height = p_height)
  ggsave("figures/pdfs/240125_2_up_wl.pdf", width = p_width, height = p_height)

  p3 <- x %>% filter(location == "UP") %>% 
    ggplot(aes(datetime, rdo_concen)) + 
    geom_line() + 
    labs(x = "", y = "DO (mg/L)", title = "Upland GW")
  ggsave("figures/240125_3_up_do.png", width = p_width, height = p_height)
  ggsave("figures/pdfs/240125_3_up_do.pdf", width = p_width, height = p_height)

  p4 <- x %>% filter(location == "TR") %>% 
    ggplot(aes(datetime, rdo_concen)) + 
    geom_line() + 
    labs(x = "", y = "DO (mg/L)", title = "Transition GW")
  ggsave("figures/240125_4_tr_do.png", width = p_width, height = p_height)
  ggsave("figures/pdfs/240125_4_tr_do.pdf", width = p_width, height = p_height)

  p5 <-x %>% filter(location == "W") %>% 
    ggplot(aes(datetime, rdo_concen)) + 
    geom_line() + 
    labs(x = "", y = "DO (mg/L)", title = "Wetland GW")
  ggsave("figures/240125_5_w_do.png", width = p_width, height = p_height)
  ggsave("figures/pdfs/240125_5_w_do.pdf", width = p_width, height = p_height)
  
  p6 <-x %>% filter(location == "W") %>% 
    ggplot(aes(datetime, salinity)) + 
    geom_line() + 
    labs(x = "", y = "Salinity (PSU)", title = "Wetland GW")
  ggsave("figures/240410_6_salinity.png", width = p_width, height = p_height)
  ggsave("figures/pdfs/240410_6_salinity.pdf", width = p_width, height = p_height)
  
  # p7 <-x %>% filter(location == "W") %>% 
  #   ggplot(aes(datetime, mean_rain)) + 
  #   geom_line() + 
  #   labs(x = "", y = "Rain (...)", title = "Wetland GW")
  # ggsave("figures/240410_7_rain.png", width = p_width, height = p_height)
  # ggsave("figures/pdfs/240410_7_rain.pdf", width = p_width, height = p_height)
  
  plot_grid(p1, p2, ncol = 1)
  ggsave("figures/240125_vertical_wl.png", width = 3.5, height = 6)

  plot_grid(p3, p4, p5, nrow = 1)
  ggsave("figures/240125_lateral_do.png", width = 12, height = 2.5)
}

## Make plots for 9/20 - 10/10
plot_timeperiod("2022-09-20", "2022-10-10")

## Export
data_to_export <- df %>% 
  filter(datetime > "2022-09-20" &
           datetime < "2022-10-10") %>% 
  filter(site == "GWI")
write_csv(data_to_export, "data/240910_synoptic_sensor_data.csv")

