library(tidyverse)
library(lubridate)
library(here)

ph_clean_updated <- read_csv(here("data", "ph_clean_1121.csv"))

#ph_clean_updated$date =  as.Date(ph_clean_updated$date, format = "%m-%d-%Y")

site_select <- ph_clean_updated %>% 
    filter(site == "Alegria")

dateFiltered <- ph_clean_updated %>%
  filter(date %in% seq("2021-06-18",
                       "2021-10-08",
                       by = "day"))

#plot
ph_ts_plot <- ggplot(dateFiltered, aes(x=date_time, y=tide)) +
  geom_line() +  
  xlab("Severity of incident") + 
  ylab("Number of incidents")

ph_ts_plot
  