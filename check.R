library(tidyverse)
library(lubridate)
library(here)

ph_clean <- read_csv(here("data", "ph_clean_1121.csv"))

ph_clean <- ph_clean %>%
  mutate(date=mdy(date))

#ph_clean_updated$date =  as.Date(ph_clean_updated$date, format = "%m-%d-%Y")

dateFiltered <- ph_clean %>%
  filter(site == "Lompoc Landing") %>%
  filter(date >= mdy("06/18/2021"),
         date <= mdy("09/08/2021"))

#plot
ph_ts_plot <- ggplot(dateFiltered, aes(x=date_time, y=tide)) +
  geom_line() +  
  xlab("Severity of incident") + 
  ylab("Number of incidents")

ph_ts_plot
  