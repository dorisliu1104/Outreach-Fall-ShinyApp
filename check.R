library(tidyverse)
library(lubridate)
library(here)

ph_clean <- read_csv(here("data", "ph_clean_1121.csv"))

ph_clean <- ph_clean %>%
  mutate(date=mdy(date))

f = "%m/%d/%Y" 
ph_clean$SetDateMonth <- format(as.POSIXct(ph_clean$date, format = f), "%m")

view(ph_clean)

#ph_clean_updated$date =  as.Date(ph_clean_updated$date, format = "%m-%d-%Y")

dateFiltered <- ph_clean %>%
  filter(site == "Lompoc Landing") %>%
  filter(date >= mdy("06/18/2021"),
         date <= mdy("07/08/2021"))

#plot
ph_ts_plot <- ggplot(dateFiltered, aes(x=date_time, y=tide)) +
  geom_line() +  
  xlab("Severity of incident") + 
  ylab("Number of incidents")

ph_ts_plot

ls <- ggplot(subset(ph_clean, site %in% "Lompoc Landing"), 
             aes(tide, p_h, color = SetDateMonth)) +
  geom_point() +
  ggtitle("Lompoc Landing")

ls

library(leaflet)
library(dplyr)
library(readxl)
site_gps <- read_excel("data/site gps.xlsx")
View(site_gps)
site_gps <- site_gps %>% 
  mutate(popup_info = paste(site, "<br/>", "Average ph", Avg_ph, "<br/>", "Average temperature", Avg_temp, "<br/>", "Average tide", Avg_tide ))
leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data = site_gps, lat = ~lat, lng = ~long, popup = ~popup_info)

devtools::install_github("UrbanInstitute/urbnmapr")

max(Alegria$p_h)
max(Bodega_Bay$p_h)
max(Lompoc_Landing$p_h)
