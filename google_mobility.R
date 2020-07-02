library(tidycovid19)
library(tidyverse)
library(dplyr)
library(zoo)
library(readr)
library(extrafont)
library(scales)
library(lubridate)
library(ggrepel)
loadfonts()

africa_iso <- c("DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CV", "CMR", "CAF", "TCD", "COM", "COD",
                "COG", "CIV", "DJI", "EGY", "ERI", "SWZ", "ETH", "GAB", "GMB", "GHA", "GIN", "GNB",
                "KEN", "LSO", "LBR", "LBY", "MDG", "MWI", "MLI", "MRT", "MUS", "MAR", "MOZ", "NAM",
                "NER", "NGA", "RWA", "STP", "SEN", "SYC", "SLE", "SOM", "ZAF", "SSD", "SDN", "TZA",
                "TGO", "TUN", "UGA", "ZMB", "ZWE")

google <- download_google_cmr_data(cached = T, silent = T)

google_afr <- google %>% 
  filter(iso3c %in% africa_iso)


zaf <- google %>% 
  filter(iso3c == "ZAF")

ggplot(zaf) +
  geom_line(aes(x = date, y = workplaces))

ggplot(zaf, aes(x = date, y = grocery_pharmacy)) +
  geom_line(size = 1.5, color = "#00965b") +
  geom_ribbon(aes(ymin = ifelse(grocery_pharmacy > 0, 0, grocery_pharmacy), ymax = 0), fill = "#00965b", alpha = 0.2) +
  labs(title = "South Africa's COVID-19 Community Mobility Report", y = "Grocery & Pharmacy", x = "",
       caption = "Source: Google Community Mobility Reports 27 June 2020, Graphic: Monique Bennett
Note: The Google report calculates movement change compared to a baseline that is the median value for the corresponding day of the week during 3rd Jan and 6 Feb 2020.") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%e-%b") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme(panel.background = element_blank(),
      text = element_text(family = "Proxima Nova Rg", size = 12),
      panel.grid.major = element_line(colour = "#f5f5f5"),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "#f5f5f5"),
      strip.text.x = element_text(hjust = 0),
      plot.caption = element_text(hjust = 0), 
      plot.title = element_text(size = 18, family = "Proxima Nova Bl"),
      plot.caption.position = "plot") 

ggsave("RSA_google_mobility.png", dpi = 600, width = 8, height = 8)

southern_afr <- c("AGO", "BWA", "MOZ", "NAM", "ZAF", "ZMB", "ZWE", "SWZ")
east_afr <- c("TZA", "KEN", "UGA", "MUS", "RWA", "UGA")
west_afr <- c("BEN", "BFA", "CIV", "GHA", "MLI", "NER", "NGA", "SEN", "TGO")

## Southern Africa grocery

google_afr %>% 
  filter(iso3c %in% southern_afr) %>% 
ggplot(aes(x = date, y = grocery_pharmacy, group = iso3c)) +
  geom_line(size = 1.5, color = "#00965b") +
  geom_ribbon(aes(ymin = ifelse(grocery_pharmacy > 0, 0, grocery_pharmacy), ymax = 0), fill = "#00965b", alpha = 0.2) +
  labs(title = "Southern Africa's COVID-19 Community Mobility Report", y = "Grocery & Pharmacy", x = "",
       caption = "Source: Google Community Mobility Reports 27 June 2020, Graphic: Monique Bennett,
Note: The Google report calculates movement change compared to a baseline that is the median value for the corresponding day of the week during 3rd Jan and 6 Feb 2020.") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%e-%b") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme(panel.background = element_blank(),
        text = element_text(family = "Proxima Nova Rg", size = 12),
        panel.grid.major = element_line(colour = "#f5f5f5"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#f5f5f5"),
        strip.text.x = element_text(hjust = 0),
        plot.caption = element_text(hjust = 0), 
        plot.title = element_text(size = 18, family = "Proxima Nova Bl"),
        plot.caption.position = "plot") +
  facet_wrap(~iso3c, ncol = 2, scales = "free") 

ggsave("southern_afr_mobility.png", dpi = 600, width = 11, height = 9)

## East Africa grocery

google_afr %>% 
  filter(iso3c %in% east_afr) %>% 
  ggplot(aes(x = date, y = grocery_pharmacy, group = iso3c)) +
  geom_line(size = 1.5, color = "#c35959") +
  geom_ribbon(aes(ymin = ifelse(grocery_pharmacy > 0, 0, grocery_pharmacy), ymax = 0), fill = "#c35959", alpha = 0.2) +
  labs(title = "East Africa's COVID-19 Community Mobility Report", y = "Grocery & Pharmacy", x = "",
       caption = "Source: Google Community Mobility Reports 27 June 2020, Graphic: Monique Bennett,
Note: The Google report calculates movement change compared to a baseline that is the median value for the corresponding day of the week during 3rd Jan and 6 Feb 2020.") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%e-%b") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme(panel.background = element_blank(),
        text = element_text(family = "Proxima Nova Rg", size = 12),
        panel.grid.major = element_line(colour = "#f5f5f5"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#f5f5f5"),
        strip.text.x = element_text(hjust = 0),
        plot.caption = element_text(hjust = 0), 
        plot.title = element_text(size = 18, family = "Proxima Nova Bl"),
        plot.caption.position = "plot") +
  facet_wrap(~iso3c, ncol = 2, scales = "free") 

ggsave("east_afr_mobility.png", dpi = 600, width = 11, height = 9)

## West Africa grocery

google_afr %>% 
  filter(iso3c %in% west_afr) %>% 
  ggplot(aes(x = date, y = grocery_pharmacy, group = iso3c)) +
  geom_line(size = 1.5, color = "#3a4971") +
  geom_ribbon(aes(ymin = ifelse(grocery_pharmacy > 0, 0, grocery_pharmacy), ymax = 0), fill = "#3a4971", alpha = 0.2) +
  labs(title = "West Africa's COVID-19 Community Mobility Report", y = "Grocery & Pharmacy", x = "",
       caption = "Source: Google Community Mobility Reports 27 June 2020, Graphic: Monique Bennett,
Note: The Google report calculates movement change compared to a baseline that is the median value for the corresponding day of the week during 3rd Jan and 6 Feb 2020.") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%e-%b") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme(panel.background = element_blank(),
        text = element_text(family = "Proxima Nova Rg", size = 12),
        panel.grid.major = element_line(colour = "#f5f5f5"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#f5f5f5"),
        strip.text.x = element_text(hjust = 0),
        plot.caption = element_text(hjust = 0), 
        plot.title = element_text(size = 18, family = "Proxima Nova Bl"),
        plot.caption.position = "plot") +
  facet_wrap(~iso3c, ncol = 2, scales = "free") 

ggsave("west_afr_mobility.png", dpi = 600, width = 11, height = 10)

#### Southern africa transit_stations

google_afr %>% 
  filter(iso3c %in% southern_afr) %>% 
  ggplot(aes(x = date, y = transit_stations, group = iso3c)) +
  geom_line(size = 1.5, color = "#005935") +
  geom_ribbon(aes(ymin = ifelse(transit_stations > 0, 0, transit_stations), ymax = 0), fill = "#005935", alpha = 0.2) +
  labs(title = "Southern Africa's COVID-19 Community Mobility Report", y = "Transit Stations", x = "",
       caption = "Source: Google Community Mobility Reports 27 June 2020, Graphic: Monique Bennett,
Note: The Google report calculates movement change compared to a baseline that is the median value for the corresponding day of the week during 3rd Jan and 6 Feb 2020.") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%e-%b") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme(panel.background = element_blank(),
        text = element_text(family = "Proxima Nova Rg", size = 12),
        panel.grid.major = element_line(colour = "#f5f5f5"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#f5f5f5"),
        strip.text.x = element_text(hjust = 0),
        plot.caption = element_text(hjust = 0), 
        plot.title = element_text(size = 18, family = "Proxima Nova Bl"),
        plot.caption.position = "plot") +
  facet_wrap(~iso3c, ncol = 2, scales = "free") 

ggsave("southern_afr_transit.png", dpi = 600, width = 11, height = 9)

### East Africa transit_stations

google_afr %>% 
  filter(iso3c %in% east_afr) %>% 
  ggplot(aes(x = date, y = transit_stations, group = iso3c)) +
  geom_line(size = 1.5, color = "#781e52") +
  geom_ribbon(aes(ymin = ifelse(transit_stations > 0, 0, transit_stations), ymax = 0), fill = "#781e52", alpha = 0.2) +
  labs(title = "East Africa's COVID-19 Community Mobility Report", y = "Transit Stations", x = "",
       caption = "Source: Google Community Mobility Reports 27 June 2020, Graphic: Monique Bennett,
Note: The Google report calculates movement change compared to a baseline that is the median value for the corresponding day of the week during 3rd Jan and 6 Feb 2020.") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%e-%b") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme(panel.background = element_blank(),
        text = element_text(family = "Proxima Nova Rg", size = 12),
        panel.grid.major = element_line(colour = "#f5f5f5"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#f5f5f5"),
        strip.text.x = element_text(hjust = 0),
        plot.caption = element_text(hjust = 0), 
        plot.title = element_text(size = 18, family = "Proxima Nova Bl"),
        plot.caption.position = "plot") +
  facet_wrap(~iso3c, ncol = 2, scales = "free") 

ggsave("east_afr_transit.png", dpi = 600, width = 11, height = 9)

### West Africa transit

google_afr %>% 
  filter(iso3c %in% west_afr) %>% 
  ggplot(aes(x = date, y = transit_stations, group = iso3c)) +
  geom_line(size = 1.5, color = "#b8b335") +
  geom_ribbon(aes(ymin = ifelse(transit_stations > 0, 0, transit_stations), ymax = 0), fill = "#b8b335", alpha = 0.2) +
  labs(title = "West Africa's COVID-19 Community Mobility Report", y = "Transit Stations", x = "",
       caption = "Source: Google Community Mobility Reports 27 June 2020, Graphic: Monique Bennett,
Note: The Google report calculates movement change compared to a baseline that is the median value for the corresponding day of the week during 3rd Jan and 6 Feb 2020.") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%e-%b") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme(panel.background = element_blank(),
        text = element_text(family = "Proxima Nova Rg", size = 12),
        panel.grid.major = element_line(colour = "#f5f5f5"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#f5f5f5"),
        strip.text.x = element_text(hjust = 0),
        plot.caption = element_text(hjust = 0), 
        plot.title = element_text(size = 18, family = "Proxima Nova Bl"),
        plot.caption.position = "plot") +
  facet_wrap(~iso3c, ncol = 2, scales = "free") 

ggsave("west_afr_transit.png", dpi = 600, width = 11, height = 10)
