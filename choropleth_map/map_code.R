# sample code for creating choropleth map
# done on county level but works for states too

#-------------------------------------------------------------------------------
# 0. load libraries and data
#-------------------------------------------------------------------------------
rm(list=ls())
library(ggplot2)
library(tigris)
library(sf)
library(stringr)
library(dplyr)

# read data
d <- read.csv("sample_data.csv")

#-------------------------------------------------------------------------------
# 1. prepare data
#-------------------------------------------------------------------------------
# pad geoid column with 0s
d <- d %>%
  select(geoid, percent) %>% 
  mutate(geoid = str_pad(geoid, width=5, pad="0"))

#-------------------------------------------------------------------------------
# 2. load county shapefile 
#-------------------------------------------------------------------------------
# set tigris options
county_shp <- counties(year=2024, cb=TRUE, resolution="20m")

# subset geoids to restrict to contiguous US
county_geoids <- d$geoid
county_shp <- county_shp %>% 
  filter(GEOID %in% county_geoids) 

#-------------------------------------------------------------------------------
# 3. merge data and county shapefile and clean data 
#-------------------------------------------------------------------------------
# shapefile must be joined on to preserve sf class
merged <- left_join(county_shp, d, by=c("GEOID" = "geoid"))
merged <- merged %>% 
  select(GEOID, percent, geometry)

#-------------------------------------------------------------------------------
# 4. prepare merged data to be mapped
#-------------------------------------------------------------------------------
# 7 color bins 
palette_7 <- c("#4575B0", "#92BFDB", "#E0F7F2", "#FFFFAF", "#FEE100", "#FC8D80", "#D73050")

# create more colors from original palette
color_ramp <- colorRampPalette(palette_7)
palette_10 <- color_ramp(10)

# create 10 bins to match colors
bin_n <- 10
min_val <- min(d[["percent"]], na.rm = TRUE)
max_val <- max(d[["percent"]], na.rm = TRUE)
break_points <- round(seq(min_val, max_val, length.out = (bin_n + 1)))

# create column with binned range 
bin_labels <- paste(head(break_points, -1), tail(break_points, -1), sep = " - ")
merged[["binned_val"]] <- cut(merged[["percent"]], breaks=bin_n, labels=bin_labels) %>% 
  as.factor()

#-------------------------------------------------------------------------------
# 5. create map
#-------------------------------------------------------------------------------

plot <- merged %>%
  ggplot() +
  geom_sf(aes(fill = binned_val)) +
  scale_fill_manual(values = palette_10, name = "Percentage", guide = guide_legend(reverse=TRUE)) +  
  labs(title = "County-level Values",
    fill = "Values", 
    x="", y="") +
  theme_void() + 
  theme(legend.title = element_text(colour = "black", size = 12), 
        legend.text = element_text(colour = "black", size = 10), 
        legend.key.size = unit(1.5, "lines")) 

plot

# output plot to png
# ggsave("map.png", plot, width = 10, height = 5)


