# Beverly checking through tutorial 
# There was a comment that stated that this needed updating

# Introductory material ----

# Load libraries

library(readr)
library(tidyr)
library(dplyr)
library(broom)
library(ggplot2)
library(ggthemes)
library(maps)
library(mapdata)
library(viridis)

# Loading data

LPI_EU <- read_csv("pop-change/LPI_EU.csv")

View(LPI_EU)
head(LPI_EU)
tail(LPI_EU)
str(LPI_EU)

# Preparing data for analysis ---- 

# Transform data to long format 

LPI_long <- gather(data = LPI_EU, key = "year", 
                   value = "population", select = 24:89)

# Changing struture of data, year and population to be numeric, not char! 

LPI_long$year <- parse_number(LPI_long$year)

str(LPI_long)

LPI_long$year <- as.numeric(as.character(LPI_long$year))

LPI_long$population <- as.numeric(as.character(LPI_long$population))

# Remove rows with no population information 

LPI_long <- filter(LPI_long, population != "NULL")

# Select populations with at least 5 data points

LPI_long <- LPI_long %>% 
  group_by(id) %>% 
  filter(length(unique(year)) > 4)

# Create new column with population size scaled to be between 0-1

LPI_long <- LPI_long %>% 
  group_by(id) %>% 
  mutate(scalepop = (population - min(population))/(max(population)-min(population)))

View(LPI_long)

# Calculating population change for Anseriformes ----

unique(LPI_long$order)

anseriformes <- filter(LPI_long, order == "Anseriformes")

pop_change <- anseriformes %>%
  group_by(latitude, longitude, binomial, id) %>%  
  do(mod = lm(scalepop ~ year, data = .)) %>%  
  tidy(mod) %>%  
  select(., latitude, longitude, binomial, id, term, estimate) %>%  
  spread(., term, estimate) %>%  
  ungroup()  

View(pop_change)

# Make map using ggplot ---- 

(EU_pop <- ggplot(pop_change, aes(x = longitude, y = latitude, fill = year)) +
    borders("world", xlim = c(-10, 25), ylim = c(40, 80), colour = "gray40", fill = "gray75", size = 0.3) +
    theme_map() +
    geom_point(shape = 21, colour = "black", alpha = 0.8, size = 4, position = position_jitter(w = 1.5, h = 1.5)) +
    scale_fill_viridis() +
    theme(legend.position = "right",
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 12),
          legend.justification = "center",
          plot.title = element_text(size = 20, vjust = 1, hjust = 0.6, face = "italic")) +
    labs(fill = "Slope\n", title = "Anseriformes"))  
