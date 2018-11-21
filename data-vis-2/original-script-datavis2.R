# Coding club tutorial: data visualization 2 

#### <a href="#hist"> 1. Customising histograms </a>

#### <a href="#labs"> 2. Adding titles, subtitles, captions and axis labels </a>

#### <a href="#panel"> 3. Changing the plot background </a>

#### <a href="#legend"> 4. Fixing the legend and customizing colours  </a>

# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

# Loading packages ---- 

library(readr)
library(dplyr)
library(ggplot2)

# Loading and exploring data ---- 
magic_veg <- read_csv("data-vis-2/magic_veg.csv")

str(magic_veg)

# Creating and customising histograms: no of species per plot ---- 

species_counts <- magic_veg %>%
  group_by(land, plot) %>%
  summarise(Species_number = length(unique(species)))

# Normal way to make a histogram in ggplot2

(hist <- ggplot(species_counts, aes(x = plot)) +
  geom_histogram() +
  theme_bw())

# Fixing the counts 

(hist <- ggplot(species_counts, aes(x = plot, y = Species_number)) +
    geom_histogram(stat = "identity"))

# Splitting the bar into the different locations, stacked

(hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
  geom_histogram(stat = "identity"))

# Splitting the bar into the different locations, not stacked

(hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge"))

# Having the plot numbers show 1,2,3,4,5,6

(hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") + 
    scale_y_continuous(limits = c(0, 50)) +
    scale_x_continuous(breaks = c(1,2,3,4,5,6)))

# Beautifying the graph: adding title, subtitle, caption, axis labels 

(hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") +
    scale_y_continuous(limits = c(0, 50)) +
    scale_x_continuous(breaks = c(1,2,3,4,5,6)) + 
    labs(title = "Species richness by plot", 
         subtitle = "In the magical lands",
         caption = "Data from the Ministry of Magic", 
         x = "Plot number", y = "Number of species"))

(hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") +
    scale_y_continuous(limits = c(0, 50)) +
    scale_x_continuous(breaks = c(1,2,3,4,5,6)) + 
    labs(title = "Species richness by plot", 
         x = "\n Plot number", y = "Number of species \n") + 
    theme(axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12, face = "italic"), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold")))

# Beautifying graph: plot background - white, and remove lines 

(hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") + 
    scale_y_continuous(limits = c(0, 50)) +
    scale_x_continuous(breaks = c(1,2,3,4,5,6)) + 
    labs(title = "Species richness by plot", 
         x = "\n Plot number", y = "Number of species \n") + 
    theme_bw() +
    theme(panel.grid. = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold")))

# Beautifying graph: putting a margin around the plot

(hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") +
    scale_y_continuous(limits = c(0, 50)) +
    scale_x_continuous(breaks = c(1,2,3,4,5,6)) + 
    labs(title = "Species richness by plot", 
         x = "\n Plot number", y = "Number of species \n") + 
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm")))

# Beautifying the graph: fixing the legend

(hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") + 
    scale_y_continuous(limits = c(0, 50)) +
    scale_x_continuous(breaks = c(1,2,3,4,5,6)) + 
    scale_fill_manual(values = c("rosybrown1", "#deebf7"), 
                      breaks = c("Hogsmeade", "Narnia"),
                      labels = c("HOGSMEADE", "NARNIA"), 
                      name = "Land of Magic") +                 # Can change to null if u want
    labs(title = "Species richness by plot", 
         x = "\n Plot number", y = "Number of species \n") + 
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "bottom", 
          legend.box.background = element_rect(color = "grey", size = 0.3)))

ggsave("data-vis-2/histogram.png", width = 7, height = 5, dpi = 300)


getwd()
