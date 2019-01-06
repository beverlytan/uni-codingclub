# Coding club tutorial: data visualization 2 

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
  geom_histogram())

ggsave("data-vis-2/histwrong1.png")

# Fixing the counts 

(hist <- ggplot(species_counts, aes(x = plot, y = Species_number)) +
    geom_histogram(stat = "identity"))

ggsave("data-vis-2/histwrong2.png")

# Splitting the bar into the different locations, stacked

(hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
  geom_histogram(stat = "identity"))

ggsave("data-vis-2/histwrong3.png")

# Splitting the bar into the different locations, not stacked

(hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge"))

ggsave("data-vis-2/histbase.png")

# Having the plot numbers show 1,2,3,4,5,6

(hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") + 
    scale_y_continuous(limits = c(0, 50)) +
    scale_x_continuous(breaks = c(1,2,3,4,5,6)))

ggsave("data-vis-2/histbase2.png")

# Beautifying the graph: adding title, subtitle, caption, axis labels 

(hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") +
    scale_y_continuous(limits = c(0, 50)) +
    scale_x_continuous(breaks = c(1,2,3,4,5,6)) + 
    labs(title = "Species richness by plot", 
         subtitle = "In the magical lands",
         caption = "Data from the Ministry of Magic", 
         x = "Plot number", y = "Number of species"))

ggsave("data-vis-2/histbeaut1a.png")

(hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") +
    scale_y_continuous(limits = c(0, 50)) +
    scale_x_continuous(breaks = c(1,2,3,4,5,6)) + 
    labs(title = "Species richness by plot", 
         x = "\n Plot number", y = "Number of species \n") + 
    theme(axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12, face = "italic"), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold")))

ggsave("data-vis-2/histbeaut1b.png")

# Beautifying graph: plot background - white, and remove lines 

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
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold")))

ggsave("data-vis-2/histbeaut2.png")

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

ggsave("data-vis-2/histbeaut-final.png")





# Creating boxplots ---- 

yearly_counts <- magic_veg %>%
  group_by(land, plot, year) %>%  
  summarise(Species_number = length(unique(species))) %>%
  ungroup() %>%
  mutate(plot = as.factor(plot))

View(yearly_counts)

(boxplot <- ggplot(yearly_counts, aes(plot, Species_number, fill = land)) +
  geom_boxplot())

ggsave("data-vis-2/boxbase.png")

(boxplot <- ggplot(yearly_counts, aes(x = plot, y = Species_number, fill = land)) +
  geom_boxplot() +
  scale_x_discrete(breaks = 1:6) +
  scale_fill_manual(values = c("rosybrown1", "#deebf7"),
                    breaks = c("Hogsmeade","Narnia"),
                    name="Land of magic",
                    labels=c("Hogsmeade", "Narnia")) +
  labs(title = "Species richness by plot", 
       x = "\n Plot number", y = "Number of species \n") + 
  theme_bw() + 
  theme() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12), 
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
        plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
        legend.position = "bottom", 
        legend.box.background = element_rect(color = "grey", size = 0.3)))

ggsave("data-vis-2/boxbeaut1.png")

ggsave("data-vis-2/magical-sp-rich-boxplot1.png", width = 7, height = 5, dpi = 300)

# If I wanted Narnia to come before Hogsmeade

yearly_counts$land <- factor(yearly_counts$land, 
                             levels = c("Narnia", "Hogsmeade"),
                             labels = c("Narnia", "Hogsmeade"))

(boxplot <- ggplot(yearly_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_boxplot() +
    scale_x_discrete(breaks = 1:6) +
    scale_fill_manual(values = c("#deebf7", "rosybrown1"),
                      breaks = c("Narnia","Hogsmeade"),
                      name = "Land of magic",
                      labels = c("Narnia", "Hogsmeade")) +
    labs(title = "Species richness by plot", 
         x = "\n Plot number", y = "Number of species \n") + 
    theme_bw() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "bottom", 
          legend.box.background = element_rect(color = "grey", size = 0.3)))

ggsave("data-vis-2/boxbeaut2.png")

# If I wanted to reorder plot number to be 6 first, then 1234, 

yearly_counts$plot <- factor(yearly_counts$plot, 
                             levels = c("6", "1", "2", "3", "4", "5"),
                             labels = c("6", "1", "2", "3", "4", "5"))

(boxplot2 <- ggplot(yearly_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_boxplot() +
    scale_x_discrete(breaks = 1:6) +
    scale_fill_manual(values = c("#deebf7", "rosybrown1"),
                      breaks = c("Narnia","Hogsmeade"),
                      name = "Land of magic",
                      labels = c("Narnia", "Hogsmeade")) +
    labs(title = "Species richness by plot", 
         x = "\n Plot number", y = "Number of species \n") + 
    theme_bw() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "bottom", 
          legend.box.background = element_rect(color = "grey", size = 0.3)))

ggsave("data-vis-2/boxbeaut3.png")

# Mixed effect model results ----

heights <- magic_veg %>%
  filter(!is.na(height)) %>%  
  group_by(year, land, plot, id) %>%
  summarise(Max_Height = max(height)) %>%  # Calculating max height
  ungroup() %>%  # Need to ungroup so that the pipe doesn't get confused
  group_by(year, land, plot) %>%
  summarise(Height = mean(Max_Height))  # Calculating mean max height

# Basic scatterplot 

ggplot(heights, aes(year, Height, colour = land)) +
  geom_point() +
  theme_bw()

# Basic scatterplot with line through 

ggplot(heights, aes(year, Height, colour = land)) +
  geom_point() +
  theme_bw() +
  stat_smooth(method = "lm")

library(nlme)

# Using the square brackets to subset the data just for Hogsmeade
lm_heights <- lme(Height ~ year, random = ~1|year/plot, 
                  data = heights[heights$land == "Hogsmeade",])

summary(lm_heights)

# Using the square brackets to subset the data just for Narnia
lm_heights2 <- lme(Height ~ year, random = ~1|year/plot, 
                   data = heights[heights$land == "Narnia",])

summary(lm_heights2)

## Predictions for Hogsmeade

# Create a blank dataset with the years we want

mm.heights <- expand.grid(year = seq(1999, 2016, 1), Height = 0)  

# Create matrix of relevant effect sizes

mm <- model.matrix(terms(lm_heights), mm.heights)  

# Calculate height based on the relevant effect sizes

mm.heights$Height <- mm %*% fixef(lm_heights)  

pvar.mm.heights <- diag(mm %*% tcrossprod(vcov(lm_heights), mm))

# Add standard errors to the dataframe

mm.heights <- data.frame(mm.heights, 
                         plo = mm.heights$Height - 1.96*sqrt(pvar.mm.heights),
                         phi = mm.heights$Height + 1.96*sqrt(pvar.mm.heights))  

## Predictions for Narnia

# Create a blank dataset with the years we want

mm.heights2 <- expand.grid(year = seq(1999, 2016, 1), Height = 0)  

mm2 <- model.matrix(terms(lm_heights2), mm.heights2)  

mm.heights2$Height <- mm2 %*% fixef(lm_heights2)  

pvar.mm.heights2 <- diag(mm2 %*% tcrossprod(vcov(lm_heights2), mm2))

mm.heights2 <- data.frame(mm.heights2, 
                          plo = mm.heights2$Height - 1.96*sqrt(pvar.mm.heights2),
                          phi = mm.heights2$Height + 1.96*sqrt(pvar.mm.heights2)) 

ggplot(heights, aes(year, Height)) +
  geom_ribbon(data = mm.heights, mapping = aes(x = year, ymin = plo, ymax = phi)) +
  geom_line(data = mm.heights, mapping = aes(x = year)) +
  geom_ribbon(data = mm.heights2, mapping = aes(x = year, ymin = plo, ymax = phi)) +
  geom_line(data = mm.heights2, mapping = aes(x = year)) +
  geom_point(data = heights, aes(colour = factor(land))) +
  theme_bw()

(ggplot(heights, aes(year, Height)) +
  geom_ribbon(data = mm.heights, mapping = aes(x = year, ymin = plo, ymax = phi),
              fill = "rosybrown1", alpha = 0.4) +  
  geom_line(data = mm.heights, mapping = aes(x = year),
            colour = "#ff9999", size = 1) +  
  geom_ribbon(data = mm.heights2, mapping = aes(x = year, ymin = plo, ymax = phi), 
              fill = "#deebf7", alpha = 0.4) +
  geom_line(data = mm.heights2, mapping = aes(x = year), 
            colour = "#99b3ff", size = 1) +
  geom_point(data = heights, aes(colour = factor(land)), alpha = 0.8) + 
  scale_x_continuous(breaks = c(1999, 2004, 2009, 2013, 2014, 2015, 2016)) + 
  scale_colour_manual(values = c("#ff9999", "#99b3ff"),
                      breaks = c("Hogsmeade","Narnia"),
                      name = "Land of magic",
                      labels = c("Hogsmeade","Narnia")) + 
  labs(title = "Change in canopy heights from 1999 - 2016 in the land of magic", 
       x = "Year", y = "Mean canopy height") + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12), 
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
        plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
        legend.text = element_text(size = 11),
        legend.title = element_text(face = "bold"),
        legend.position = "bottom", 
        legend.box.background = element_rect(color = "grey", size = 0.3)))
  
  

