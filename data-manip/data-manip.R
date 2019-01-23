# Beverly is doing coding club tutorial before the workshop on 23 Jan

# Introductory things ----

# Loading packages

library(tidyverse)

# Load data 

elongation <- read.csv("data-manip/EmpetrumElongation.csv", sep = ";")
germination <- read.csv("data-manip/Germination.csv", sep = ";")

# Learning about gather and spread ---- 

elongation_long <- gather(elongation, Year, Length, 
                          c(X2007, X2008, X2009, X2010, X2011, X2012))

elongation_wide <- spread(elongation_long, Year, Length) 

# Creating a boxplot

boxplot(Length ~ Year, data = elongation_long, 
        xlab = "Year", ylab = "Elongation (cm)", 
        main = "Annual growth of Empetrum hermaphroditum")

# Learning about filter, select, mutate, summarise and group_by ---- 

# Filter specific species, SR 
germinSR <- filter(germination, Species == 'SR')

# Select specific columns
germin_clean <- select(germination, Species, Treatment, Nb_seeds_germin)

# Adding a new column with mutate 
germin_percent <- mutate(germination, Percent = Nb_seeds_germin / Nb_seeds_tot * 100)

# Calculating summary statistics with summarise and group_by 
germin_average <- summarise(germin_percent, Germin_average = mean(Percent))
germin_grouped <- group_by(germin_percent, Species, Treatment) 
germin_summary <- summarise(germin_grouped, Average = mean(Percent))

# Learning how to use pipes ---- 

# Performing the above analyses with pipes

germin_summary <- germination %>%  
  mutate(Percent = Nb_seeds_germin/Nb_seeds_tot * 100) %>% 
  group_by(Species, Treatment) %>%  
  summarise(Average = mean(Percent), SD = sd(Percent)) %>%  
  ungroup()

# Other functions in dplyr ---- 

# Filter across multiple columns

potential_outlier <- filter_all(elongation, any_vars(. == 8.4))

elongation_df2 <- filter_all(elongation, any_vars(. < 8.4))

elongation_df3 <- filter_all(elongation, any_vars(. != 8.4))

# Filter for multiple criteria at the same time

control_vaccinium <- filter(germination, Treatment %in% c('Control', 'Vaccinium'))

# Summarising your data 

mean_elongation <- summarise_all(elongation, mean)

year_mean <- summarise_at(elongation, vars(contains("20")), mean)

numeric_mean <- summarise_if(germination, is.numeric, mean)

# Reordering and renaming factosr 

(germination$Species <- factor(germination$Species,
                               levels = c("SR", "SP"),
                               labels = c("Salix richardsonii", "Salix pulchra")))

# Joining the datasets together 

germination <- rename(germination, Zone = Block)

germination2 <- inner_join(germination, elongation, by = "Zone")

View(germination2)
