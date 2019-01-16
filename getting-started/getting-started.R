# Coding Club Workshop 1 R Basics
# Beverly going through the workshop again before the session (16/01)

# Introductory material ---- 

# Loading packages 
library(tidyverse)

# Loading data
edidiv <- read.csv("getting-started/edidiv.csv")

# Exploring the data
head(edidiv)
tail(edidiv)
str(edidiv)
summary(edidiv)
summary(edidiv$taxonGroup)

# Converting character to factor 
edidiv$taxonGroup <- as.factor(edidiv$taxonGroup)


# Calculating species richness ---- 

# Filter out the data for each taxon group 

Beetle <- filter(edidiv, taxonGroup == "Beetle")
Bird <- filter(edidiv, taxonGroup == "Bird")
Butterfly <- filter(edidiv, taxonGroup == "Butterfly")
Dragonfly <- filter(edidiv, taxonGroup == "Dragonfly")
Flowering.Plants <- filter(edidiv, taxonGroup == "Flowering.Plants")
Fungus <- filter(edidiv, taxonGroup == "Fungus")
Hymenopteran <- filter(edidiv, taxonGroup == "Hymenopteran")
Lichen <- filter(edidiv, taxonGroup == "Lichen")
Liverwort <- filter(edidiv, taxonGroup == "Liverwort")
Mammal <- filter(edidiv, taxonGroup == "Mammal")
Mollusc <- filter(edidiv, taxonGroup == "Mollusc")

# Find out the number of different species in each taxa

a <- length(unique(Beetle$taxonName))
b <- length(unique(Bird$taxonName))
c <- length(unique(Butterfly$taxonName))
d <- length(unique(Dragonfly$taxonName))
e <- length(unique(Flowering.Plants$taxonName))
f <- length(unique(Fungus$taxonName))
g <- length(unique(Hymenopteran$taxonName))
h <- length(unique(Lichen$taxonName))
i <- length(unique(Liverwort$taxonName))
j <- length(unique(Mammal$taxonName))
k <- length(unique(Mollusc$taxonName))

# Creating vector, then plotting ---- 

# Combining into one vector

biodiv <- c(a,b,c,d,e,f,g,h,i,j,k)

# Add relevant labels

names(biodiv) <- c("Beetle", 
                   "Bird", 
                   "Butterfly", 
                   "Dragonfly", 
                   "Fl.Plants", 
                   "Fungus", 
                   "Hymenopteran", 
                   "Lichen", 
                   "Liverwort", 
                   "Mammal", 
                   "Mollusc")

View(biodiv)

# Plot the barplot 

barplot(biodiv, xlab="Taxa", ylab="Number of species", ylim=c(0,600), cex.names= 1.5, cex.axis=1.5, cex.lab=1.5)

# Creating dataframe, then plotting ---- 

# Creating the dataframe

taxa <- c("Beetle",
          "Bird",
          "Butterfly",
          "Dragonfly",
          "Flowering.Plants",
          "Fungus",
          "Hymenopteran",
          "Lichen",
          "Liverwort",
          "Mammal",
          "Mollusc")

taxa_f <- factor(taxa)
richness <- c(a,b,c,d,e,f,g,h,i,j,k)
biodata <- data.frame(taxa_f, richness)

View(biodata)

# Plotting the barplot 

barplot(biodata$richness, names.arg=c("Beetle",
                                      "Bird",
                                      "Butterfly",
                                      "Dragonfly",
                                      "Flowering.Plants",
                                      "Fungus",
                                      "Hymenopteran",
                                      "Lichen",
                                      "Liverwort",
                                      "Mammal",
                                      "Mollusc"),
        xlab="Taxa", ylab="Number of species", ylim=c(0,600))
