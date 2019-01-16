

library(tidyverse)

edidiv <- read.csv("getting-started/edidiv.csv")

Head(edidiv)      # capital H
tail(edidiv)
str(edidiv)
dim(edidin)       # edidiv not edidin
summary(edidiv)   

Beetle <- filter(edidiv taxonGroup == "Beetle")                      # ,
Bird <- filter(edidiv, taxonGroup = "Bird")                          # ==
Butterfly <- filter(edidiv, taxonGroup == "Buterfly")                # Butterfly
FloweringPlants <- filter(edidiv, taxonGroup == "Flowering.Plants")
Fungus <- filter(edidiv, taxonGroup == "Fungus")
Hymenopteran <- filter(edidiv, taxonGroup == "Hymenopteran")
Lichen <- filter(edidiv, taxonGroup == "Lichen")
Liverwort <- filter(edidiv, taxonGroup == Liverwort")                # "
Mammal <- filter(edidiv, taxonGroup == "Mammal")
Mollusc <- filter(edidiv, taxonGroup == "mollusc")                   # capital M

a <- length(unique(Beetle$taxonName))
b <- length(unique(Bird$taxonName))
c <- length(unique(Butterfly$taxonName))
d <- length(unique(Dragonfly$taxonName))
e <- length(unique(Flowering.Plants$taxonName))
f <- length(unique(Fungus$TaxonName))
g <- length(unique(Hymenopteran$taxon.Name))
h <- length(unique(Lichen$taxonName))
i <- length(unique(Liverwort$taxonName))
j <- length(unique(Mammal$taxonname))
k <- length(unique(Mollusc$taxonName))

biodiv <- (a,b,c,d,e,f,g,h,i,j,k)      # c()

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

barplot(as.matrix(biodiv))                 # as.matrix

barplot(biodiv, xlab="Taxa"                # Missing ,
        ylab="Number of species", ylim=c(0,900), cex.axis=1.5, cex.lab=1.5)

taxa <- c("Beetle", 
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

taxa_f <- factor(taxa)
richness <- c(a,b,c,d,e,f,g,h,i,j,k)
biodata <- data.frame(taxaf, richness)      # taxa_f

barplot(biodata$richness, names.arg=c("Beetle", 
                                      Bird",, 
                                      "Butterfly", 
                                      "Dragonfly", 
                                      "Fl.Plants", 
                                      "Fungus", 
                                      "Hymenopteran", 
                                      "Lichen", 
                                      "Liverwort", 
                                      "Mammal", 
                                      "Mollusc")),      # Double bracket
        xlab=Taxa", ylab="Number of species", ylim=c(0,600), cex.axis=1.5, cex.lab=1.5)

