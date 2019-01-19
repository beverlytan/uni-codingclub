# Coding club ordination tutorial 

# Loading libraries

library(vegan)
library(ape)
library(dplyr)

# Loading data 
data(varespec)

varespec %>%
  metaMDS(trace = F) %>%
  ordiplot(type = "none") %>%
  text("sites")

PCA <- rda(varespec, scale = FALSE)

barplot(as.vector(PCA$CA$eig)/sum(PCA$CA$eig)) 

sum((as.vector(PCA$CA$eig)/sum(PCA$CA$eig))[1:2]) 

plot(PCA, type = "points")

sitePCA <- PCA$CA$u # Site scores
speciesPCA <- PCA$CA$v # Species scores

biplot(PCA, choices = c(1,2), type = c("text", "points"), xlim = c(-5,10))
biplot(PCA, choices = c(1,3), type = c("text","points")) 


dist <- vegdist(varespec,  method = "bray")

library(ape)
PCOA <- pcoa(dist)


barplot(PCOA$values$Relative_eig[1:10])

PCOA <- pcoa(dist, correction = "cailliez")
        
biplot.pcoa(PCOA)

biplot.pcoa(PCOA, varespec)

PCOAaxes <- PCOA$vectors[,c(1,2)]
par(mfrow = c(1, 2)) 
biplot.pcoa(PCOA)
plot(PCA)

par(mfrow = c(1, 1)) 


dist <- vegdist(varespec,  method = "bray")

NMDS.scree <- function(x) { #where x is the name of the data frame variable
  plot(rep(1, 10), replicate(10, metaMDS(x, autotransform = F, k = 1)$stress), xlim = c(1, 10),ylim = c(0, 0.30), xlab = "# of Dimensions", ylab = "Stress", main = "NMDS stress plot")
  for (i in 1:10) {
    points(rep(i + 1,10),replicate(10, metaMDS(x, autotransform = F, k = i + 1)$stress))
  }
}

NMDS.scree(dist)
  

set.seed(2)
NMDS1 <- metaMDS(dist, k = 2, trymax = 100, trace = F)
NMDS1

NMDS2 <- metaMDS(varespec, k = 2, trymax = 100, trace = F)
NMDS2

stressplot(NMDS1)

plot(NMDS1, type = "t")
NMDS3 <- metaMDS(varespec, k = 2, trymax = 100, trace = F, autotransform = FALSE, distance="bray")
plot(NMDS3)
plot(NMDS3, display = "sites", type = "n")
points(NMDS3, display = "sites", col = "red", cex = 1.25)
text(NMDS3, display ="species")

ordiplot(NMDS3, type = "n")
orditorp(NMDS3, display = "species", col = "red", air = 0.01)
orditorp(NMDS3, display = "sites", cex = 1.1, air = 0.01)

data(varechem)
ef <- envfit(NMDS3, varechem, permu = 999)
plot(NMDS3, type = "t", display = "sites")
plot(ef, p.max = 0.05)


group = c(rep("Group1", 12), rep("Group2", 12))

colors = c(rep("red", 12), rep("blue", 12))

ordiplot(NMDS3, type = "n")
for(i in unique(group)) {
  ordihull(NMDS3$point[grep(i, group),], draw="polygon",
           groups = group[group == i],col = colors[grep(i,group)],label=F) } 

orditorp(NMDS3, display = "species", col = "red", air = 0.01)
orditorp(NMDS3, display = "sites", col = c(rep("red",12),
                                           rep("blue", 12)), air = 0.01, cex = 1.25)

