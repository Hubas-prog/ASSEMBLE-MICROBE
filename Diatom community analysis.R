###   Diatom community analysis  ###

# Packages
library(vegan)
library(readxl)
library(RVAideMemoire)
# Dataframe
diatoms <- read_excel("Diatom assemblage.xlsx")
diatoms2 <- diatoms[,2:83]

# Non-metric multidimensional scaling

# Stress
s1 <- metaMDS(diatoms2, autotransform = F, k = 1)$stress
s2 <- metaMDS(diatoms2, autotransform = F, k = 2)$stress
s3 <- metaMDS(diatoms2, autotransform = F, k = 3)$stress
s4 <- metaMDS(diatoms2, autotransform = F, k = 4)$stress
s5 <- metaMDS(diatoms2, autotransform = F, k = 5)$stress
s6 <- metaMDS(diatoms2, autotransform = F, k = 6)$stress
s7 <- metaMDS(diatoms2, autotransform = F, k = 7)$stress

par(mar=c(4.5,4.5,3,3))
plot(c(s1, s2, s3, s4, s5, s6, s7), pch = 16, xlab = "Number of dimensions", ylab = "Stress")

# Stress plot for 2 dimensions
NMDS2 <- metaMDS(comm = diatoms2, k = 2, trymax = 100, trace = F, autotransform = F, distance = "bray")
stressplot(NMDS2)
# Plot
par(mar=c(5.8,5,2,2)+0.1)
plot(NMDS2$points, type = "n", cex.axis = 1.5, ylab = "NMDS2", xlab = "NMDS1", cex.lab = 1.5)
treatment <- c(rep("Control", 3), rep("A1", 3), rep("A2", 3), rep("B1", 3), rep("B2", 3))
colours <- c(rep("black", 3), rep("blue", 3), rep("green", 3), rep("red", 3), rep("orange", 3))
for(i in unique(treatment)) {
  ordihull(NMDS2$point[grep(i, treatment),], draw="polygon",
           groups = treatment[treatment == i],col = colours[grep(i,treatment)],label=F, pch = c(1,2, 3,4)) 
}

# PERMANOVA
adonis(diatoms[,2:83] ~ diatoms$Treatment, permutations = 999, method = "bray")

# Pairwise posthoc PERMANOVA
pairwise.perm.manova(vegdist(diatoms[2:83],method = "bray"), 
                     diatoms$Treatment, nperm = 999, p.method = "none")


