library(vegan)

# geographical distance
# file as in testgeo 
rownames(testgeo) <- c("a", "b", "c")
# euclidean distance in degrees
geodist <- vegdist(testgeo, method = "euclidean", diag=F, upper=F)
geodist

# cultural distance
# binary data as testcult; columns=presence or absence of trait
# method "jaccard" and "gower" give different results: we need to discuss which is best 
rownames(testcult) <- c("a", "b", "c", "d", "e")
cultdist <- vegdist(testcult, method = "jaccard", diag=F, opper=F)
cultdist 

# genetic distance
# seems you already have a matrix, so you need to convert it 
# if you start with testgen
gendist <- as.dist(testgen, diag=F, upper=F)
gendist

# calculate binary correlations between the distances
# each population pair has a distance: make a single dataframe with all and correlate
distframe <- data.frame(geo=as.vector(geodist),
                        cult=as.vector(cultdist),
                        gen=as.vector(gendist))
library(Hmisc)
corrs <- rcorr(as.matrix(distframe), type="spearman")
corrs
corrs$r
corrs$P

# simple Mantel test
detach(package:vegan) # because vegn also has a Mantel function
library(ecodist)
# mantelr is correlation, pval3 is two-tailed p value
# ?mantel 
mantel(geodist ~ cultdist,nperm = 10000, mrank = T)

# partial Mantel test
# with y ~ x1 + x2, you calculate correlation between 
# y and x1 controlling for x2 
mantel(geodist ~ cultdist + gendist, nperm = 10000, mrank = T)


## Code for preparation of real data

  d.real <- read.csv("d.tools3.csv", sep = ";")
  d.genes <- read.csv("d.genes3.csv", sep = ";")

# Geographical distance
  m.geo <- distinct(d.real[, c(1,4,5)], population, .keep_all = T)
  rownames(m.geo) <- m.geo$population
  m.geo <- m.geo[,-1]
  geodist <- vegdist(m.geo, method = "euclidean", diag=F, upper=F)
  geodist

# cultural distance
  m.cult <- distinct(d.real[, c(1,6,11)], population, tool, .keep_all = T)
  m.cult <- tidyr::spread(m.cult, tool, use)
  rownames(m.cult) <- m.cult$population
  m.cult <- m.cult[,-1]
  cultdist <- vegdist(m.cult, method = "jaccard", diag=F, upper=F, na.rm = T)
  cultdist

# genetic distance
  m.ibd <- d.genes[, c(2,3,5)]
  m.ibd<- tidyr::spread(m.ibd, population1, IBDmax)
  m.ibd <- rbind(c(NA), m.ibd)
  m.ibd$population2[1] <- "Bafing"
  rownames(m.ibd) <- m.ibd$population2
  m.ibd <- m.ibd[,-1]
  m.ibd <- cbind(m.ibd, c(NA))
  colnames(m.ibd)[ncol(m.ibd)] <- "Tai"
  ibddist <- as.dist(m.ibd, diag=F, upper=F)
  ibddist
  
  m.sim <- d.genes[, c(2,3,6)]
  m.sim<- tidyr::spread(m.sim, population1, similarity)
  m.sim <- rbind(c(NA), m.sim)
  m.sim$population2[1] <- "Bafing"
  rownames(m.sim) <- m.sim$population2
  m.sim <- m.sim[,-1]
  m.sim <- cbind(m.sim, c(NA))
  colnames(m.sim)[ncol(m.sim)] <- "Tai"
  simdist <- as.dist(m.sim, diag=F, upper=F)
  simdist


