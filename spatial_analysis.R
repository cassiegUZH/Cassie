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
