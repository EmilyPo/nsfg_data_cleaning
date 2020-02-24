# "population" survey weights
summary(nsfg$weight)

# "sample" survey weights (new weights = old weight*(sampleSize/popSize))
# popSize here being the sum of the weights
summary(nsfg_complete$weight)

# calculate ppopsize: 3 * (sampleSize/minWeight) (from ergm.ego tutorial)
ppopsize <- 3*(sum(nsfg_complete$weight)/min(nsfg_complete$weight)); ppopsize

# calculate ppopsize: ppopsize = 3/minWeight (from MM's issue comment)
ppopsize2 <- 3*min(nsfg_complete$weight); ppopsize2