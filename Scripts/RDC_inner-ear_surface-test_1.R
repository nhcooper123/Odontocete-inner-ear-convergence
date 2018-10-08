#--------------------------------------------------------------
# Tests for convergence of toothed whale inner ears using SURFACE
# Travis Park & Natalie Cooper 2018
#--------------------------------------------------------------

library(surface)
library(tibble)

# Read in the phylogeny and clean so that taxa match those
# in analysis
#-------------------------------------

# Read in tree
cleanwhaletree <- read.tree("Data/cleanwhaletree_1.tree")

# read in PC scores from inner ear PCA
mydata <- read.csv("Data/RDC_inner-ear_pc-scores_1.csv")

# Prepare the tree by adding node names
tree <- nameNodes(cleanwhaletree)
str(tree)

# Prepare the data by making rownames species names
# and removing that column (X in my data)
# and selecting just PC1:PC4
rownames(mydata) <- mydata$X
mydata <- mydata[, 2:4] # columns 2:5 are PC1:PC3
as.tibble(mydata)

# Set up tree and data so it's in ouch format
olist <- convertTreeData(tree, mydata)
otree <- olist[[1]]
odata <- olist[[2]]

# Run forwards selection with one regime
fwd <- surfaceForward(otree, odata, aic_threshold = 0, exclude = 0,
                      verbose = FALSE, plotaic = FALSE)

# Have a look at some outputs
# Optimal number of regimes
k <- length(fwd)

# Summary info
fsum <- surfaceSummary(fwd)
names(fsum)
fsum$aics

# Then fit the backwards search starting at kth regime of fwd
bwd <- surfaceBackward(otree, odata, starting_model = fwd[[k]], aic_threshold = 0,
                       only_best = TRUE, verbose = FALSE, plotaic = FALSE)
bsum <- surfaceSummary(bwd)
kk <- length(bwd)

# Look at the outputs
# These are standard OU model outputs
bsum$alpha
bsum$sigma_squared
bsum$theta
# This is the optimal number of regimes
bsum$n_regimes

# Plot it!
# Note this fails here if there's only one regime
surfaceTreePlot(tree, bwd[[kk]], labelshifts = TRUE)

# More plots - these won't work because there's only one regime
par(mfrow=c(1,2), mai=c(0.8,0.8,0.2,0.2))
surfaceTraitPlot(mydata, bwd[[kk]], whattraits = c(1,2))
surfaceTraitPlot(mydata, bwd[[kk]], whattraits = c(3,2))

# Set up starting models if you want to compare say BM to surface
# or a simple OU model to a complex one. Can get AIC from these etc.
bm <- startingModel(otree, odata, brownian = TRUE)
ou1 <- startingModel(otree, odata)

# Or impose some shift points (can use this to compare to other methods)
H12 <- startingModel(otree, odata,
                     shifts=c("10" = "Habitat1","25" = "Habitat2", 
                              "19" = "Habitat2"))

# Need to then run surface forwards and backwards with starting list
# defined
x <- surfaceForward(otree, odata, aic_threshold = 0, exclude = 0,
                    verbose = FALSE, plotaic = FALSE, starting_list = H12)
kkk <- length(x)
# Plot the enforced shift tree
surfaceTreePlot(tree, x[[kkk]], labelshifts = TRUE)

# Look at AICS of models with different regimes
# changing through forward and backwards phases
surfaceAICPlot(fwd = fwd, bwd = bwd)

# Can add AIC of other models to this to compare...
# eg add OU1 model AIC
abline(h = ou1[[1]]$aic, lty = "longdash")

# Run some simulations to compare results to simple models
# First simulate the data
newsim <- surfaceSimulate(tree, type = "hansen-fit", hansenfit = fwd[[k]]$fit, 
                          shifts = fwd[[k]]$savedshifts, sample_optima = TRUE)

# Plot to look at it
par(mfrow=c(1,2),mai=c(0.8,0.8,0.2,0.2))
surfaceTraitPlot(newsim$data, newsim, whattraits = c(1,2), convcol = FALSE)
surfaceTraitPlot(newsim$data, newsim, whattraits = c(3,2), convcol = FALSE)
# Run the fwd and bwd models (can combine this using runSurface)
newout<-runSurface(tree, newsim$dat, only_best = TRUE)

# Get outputs
newsum<-surfaceSummary(newout$bwd)
newkk<-length(newout$bwd)
newsum$n_regimes # this is the key thing to compare
bsum$n_regimes # this is from our bwd model

#Plot simulated results
par(mfrow=c(1,2),mai=c(0.8,0.8,0.2,0.2))
surfaceTraitPlot(newsim$data, newout$bwd[[newkk]], whattraits = c(1,2))
surfaceTraitPlot(newsim$data, newout$bwd[[newkk]], whattraits = c(3,2))

# To get p values
# repeat the step of simulating data sets (with different random number seeds) 
# many times (1000 min), and run SURFACE on them to get a null distribution 
# of deltak or c. We could calculate the statistical significance of such a test 
# as the proportion of null values that meet or exceed the observed value 
# (note that in a small data set like this statistical power is poor, 
# but that statistical properties are much better given larger trees 
# and at least two trait dimensions).
