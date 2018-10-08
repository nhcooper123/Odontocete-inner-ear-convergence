#--------------------------------------------------------------
# Geometric morphometric analyses and tests for convergence of river dolphin cochleae
# Travis Park 2018
# Modified by Natalie Cooper 2018

# STEP 1: Input and ordinate shape data
# Input coordinate data
# GPA (Generalised Procrustes Analysis)

# STEP 2: Tests for convergence 
# Procrustes ANOVA on GPA coordinates with phylogenetic correction
# PCA to explore shape variation
# Phylogenetic ANOVA on first three principal components (PCs) individually to see which PCs are significantly different
# Stayton's distance-based metric

#-------------------------------------
# Setup
#-------------------------------------

# Load libraries
library(ape)
library(rgl)
library(geomorph)
library(geiger)
library(maps)
library(phytools)
library(BAMMtools)
library(convevol)
library(dispRity)

#----------------------------------------------
# STEP 1: Input and ordinate shape data
#----------------------------------------------

## Load in .pts files

# These are the landmark points
# Define total no. of landmarks and curve semi-LMs
n.landmarks <- 371

# Define spatial dimensionality of data (2-D or 3-D)
n.dim <- 3  

# Define path to folder where the data are
mypath <- "Landmarks/L371-cochlea-dataset/RDC_L371-cochlea_pts"

## Import data
# This is code from Ryan Felice, orignallly written by Anne-Claire Fabre
# Makes a list of the files to import
# These use unmodified .pts data format from IDAV Landmark
ptslist <- dir(path = mypath, pattern = '.pts', recursive = TRUE) 

# Make a matrix of the .pts files
# dimensions = number of landmarks and semilandmarks, n.dim = 2D/3D, number of specimens)
ptsarray <- array(dim = c(n.landmarks, n.dim, length(ptslist)))

# Loop through reading in each file
for (i in 1:length(ptslist)) {
  ptsarray[, , i] <-
    as.matrix(read.table(
      file = paste0(mypath, "/" , ptslist[i]),
      skip = 2,
      header = FALSE,
      sep = "",
      row.names = 1
    ))
}

#--------------------------------------------------------------------

## Load in sliding matrix for GPA

slidematrix <- as.matrix(read.csv("Data/Sliding-matrix_L371-cochlea-dataset_1.csv"))

# Read in metadata on specimens
ds <- read.csv("Data/OIEC_meta-data_1.csv")

## Perform GPA
RDC_gpa <- gpagen(ptsarray, curves = slidematrix)

# assign taxon names to gpa coords
dimnames(RDC_gpa$coords)[[3]] <- ds$Taxon


#-------------------------------------
# STEP 2: Testing for convergence
#-------------------------------------
# Read in the phylogeny and add to data
# for further analyses
#-------------------------------------
# Read in tree
whaletree <- read.nexus("Data/Steeman-et-al-2009.nex")

# drop tips from tree that aren't part of dataset
droppedtips <- c("Balaena_mysticetus",
                 "Balaenoptera_physalus", 
                 "Eubalaena_australis", 
                 "Eubalaena_glacialis", 
                 "Eubalaena_japonica", 
                 "Caperea_marginata", 
                 "Eschrichtius_robustus", 
                 "Balaenoptera_acutorostrata", 
                 "Balaenoptera_bonaerensis", 
                 "Megaptera_novaeangliae", 
                 "Balaenoptera_musculus",
                 "Balaenoptera_omurai", 
                 "Balaenoptera_borealis", 
                 "Balaenoptera_brydei", 
                 "Balaenoptera_edeni",
                 "Platanista_minor", 
                 "Berardius_bairdii", 
                 "Indopacetus_pacificus",
                 "Hyperoodon_planifrons", 
                 "Mesoplodon_traversii", 
                 "Mesoplodon_ginkgodens", 
                 "Mesoplodon_europaeus", 
                 "Mesoplodon_bowdoini", 
                 "Mesoplodon_carlhubbsi",  
                 "Mesoplodon_layardii", 
                 "Mesoplodon_densirostris", 
                 "Mesoplodon_stejnegeri", 
                 "Mesoplodon_perrini", 
                 "Mesoplodon_peruvianus", 
                 "Phocoena_sinus", 
                 "Globicephala_macrorhynchus",
                 "Lissodelphis_borealis", 
                 "Lissodelphis_peronii", 
                 "Cephalorhynchus_hectori", 
                 "Cephalorhynchus_eutropia", 
                 "Stenella_clymene", 
                 "Stenella_frontalis", 
                 "Delphinus_tropicalis", 
                 "Delphinus_capensis")
cleanwhaletree <- drop.tip(whaletree, droppedtips)

# combine data into dataframe for further analysis

gdf <- geomorph.data.frame(coords = RDC_gpa$coords, phy = cleanwhaletree, group = ds$Regime)
gdf

#-------------------------------------------------
# Calculate phylogenetic signal (multivariate K)
# to test whether there is phylogenetic signal in
# mandible shape of the species
#-------------------------------------------------
phy1 <- physignal(gdf$coords, cleanwhaletree)
summary(phy1)


#------------------------------------------------------
# Run MANOVAs on GPA coords with phylogeny accounted for
# to determine if river dolphins are significantly 
# different to other dolphins
#-------------------------------------------------------

# GPA MANOVA with randomize residuals RRPP method
manova <- procD.pgls(coords ~ group, phy = cleanwhaletree, data = gdf, iter =  999) 
summary(manova)

# Plot resulting phylomorphospace
plotGMPhyloMorphoSpace(gdf$phy, gdf$coords, tip.labels = TRUE, node.labels = TRUE,
                       ancStates = TRUE, xaxis = 1, yaxis = 2, zaxis = NULL, 
                       plot.param = list(), shadow = FALSE)

#------------------------------------
# Principal components analysis (PCA)
#------------------------------------
# PCA 
RDC_pca <- plotTangentSpace(RDC_gpa$coords, 
                            groups = ds$Regime,
                            legend = TRUE, label = TRUE)

# Summary of variance associated with each PCA axis
RDC_pca$pc.summary

# PC scores for each species
RDC_pca$pc.scores

# Show barplot of variance explained by each PC
barplot(RDC_pca$pc.summary$sdev^2 / 
          sum(RDC_pca$pc.summary$sdev^2))

# Eigenvectors
RDC_pca$rotation

# Create dataframe for further analysis
RDC_df <- data.frame(RDC_pca$pc.scores,
                     species = ds$Taxon)


#----------------------------------------------
# Stayton's distance-based convergence metrics
#----------------------------------------------
# Create data object for convevol functions
# This selects just the first 31 PCs (i.e. the number of PC's that = 95% of the variation)
RDC <- as.matrix(array(RDC_df[, 1:6]))

# Select tips that are river dolphins
convtips <- c("Kogia_sima",
               "Kogia_breviceps",
               "Phocoenoides_dalli")

# Run analyses and 
RDC_analysis <- convrat(phyl = cleanwhaletree,
                        phendata = RDC,
                        convtips = convtips)

# Look at output
RDC_analysis

# Get significance values
# Takes a while to run
pval <- convratsig(phyl = cleanwhaletree,
                   phendata = RDC,
                   convtips = convtips,
                   nsim = 1000)

# Look at output
pval
