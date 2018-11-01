### looking at PC variation using wireframe and mesh-warping methods
### created 17/10/2018 by Travis Park

library(geomorph)
library(Morpho)
library(landvR)

## need to have alrfeady run the GPA and PCA for the OIEC analysis

# find the mean species (it's Orcinus orca - number 28)
meanspec <- findMeanSpec(OIEC_gpa$coords)

# assign it to an object to act as a reference point to compare to the PC extremes
ref <- OIEC_gpa$coords[, , meanspec]

## need to create a 3d mesh of the mean cochlea shape

# find the mean cochlear shape using the GPA coords
meanshape <- mshape(OIEC_gpa$coords)

# Load in the orca ply file to act as a reference point to warp into the mean mesh shape
refmesh <- read.ply("Landmarks/L371-cochlea-dataset/RDC_L371-cochlea_pts/ECHO_L371-cochlea_ply/Orcinus-orca_NHMUK1927.28_3.ply")

# load in the orca ply coords
refcoords <- read.pts("Landmarks/L371-cochlea-dataset/RDC_L371-cochlea_pts/Orcinus-orca_NHMUK1927.28_3.pts")

# warp the orca mesh into the mean mesh
meanmesh <- warpRefMesh(mesh = refmesh, mesh.coord = refcoords, ref = meanshape)


## look at PC variation

# create wireframes of PC extremes
plotRefToTarget(ref, OIEC_pca$pc.shapes$PC1min)
plotRefToTarget(ref, OIEC_pca$pc.shapes$PC1max)
plotRefToTarget(ref, OIEC_pca$pc.shapes$PC2min)
plotRefToTarget(ref, OIEC_pca$pc.shapes$PC2max)

# create vectors showing of PC extremes
plotRefToTarget(ref, OIEC_pca$pc.shapes$PC1min, method = "vector")
plotRefToTarget(ref, OIEC_pca$pc.shapes$PC1max, method = "vector")
plotRefToTarget(ref, OIEC_pca$pc.shapes$PC2min, method = "vector")
plotRefToTarget(ref, OIEC_pca$pc.shapes$PC2max, method = "vector")



# create points showing difference between mean species and PC extremes
plotRefToTarget(ref, OIEC_pca$pc.shapes$PC1min, method = "points")
plotRefToTarget(ref, OIEC_pca$pc.shapes$PC1max, method = "points")
plotRefToTarget(ref, OIEC_pca$pc.shapes$PC2min, method = "points")
plotRefToTarget(ref, OIEC_pca$pc.shapes$PC2max, method = "points")

# create 3d mesh of the PC extremes based on the orca refmesh
plotRefToTarget(ref, OIEC_pca$pc.shapes$PC1min, mesh = refmesh, method = "surface")
plotRefToTarget(ref, OIEC_pca$pc.shapes$PC1max, mesh = refmesh, method = "surface")
plotRefToTarget(ref, OIEC_pca$pc.shapes$PC2min, mesh = refmesh, method = "surface")
plotRefToTarget(ref, OIEC_pca$pc.shapes$PC2max, mesh = refmesh, method = "surface")


## Using landvR to look at landmark variation

# transform data into 2d array
array <- two.d.array(OIEC_gpa$coords)

# have to use prcomp to ordinate data for this analysis
ordination <- stats::prcomp(array)

# calculate the coordinate differences between the two PC extremes
PC1var <- variation.range(OIEC_gpa, axis = 1, ordination = ordination, type = "spherical", angle = "degree", what = "radius")
PC1var

# assign coordinates of PC extremes to their oen objects for plotting
hyp1 <- OIEC_pca$pc.shapes$PC1min
hyp2<- OIEC_pca$pc.shapes$PC1max

# plot range of PC variation using a heat colour mode
procrustes.var.plot(hyp1, hyp2, col = heat.colors, col.val = PC1var[, "radius"], labels = TRUE)



