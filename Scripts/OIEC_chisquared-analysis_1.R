#### Chi-squared tests to look at patterns between convergent regimes and ecology
#### created 31/10/18 by Travis Park


### red regime

## red ~ diet
red_diet <- read.csv(as.matrix("Data/OIEC_chisquared-matrix-red-diet_1.csv"))
# make first column of csv file the rownames and get rid of column X
rownames(red_diet) <- red_diet$X
red_diet <- red_diet[, 2:3]
# run chi-squared analysis for red ~ diet
red_diet_chisq <- chisq.test(red_diet)
red_diet_chisq

## red ~ habitat1 (all riverine/nearshores are classed as rivernine 
## and nearshore/oceanics are classed as nearshore)
red_habitat1 <- read.csv(as.matrix("Data/OIEC_chisquared-matrix-red-habitat1_1.csv"))
# make first column of csv file the rownames and get rid of column X
rownames(red_habitat1) <- red_habitat1$X
red_habitat1 <- red_habitat1[, 2:3]
# run chi-squared analysis for red ~ habitat1
red_habitat1_chisq <- chisq.test(red_habitat1)
red_habitat1_chisq

## red ~ habitat2 (all riverine/nearshores are classed as nearhsores 
## and nearshore/oceanics are classed as oceanics)
red_habitat2 <- read.csv(as.matrix("Data/OIEC_chisquared-matrix-red-habitat2_1.csv"))
# make first column of csv file the rownames and get rid of column X
rownames(red_habitat2) <- red_habitat2$X
red_habitat2 <- red_habitat2[, 2:3]
# run chi-squared analysis for red ~ habitat2
red_habitat2_chisq <- chisq.test(red_habitat2)
red_habitat2_chisq

## red ~ feeding1 (all raptorial/suction are classed as raptorial)
red_feeding1 <- read.csv(as.matrix("Data/OIEC_chisquared-matrix-red-feeding1_1.csv"))
# make first column of csv file the rownames and get rid of column X
rownames(red_feeding1) <- red_feeding1$X
red_feeding1 <- red_feeding1[, 2:3]
# run chi-squared analysis for red ~ feeding1
red_feeding1_chisq <- chisq.test(red_feeding1)
red_feeding1_chisq

## red ~ feeding2 (all raptorial/suction are classed as suction)
red_feeding2 <- read.csv(as.matrix("Data/OIEC_chisquared-matrix-red-feeding2_1.csv"))
# make first column of csv file the rownames and get rid of column X
rownames(red_feeding2) <- red_feeding2$X
red_feeding2 <- red_feeding2[, 2:3]
# run chi-squared analysis for red ~ feeding1
red_feeding2_chisq <- chisq.test(red_feeding2)
red_feeding2_chisq

## red ~ divetype
red_divetype <- read.csv(as.matrix("Data/OIEC_chisquared-matrix-red-divetype_1.csv"))
# make first column of csv file the rownames and get rid of column X
rownames(red_divetype) <- red_divetype$X
red_divetype <- red_divetype[, 2:3]
# run chi-squared analysis for red ~ feeding1
red_divetype_chisq <- chisq.test(red_divetype)
red_divetype_chisq


### blue regime

## blue ~ diet
blue_diet <- read.csv(as.matrix("Data/OIEC_chisquared-matrix-blue-diet_1.csv"))
# make first column of csv file the rownames and get rid of column X
rownames(blue_diet) <- blue_diet$X
blue_diet <- blue_diet[, 2:3]
# run chi-squared analysis for blue ~ diet
blue_diet_chisq <- chisq.test(blue_diet)
blue_diet_chisq

## blue ~ habitat1 (all riverine/nearshores are classed as rivernine 
## and nearshore/oceanics are classed as nearshore)
blue_habitat1 <- read.csv(as.matrix("Data/OIEC_chisquared-matrix-blue-habitat1_1.csv"))
# make first column of csv file the rownames and get rid of column X
rownames(blue_habitat1) <- blue_habitat1$X
blue_habitat1 <- blue_habitat1[, 2:3]
# run chi-squared analysis for blue ~ habitat1
blue_habitat1_chisq <- chisq.test(blue_habitat1)
blue_habitat1_chisq

## blue ~ habitat2 (all riverine/nearshores are classed as nearhsores 
## and nearshore/oceanics are classed as oceanics)
blue_habitat2 <- read.csv(as.matrix("Data/OIEC_chisquared-matrix-blue-habitat2_1.csv"))
# make first column of csv file the rownames and get rid of column X
rownames(blue_habitat2) <- blue_habitat2$X
blue_habitat2 <- blue_habitat2[, 2:3]
# run chi-squared analysis for blue ~ habitat2
blue_habitat2_chisq <- chisq.test(blue_habitat2)
blue_habitat2_chisq

## blue ~ feeding1 (all raptorial/suction are classed as raptorial)
blue_feeding1 <- read.csv(as.matrix("Data/OIEC_chisquared-matrix-blue-feeding1_1.csv"))
# make first column of csv file the rownames and get rid of column X
rownames(blue_feeding1) <- blue_feeding1$X
blue_feeding1 <- blue_feeding1[, 2:3]
# run chi-squared analysis for blue ~ feeding1
blue_feeding1_chisq <- chisq.test(blue_feeding1)
blue_feeding1_chisq

## blue ~ feeding2 (all raptorial/suction are classed as suction)
blue_feeding2 <- read.csv(as.matrix("Data/OIEC_chisquared-matrix-blue-feeding2_1.csv"))
# make first column of csv file the rownames and get rid of column X
rownames(blue_feeding2) <- blue_feeding2$X
blue_feeding2 <- blue_feeding2[, 2:3]
# run chi-squared analysis for blue ~ feeding1
blue_feeding2_chisq <- chisq.test(blue_feeding2)
blue_feeding2_chisq

## blue ~ divetype
blue_divetype <- read.csv(as.matrix("Data/OIEC_chisquared-matrix-blue-divetype_1.csv"))
# make first column of csv file the rownames and get rid of column X
rownames(blue_divetype) <- blue_divetype$X
blue_divetype <- blue_divetype[, 2:3]
# run chi-squared analysis for blue ~ feeding1
blue_divetype_chisq <- chisq.test(blue_divetype)
blue_divetype_chisq


### green regime

## green ~ diet
green_diet <- read.csv(as.matrix("Data/OIEC_chisquared-matrix-green-diet_1.csv"))
# make first column of csv file the rownames and get rid of column X
rownames(green_diet) <- green_diet$X
green_diet <- green_diet[, 2:3]
# run chi-squared analysis for green ~ diet
green_diet_chisq <- chisq.test(green_diet)
green_diet_chisq

## green ~ habitat1 (all riverine/nearshores are classed as rivernine 
## and nearshore/oceanics are classed as nearshore)
green_habitat1 <- read.csv(as.matrix("Data/OIEC_chisquared-matrix-green-habitat1_1.csv"))
# make first column of csv file the rownames and get rid of column X
rownames(green_habitat1) <- green_habitat1$X
green_habitat1 <- green_habitat1[, 2:3]
# run chi-squared analysis for green ~ habitat1
green_habitat1_chisq <- chisq.test(green_habitat1)
green_habitat1_chisq

## green ~ habitat2 (all riverine/nearshores are classed as nearhsores 
## and nearshore/oceanics are classed as oceanics)
green_habitat2 <- read.csv(as.matrix("Data/OIEC_chisquared-matrix-green-habitat2_1.csv"))
# make first column of csv file the rownames and get rid of column X
rownames(green_habitat2) <- green_habitat2$X
green_habitat2 <- green_habitat2[, 2:3]
# run chi-squared analysis for green ~ habitat2
green_habitat2_chisq <- chisq.test(green_habitat2)
green_habitat2_chisq

## green ~ feeding1 (all raptorial/suction are classed as raptorial)
green_feeding1 <- read.csv(as.matrix("Data/OIEC_chisquared-matrix-green-feeding1_1.csv"))
# make first column of csv file the rownames and get rid of column X
rownames(green_feeding1) <- green_feeding1$X
green_feeding1 <- green_feeding1[, 2:3]
# run chi-squared analysis for green ~ feeding1
green_feeding1_chisq <- chisq.test(green_feeding1)
green_feeding1_chisq

## green ~ feeding2 (all raptorial/suction are classed as suction)
green_feeding2 <- read.csv(as.matrix("Data/OIEC_chisquared-matrix-green-feeding2_1.csv"))
# make first column of csv file the rownames and get rid of column X
rownames(green_feeding2) <- green_feeding2$X
green_feeding2 <- green_feeding2[, 2:3]
# run chi-squared analysis for green ~ feeding1
green_feeding2_chisq <- chisq.test(green_feeding2)
green_feeding2_chisq

## green ~ divetype
green_divetype <- read.csv(as.matrix("Data/OIEC_chisquared-matrix-green-divetype_1.csv"))
# make first column of csv file the rownames and get rid of column X
rownames(green_divetype) <- green_divetype$X
green_divetype <- green_divetype[, 2:3]
# run chi-squared analysis for green ~ feeding1
green_divetype_chisq <- chisq.test(green_divetype)
green_divetype_chisq
