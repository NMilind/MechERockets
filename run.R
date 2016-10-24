### ----------------------------------------------- ###
### Author:   Nikhil Milind and Karl Westendorff    ###
### Date:     October 11 2016                       ###
### ----------------------------------------------- ###

setwd("~/Desktop/R-Code/MechERockets")
source("rocket.R")

rocket <- runSim(0)

NEGATIVE_MASS = TRUE

alt.max1 <- 0
alt.max2 <- 0
mass.m1 <- 0
mass.m2 <- 0
mass.inc <- mass$ballast.inc
mass.dir <- 1
while (TRUE) {
  alt.max1 <- alt.max2
  mass.m1 <- mass.m2
  mass.m2 <- mass.m1 + (mass.inc * mass.dir)
  if (abs(mass.m2 - mass.m1) <= 0.00001) {
    break;
  }
  rocket <- runSim(mass.m2)
  alt.max2 <- max(rocket$Altitude)
  print(alt.max2)
  if (mass.dir == 1 && (alt.max2 - alt.max1) < 0) {
    mass.dir <- -1
    mass.inc <- mass.inc / 2
  }
  else if (mass.dir == -1 && (alt.max2 - alt.max1) < 0 || (mass.m2 <= 0 && !NEGATIVE_MASS)) {
    mass.dir <- 1
    mass.inc <- mass.inc / 2
  }
}

### GRAPH ANALYSIS
plotRawData()
