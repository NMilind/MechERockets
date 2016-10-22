### ----------------------------------------------- ###
### Author:   Nikhil Milind and Karl Westendorff    ###
### Date:     October 11 2016                       ###
### ----------------------------------------------- ###

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
graphAlts <- c()
graphMass <- seq(mass.m1 - (mass.inc * 20), mass.m1 + (mass.inc * 20), by=mass.inc)
for (i in graphMass) {
  rocket <- runSim(i)
  graphAlts <- c(graphAlts, max(rocket$Altitude))
}
fit.data <- data.frame(graphMass, graphMass^2, graphAlts)
names(fit.data) <- c("mass", "mass2", "altitude")
fit <- glm(altitude ~ mass + mass2, data=fit.data)
summary(fit)

maxMass <- (-1 * fit$coefficients[2]) / (2 * fit$coefficients[3])
maxAlt <- fit$coefficients[3] * maxMass^2 + fit$coefficients[2] * maxMass + fit$coefficients[1]

qplot(x=graphMass, y=graphAlts, main="Ballast vs. Altitude Performance", xlab="Mass of Ballast (kg)", ylab="Max Height (meters)") + geom_line(aes(x=fit.data$mass, y=predict(fit))) + geom_point(aes(x=c(maxMass), y=c(maxAlt)), colour="red", size=I(3))
print(paste("Mass: ", maxMass, " | Altitude: ", maxAlt, sep=""))
