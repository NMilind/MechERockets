### ----------------------------------------------- ###
### Author:   Nikhil Milind and Karl Westendorff    ###
### Date:     October 11 2016                       ###
### ----------------------------------------------- ###

plotInitialData <- function() {
  
  ### DATA CHECK ###
  plots.thrustCurve <- qplot(thrust$time, thrust$thrust, main="Thrust Curve for B6 Rocket", xlab="Time (sec)", ylab="Thrust (N)", size=I(0.5)) + geom_line()
  plots.densityCurve <- qplot(density$altitude, density$air.density, main="Density Curve", xlab="Altitude (meters)", ylab="Density (kg/m^3)", size=I(0.5)) + geom_line()
  thrust.testCurve <- c()
  pg <- progress_bar$new(total = (1 - 0) / 0.005)
  print("Thrust Curve Run")
  for (i in seq(0,1,by=0.005)) {
    thrust.testCurve <- c(thrust.testCurve, thrust.curve(i))
    pg$tick()
  }
  plots.thrustRun <- qplot(x=seq(0, 1, by=0.005), y=thrust.testCurve, main="Thrust Curve Sample Run", xlab="Time (sec)", ylab="Thrust (N)", size=I(0.5)) + geom_line()
  density.testCurve <- c()
  pg <- progress_bar$new(total = (8000 + 1000) / 100)
  print("Density Curve Run")
  for (i in seq(-1000, 8000, by=100)) {
    density.testCurve <- c(density.testCurve, density.curve(i))
    pg$tick()
  }
  plots.densityRun <- qplot(x=seq(-1000, 8000, by=100), y=density.testCurve, main="Density Curve Sample Run", xlab="Altitude (m)", ylab="Density (kg/m^3)", size=I(0.5)) + geom_line()
  x11()
  grid.arrange(plots.thrustCurve, plots.thrustRun, plots.densityCurve, plots.densityRun, ncol=2)
  mass.testCurve <- c()
  for (i in seq(0, 1, by=0.005)) {
    mass.testCurve <- c(mass.testCurve, mass.curve(i))
    pg$tick()
  }
  plots.massRun <- qplot(x=seq(0, 1, by=0.005), y=mass.testCurve, main="Mass Curve Sample Run", xlab="Time (sec)", ylab="Mass of Rocket (kg)", size=I(0.5)) + geom_line()
  x11()
  grid.arrange(plots.massRun, ncol=1)
}

plotResults <- function(rocket) {
  
  ### RESULT VISUALIZATION ###
  plots.thrust <- qplot(rocket$Time, rocket$FNet, main="Rocket Net Force", xlab="Time (sec)", ylab="Force (N)", size=I(0.5)) + geom_line()
  plots.acceleration <- qplot(rocket$Time, rocket$Acceleration, main="Rocket Acceleration", xlab="Time (sec)", ylab="Acceleration (m/s^2)", size=I(0.5)) + geom_line()
  plots.velocity <- qplot(rocket$Time, rocket$Velocity, main="Rocket Velocity", xlab="Time (sec)", ylab="Velocity (m/s)", size=I(0.5)) + geom_line()
  plots.altitude <- qplot(rocket$Time, rocket$Altitude, main="Rocket Altitude", xlab="Time (sec)", ylab="Altitude (m)", size=I(0.5)) + geom_line()
  grid.arrange(plots.thrust, plots.acceleration, plots.velocity, plots.altitude, ncol=2)
}

plotRawData <- function() {
  
  graphics.off()
  graphAlts <- c()
  graphMass <- seq(mass.m1 - (mass.inc * 10), mass.m1 + (mass.inc * 10), by=mass.inc)
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
  
  raw.alpha1 <- read.csv("data/raw-alpha-1.csv")
  raw.alpha2 <- read.csv("data/raw-alpha-2.csv")
  raw.beta1 <- read.csv("data/raw-beta-1.csv")
  raw.beta2 <- read.csv("data/raw-beta-2.csv")
  
  raw.alpha1$Altitude <- raw.alpha1$Altitude * 0.3048
  raw.alpha2$Altitude <- raw.alpha2$Altitude * 0.3048
  raw.beta1$Altitude <- raw.beta1$Altitude * 0.3048
  raw.beta2$Altitude <- raw.beta2$Altitude * 0.3048
  
  raw.alpha.alt <- (raw.alpha1$Altitude[1:301] + raw.alpha2$Altitude[1:301]) / 2
  raw.alpha.time <- (raw.alpha1$Time[1:301] + raw.alpha2$Time[1:301]) / 2
  raw.alpha <- data.frame(raw.alpha.time, raw.alpha.alt)
  names(raw.alpha) <- c("Time", "Altitude")
  
  raw.beta.alt <- (raw.beta1$Altitude[1:301] + raw.beta2$Altitude[1:301]) / 2
  raw.beta.time <- (raw.beta1$Time[1:301] + raw.beta2$Time[1:301]) / 2
  raw.beta <- data.frame(raw.beta.time, raw.beta.alt)
  names(raw.beta) <- c("Time", "Altitude")
  
  raw.alpha1.plot <- qplot(x=raw.alpha1$Time, y=raw.alpha1$Altitude, main="Alpha Flight 1 Altitude vs. Time", xlab="Time (sec)", ylab="Altitude (meters)", size=I(0.2)) + geom_line()
  raw.alpha2.plot <- qplot(x=raw.alpha2$Time, y=raw.alpha2$Altitude, main="Alpha Flight 2 Altitude vs. Time", xlab="Time (sec)", ylab="Altitude (meters)", size=I(0.2)) + geom_line()
  raw.beta1.plot <- qplot(x=raw.beta1$Time, y=raw.beta1$Altitude, main="Beta Flight 1 Altitude vs. Time", xlab="Time (sec)", ylab="Altitude (meters)", size=I(0.2)) + geom_line()
  raw.beta2.plot <- qplot(x=raw.beta2$Time, y=raw.beta2$Altitude, main="Beta Flight 2 Altitude vs. Time", xlab="Time (sec)", ylab="Altitude (meters)", size=I(0.2)) + geom_line()
  grid.arrange(raw.alpha1.plot, raw.alpha2.plot, raw.beta1.plot, raw.beta2.plot)
  
  raw.alpha.plots <- qplot(x=raw.alpha1$Time, y=raw.alpha1$Altitude, main="Alpha Flights Altitude vs. Time", xlab="Time (sec)", ylab="Altitude (meters)", size=I(0.2)) + geom_line() + geom_point(aes(x=raw.alpha2$Time, y=raw.alpha2$Altitude), size=I(0.2), color="green")
  raw.beta.plots <- qplot(x=raw.beta1$Time, y=raw.beta1$Altitude, main="Beta Flights Altitude vs. Time", xlab="Time (sec)", ylab="Altitude (meters)", size=I(0.2), ylim=c(-30, 80)) + geom_line() + geom_point(aes(x=raw.beta2$Time, y=raw.beta2$Altitude), size=I(0.2), color="green")
  grid.arrange(raw.alpha.plots, raw.beta.plots, ncol=1)  
  
  rocket <- runSim(0)
  raw.alpha.plot <- qplot(x=raw.alpha$Time, y=raw.alpha$Altitude, main="Alpha Flight Comparison", xlab="Time (sec)", ylab="Altitude (meters)", size=I(0.2)) + geom_line() + geom_point(aes(x=rocket$Time, y=rocket$Altitude), color="red", size=I(0.2)) + geom_line() + geom_point(aes(x=raw.alpha1$Time, y=raw.alpha1$Altitude), color="green", size=I(0.2)) + geom_line() + geom_point(aes(x=raw.alpha2$Time, y=raw.alpha2$Altitude), color="green", size=I(0.2)) + geom_line()
  rocket <- runSim(0.010) 
  raw.beta.plot <- qplot(x=raw.beta$Time, y=raw.beta$Altitude, main="Beta Flight Comparison", xlab="Time (sec)", ylab="Altitude (meters)", size=I(0.2), ylim=c(-50, 80)) + geom_line() + geom_point(aes(x=rocket$Time, y=rocket$Altitude), color="red", size=I(0.2)) + geom_line() + geom_point(aes(x=raw.beta1$Time, y=raw.beta1$Altitude), color="green", size=I(0.2)) + geom_line() + geom_point(aes(x=raw.beta2$Time, y=raw.beta2$Altitude), color="green", size=I(0.2)) + geom_line()
  grid.arrange(raw.alpha.plot, raw.beta.plot, ncol=1)
}

nasaExpenditurePlot <- function() {
  
  nasa <- read.csv("data/nasa-budget.csv")
  qplot(main="NASA Budget as Percent of Federal Budget", xlab="Year", ylab="Percent of Federal Budget") + geom_bar(aes(x=nasa$year, y=nasa$percent.budget), stat="identity", fill="darkgreen", color="black")
}