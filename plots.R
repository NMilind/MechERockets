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
  graphics.off()
  grid.arrange(plots.thrust, plots.acceleration, plots.velocity, plots.altitude, ncol=2)
}