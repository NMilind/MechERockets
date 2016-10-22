### ----------------------------------------------- ###
### Author:   Nikhil Milind and Karl Westendorff    ###
### Date:     October 11 2016                       ###
### ----------------------------------------------- ###

### LOAD DATA ###
thrust <- read.csv("thrust.csv", header=TRUE)
density <- read.csv("density.csv", header=TRUE)
mass <- read.csv("mass.csv", header=TRUE)

### GENERATE CURVES ###

## Thrust Curve
thrust.curve <- function(time) {
  
  # Loop through all intervals in the data
  for (interval in 1:(length(thrust$time) - 1)) {
    
    # Check if the time argument falls in the current interval
    if (time >= thrust[interval, "time"] && time < thrust[interval+1, "time"]) {
      
      # Get the initial and final values for thrust and time in this interval
      thrust1 <- thrust[interval, "thrust"]
      thrust2 <- thrust[interval+1, "thrust"]
      time1 <- thrust[interval, "time"]
      time2 <- thrust[interval+1, "time"]
      # Calculate the slope of the interval
      a <- ((thrust2 - thrust1) / (time2 - time1))
      # Calculate the y-intercept of the line of thrust over this interval
      b <- (-a * time1) + thrust1
      # Return a value that is linearly interpolated between the two thrust values of the interval
      return(a*time + b)
    }
  }
  # If the thrust data is over, simply return a thrust of 0
  return(0)
}

## Density Curve
density.curve <- function(altitude) {
  
  # Iterate through the density data
  for (interval in 1:(length(density$altitude) - 1)) {
    
    # Check if the altitude falls in the current interval
    if (altitude >= density[interval, "altitude"] && altitude < density[interval+1, "altitude"]) {
      
      # Get the density and altitude value for the current interval
      density1 <- density[interval, "air.density"]
      density2 <- density[interval+1, "air.density"]
      alt1 <- density[interval, "altitude"]
      alt2 <- density[interval+1, "altitude"]
      # Calculate the slope of the line through the current interval
      a <- ((density2 - density1) / (alt2 - alt1))
      # Calculate the y-intercept of the line through the current interval
      b <- (-a * alt1) + density1
      # Return a value that is linearly interpolated between the two density values of the interval
      return(a*altitude + b)
    }
  }
  # If the curve ends, return a value of 0 for density (only occurs if the rocket exceeds 80,000 meters)
  return(0)
}

## Mass Curve
mass.netImpulse <- -1
mass.burnTime <- -1
mass.integral <- c(0)
mass.dataInt <- function(time, iter) {
  
  # Calculate Net Impulse and Burn Time if not calculated
  if (mass.netImpulse < 0 || mass.burnTime < 0) {
    mass.netImpulse <- integrate(thrust.curve, min(thrust$time), max(thrust$time), iter)
    mass.burnTime <- max(thrust$time) - min(thrust$time) 
  }
  
  # Calculate the current impulse
  currInt <- integrate(thrust.curve, min(thrust$time), time, iter)
  
  massFrac <- currInt / mass.netImpulse
  return(mass$initial.mass - massFrac * (mass$initial.mass - mass$final.mass))
}

mass.testCurve <- c()
pg <- progress_bar$new(total = (1 - 0) / 0.005)
print("Mass Curve Run")
for (i in seq(0, 1, by=0.005)) {
  mass.testCurve <- c(mass.testCurve, mass.dataInt(i, (1-0)/0.005))
  pg$tick()
}
mass.curve.data <- data.frame(seq(0, 1, by=0.005), mass.testCurve)
names(mass.curve.data) <- c("time", "mass")

mass.curve <- function(time) {
  
  for (interval in 1:(length(mass.curve.data$time) - 1)) {
    
    if (time >= mass.curve.data[interval, "time"] && time < mass.curve.data[interval+1, "time"]) {
      
      mass1 <- mass.curve.data[interval, "mass"]
      mass2 <- mass.curve.data[interval+1, "mass"]
      time1 <- mass.curve.data[interval, "time"]
      time2 <- mass.curve.data[interval+1, "time"]
      
      a <- ((mass2 - mass1) / (time2 - time1))
      
      b <- (-a * time1) + mass1
      
      return(a*time + b)
    }
  }
  
  return(mass.curve.data[length(mass.curve.data), "mass"])
}