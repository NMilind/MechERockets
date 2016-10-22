### ----------------------------------------------- ###
### Author:   Nikhil Milind and Karl Westendorff    ###
### Date:     October 11 2016                       ###
### ----------------------------------------------- ###

### LIBRARIES ###
library(ggplot2)
library(progress)
library(grid)
library(gridExtra)

### ENVIRONMENT SETUP ###
rm(list = ls())
graphics.off()
setwd("/home/nikhil/Desktop/R-Code/RocketModel")

source("integrator.R")
source("data.R")
source("plots.R")

# Plot the Initial Data
plotInitialData()

runSim <- function(m) {

  source("config.R")
  
  ### SIMULATION RUN ###
  rocket <- data.frame()
  rocket.ballast[1] <- m
  rocket.mass[1] <- rocket.mass[1] + rocket.ballast[1]
  rocket.weight[1] <- rocket.mass[1] * 9.8
  pg <- progress_bar$new(total = length(rocket.time) - 1)
  print(paste("Rocket Simulation with Mass", m, sep=" "))
  for (dt in 2:length(rocket.time)) {
    
    rocket.ballast[dt] <- rocket.ballast[dt-1]
    rocket.mass[dt] <- mass.curve(rocket.time[dt]) + rocket.ballast[dt]
    if (rocket.velocity[dt-1] >= 0) { rocket.cd[dt] <- rocket.cd[dt-1] }
    else { rocket.cd[dt] <- 4.0 } # Coefficient of drag during decent 
    rocket.area[dt] <- rocket.area[dt-1] # Carry over cross-sectional area
    rocket.thrust[dt] <- thrust.curve(rocket.time[dt])
    rocket.weight[dt] <- rocket.mass[dt] * 9.8
    rocket.density[dt] <- density.curve(rocket.time[dt])
    rocket.drag[dt] <- (0.5) * rocket.density[dt] * rocket.velocity[dt-1]^2 * rocket.cd[dt] * rocket.area[dt]
    
    # Calculating the total force on the rocket at this instant
    rocket.fnet[dt] <- -1 * rocket.weight[dt]
    if (rocket.velocity[dt-1] >= 0) { rocket.fnet[dt] <- rocket.fnet[dt] + (-1 * rocket.drag[dt]) }
    else { rocket.fnet[dt] <- rocket.fnet[dt] + rocket.drag[dt] }
    rocket.fnet[dt] <- rocket.fnet[dt] + rocket.thrust[dt]
    
    # Calculate acceleration at this instant
    rocket.acceleration[dt] <- rocket.fnet[dt] / rocket.mass[dt]
    
    # Calculate velocity at this instant
    rocket.velocity[dt] <- rocket.velocity[dt-1] + differentiate(function(x) { return(rocket.acceleration[dt]) }, rocket.time[dt], DT)
    
    # Calculate the displacement over the interval
    rocket.altitude[dt] <- rocket.altitude[dt-1] + differentiate(function(x) { return(rocket.velocity[dt]) }, rocket.time[dt], DT)
    
    pg$tick()
  }
  rm(dt, pg)
  
  ### SIMULATION DATA COLLECTION ###
  rocket <- data.frame(rocket.time, rocket.fnet, rocket.altitude, rocket.velocity, rocket.acceleration, rocket.thrust, rocket.mass, rocket.ballast, rocket.weight, rocket.density, rocket.drag, rocket.cd, rocket.area)
  names(rocket) <- c("Time", "FNet", "Altitude", "Velocity", "Acceleration", "Thrust", "Mass", "Ballast", "Weight", "Density", "Drag", "Cd", "Area")
  
  # Plot the Final Data
  plotResults(rocket)
  print(paste("Max Altitude: ", max(rocket.altitude), sep=""))
  
  return(rocket)
}