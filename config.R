### ----------------------------------------------- ###
### Author:   Nikhil Milind and Karl Westendorff    ###
### Date:     October 11 2016                       ###
### ----------------------------------------------- ###

### SIMULATION SETUP ###
DT <- mass$dt
RANGE <- mass$range
rocket.time <- seq(0, RANGE, length.out=(RANGE / DT))
rocket.fnet <- vector(mode="double", length=(RANGE / DT))
rocket.altitude <- vector(mode="double", length=(RANGE / DT))
rocket.velocity <- vector(mode="double", length=(RANGE / DT))
rocket.acceleration <- vector(mode="double", length=(RANGE / DT))
rocket.thrust <- vector(mode="double", length=(RANGE / DT))
rocket.mass <- vector(mode="double", length=(RANGE / DT))
rocket.ballast <- vector(mode="double", length=(RANGE / DT))
rocket.weight <- vector(mode="double", length=(RANGE / DT))
rocket.density <- vector(mode="double", length=(RANGE / DT))
rocket.drag <- vector(mode="double", length=(RANGE / DT))
rocket.cd <- vector(mode="double", length=(RANGE / DT))
rocket.area <- vector(mode="double", length(RANGE / DT))

rocket.thrust[1] <- thrust.curve(0)
rocket.mass[1] <- mass.curve(0)
rocket.ballast[1] <- 0
rocket.weight[1] <- rocket.mass[1] * 9.8
rocket.density[1] <- density.curve(0)
rocket.cd[1] <- mass$cd
rocket.area[1] <- mass$rocket.area