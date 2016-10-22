### ----------------------------------------------- ###
### Author:   Nikhil Milind and Karl Westendorff    ###
### Date:     October 11 2016                       ###
### ----------------------------------------------- ###

# Integrates a function over a certain interval
# func = Function that is to be integrated - must be in the form func(t)
# lower = Lower bound of the interval
# upper = Upper bound of the interval
# i = Number of geometric quadrilaterals to be generated
integrate <- function(func, lower, upper, i) {
  
  # Calculate the Delta-T value from the number of quadrilaterals required
  dt <- (upper - lower) / i
  
  # We will use the Runge-Kutta 4 Method to integrate
  # This is a weighted sum of the midpoint rule and the trapezoidal rule
  # 1/3 of the trapezoidal sum and 2/3 of the midpoint sum are utilized
  
  # Calculate the sum of all left-bound t values
  sumFtn <- 0
  for (n in 0:(i-1)) {
    sumFtn <- sumFtn + func(n * dt)
  }
  # Calculate the sum of all right-bound t values
  sumFtn1 <- 0
  for (n in 1:i) {
    sumFtn1 <- sumFtn1 + func(n * dt)
  }
  # Calculate the sum of all midpoint t values
  sumFtnMid <- 0
  for (n in 0:(i-1)) {
    sumFtnMid <- sumFtnMid + func((n * dt) + (dt / 2))
  }
  
  # Calculate the weighted sum
  integralVal <- ((1/6) * dt * sumFtn) + ((1/6) * dt * sumFtn1) + ((2/3) * dt * sumFtnMid)
  return (integralVal)
}

# Finds the delta-differential value
# func = The function to be differentiated in the form func(x)
# n = The time at which the current step is evaluated, func(n)
# dt = The next step in the differential, func(n+dt)
differentiate <- function(func, n, dt) {
  
  eulerStep <- func(n)
  midpointStep <- func(n + (dt / 2))
  fullStep <- func(n + dt)
  
  diff <- ((eulerStep + 4 * midpointStep + fullStep) / 6) * dt
  return(diff)
}