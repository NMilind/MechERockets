# Rockets in Mechanical Engineering

Information
-----------
This is a model built exclusively in R to model rockets. The flight paths predicted by this model are only applicable for the ascent of a rocket. The goal of this model is to predict the maximum height a rocket with a certain engine will achieve and tries to optimize this height using ballast mass.

Setup
-----
The code has been tested on a Linux (Ubuntu Distro) system. All development was conducted using RStudio. Data was provided by the official manufacturers of the model rocket engines, Estes.

Installation
------------
Install R from the CRAN distribution that is available online at https://www.r-project.org/. RStudio is the preferred method of using R on Windows, Mac, or Linux machines. RStudio is available at https://www.rstudio.com/. However, RStudio is not necessary for running this simulation.

Editing Values
--------------
To change the initial values with which the simulation runs, navigate to the data/ directory and open the mass.csv file. The range over which the simulation runs, the Delta T value of the integration, and the ballast mass machine learning configurationsa are all modifiable. 

Running the Program
-------------------
Execute `run.R` to start the simulation. In R, navigate to the main directory of this program and run the following in the terminal:
`R --file="run.R"`
In RStudio, simply open `run.R` and click the "Run" button to start the simulation.
