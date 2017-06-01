# Statistical Process Control
# Levy-Jennings Chart & Westgard Rules
# Author : Aurthur Musendame

rm(list = ls())
# setwd("C:/Users/musendamea/Desktop/WorkingEnv/builds")
source("./builds/westgard_rules.R")


# set.seed( seed = NULL)

# LJStats
LjStats <- function(x){
  cl.sds      <<- -3:3 * sd(x) + mean(x)  # Mean +/- -4:4 Standard Deviations
  cl.ylim     <<- c(-4 * sd(x) + mean(x), 4 *sd(x) + mean(x)) # Y Limit : 4 standard devitions
  cl.sd.names <- c("-3 SD", "-2 SD", "-1 SD", "Mean", "1 SD", "2 SD", "3 SD")  # Standard Deviation Labels
  cl.base     <<- data.frame(cl.sd.names, cl.sds, stringsAsFactors = FALSE)
  return(c(Obs = length(x),
           Mean = cl.sds[4],
           SD = sd(x)))
}

# Number of days based on number of obserbvations
LjObs <- function(x){
  obs <- length(x)
  return (obs)
}


# Plot the SPC Chart
LjChart <- function(x, title = main.title){
  statz = LjStats(x)
  days  <- 1:LjObs(x)
  controls <- WestgardRules(x)
  # Set the text and margin sizes
  par(cex.axis = 0.8, mai = c(1, 0.7, 0.7, 1), fig = c(0, 0.95, 0, 1))
  # Plotting the SPC
  plot(days, controls$x,
       xaxt = "n",
       yaxt = "n",
       pch  = 20, # use also 1, 4, 18 etc
       lty  = 1,
       ylim = cl.ylim,
       type = "b",
       col  = controls$color,
       ylab = "",
       xlab = "Control Run Days",
       main = title
  )
  #  Draw the Standard Deviation Lines
  abline(h   = c(cl.base[, 2]),
         col = c("red", "orange", "yellow", "green", "yellow", "orange", "red"), lty = 5)
  axis(1, at = days, labels = days, las = 2)
  # Left Labels
  axis(2, at = cl.base[, 2], labels = cl.base[, 1], las = 2)
  # Right Labels - Numerical
  axis(4, at = cl.base[,2], labels = round(cl.base[,2], digits = 3), las = 2)
  # adding rule text to the graph
  text(controls$x, controls$rule, cex = 0.7, pos = 3)
  # adding LjStats()
  mtext("Obs: " , side = 3, adj = 0)
  mtext( statz[1] , side = 3, adj = 0.085)
  mtext("Mean: " , side = 3, adj = 0.45)
  mtext( round(statz[2], 3), side = 3, adj = 0.572)
  mtext("SD: " , side = 3, adj = 0.9)
  mtext( round(statz[3], 3) , side = 3, adj = 0.997)
  ## Add a BoxPlot to the plot
  # par(fig = c(0.675, 1, 0, 1), new = TRUE)
  # boxplot(cl.base[, 2], axes = FALSE)
}


main.title <- "Levey Jennings Chart  Normal Control \n"
normal.control <- rnorm(1:30,52,4.3)
 LjChart(normal.control)
 LjStats(normal.control)



