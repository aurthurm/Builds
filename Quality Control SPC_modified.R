# Statistical Process Control
# Levy-Jennings Chart
# Author : Aurthur Musendame

rm(list = ls())

# set.seed( seed = NULL)

# LJStats
LjStats <- function(x){
  cl.sds      <<- -4:4 + mean(x)  # Mean +/- -4:4 Standard Deviations
  cl.ylim     <<- c(cl.sds[1], cl.sds[9]) # Y Limit : 4 standard devitions
  cl.sd.names <- c("-3 SD", "-2 SD", "-1 SD", "Mean", "1 SD", "2 SD", "3 SD")  # Standard Deviation Labels
  cl.base     <<- data.frame(cl.sd.names, cl.sds[2]:cl.sds[8], stringsAsFactors = FALSE)
  return(c(Obs = length(x),
           Mean = cl.sds[5],
           SD = sd(x)))
}

# Number of days based on number of obserbvations
LjObs <- function(x){
  obs <- length(x)
  return (obs)
}


# Plot the SPC Chart
LjChart <- function(x, title = main.title){
  LjStats(x)
  days  <- 1:LjObs(x)
  # Set the text and margin sizes
  par(cex.axis = 0.8, mai = c(1, 0.7, 0.7, 1), fig = c(0, 0.95, 0, 1))
  # Plotting the SPC
  plot(days, x,
       xaxt = "n",
       yaxt = "n",
       pch  = 20, # use also 1, 4, 18 etc
       lty  = 3,
       ylim = cl.ylim,  # c(cl.base[,2][7],cl.base[,2][1])
       type = "b",
       col  = "green",
       ylab = "",
       xlab = "Control Run Days",
       main = title
  )
  #  Draw the Standard Deviation Lines
  abline(h   = c(cl.base[, 2]), 
         col = c("red", "orange", "yellow", "green", "yellow", "orange", "red"))
  axis(1, at = days, labels = days, las = 2)
  # Left Labels 
  axis(2, at = cl.base[, 2], labels = cl.base[, 1], las = 2) 
  # Right Labels - Numerical
  axis(4, at = cl.base[,2], labels = round(cl.base[,2], digits = 3), las = 2)
  ## Add a BoxPlot to the plot
  #par(fig = c(0.675, 1, 0, 1), new = TRUE)
  #boxplot(cl.base[, 2], axes = FALSE)
}

normal.control <- rnorm(1:30,5,1.3)
main.title <- "Levey Jennings Chart \n Normal Control "
#normal control
LjChart(normal.control)
normal.control
LjStats(normal.control)




