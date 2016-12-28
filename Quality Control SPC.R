# Statistical Process Control
# Levy-Jennings Chart
# Author : Aurthur Musendame

rm(list = ls())

# set.seed( seed = NULL)

# Basic Statistics Calculator
clqcst <- function(x){
      cl.len <- length(x)    # Number of Observations
      cl.mu <- mean(x)       # Avarage
      cl.sd <- sd(x)         # Standard Deviation
      cl.sds <-  c(cl.mu + cl.sd*3,cl.mu + cl.sd*2,cl.mu + cl.sd*1,cl.mu,cl.mu - cl.sd*1,cl.mu - cl.sd*2,cl.mu - cl.sd*3)   # Mean +/- -3:3 Standard Deviations 
      cl.ylim <<- c(cl.mu - cl.sd*4,cl.mu + cl.sd*4) # Y Limit : 4 standard devitions
      cl.sd.names <- c("3 SD","2 SD","1 SD","Mean","-1 SD","-2 SD","-3 SD")  # Standard Deviation Labels
      cl.base <<- data.frame(cl.sd.names, cl.sds, stringsAsFactors = FALSE)
      return(c(Obs = cl.len,
               Mean = cl.mu,
               SD = cl.sd))
}

# Plot the SPC Chart
clspcc <- function(x,title = main.title){
  clqcst(x)
  # Set the text and margin sizes
  par(cex.axis = 0.8, mai = c(1,0.7,0.7,1), fig = c(0,0.95,0,1))
  # Plotting the SPC
  plot(days, x,
            xaxt = "n",
            yaxt = "n",
            pch = 20, # use also 1, 4, 18 etc
            lty = 3,
            ylim = cl.ylim,  # c(cl.base[,2][7],cl.base[,2][1])
            type = "b",
            col = "Green",
            ylab = "",
            xlab = "Control Run Days",
            main = title
       )
  #  Draw the Standard Deviation Lines
  abline(h = c(cl.base[,2]), 
         col = c("red", "orange", "yellow", "green", "yellow", "orange", "red"))
  axis(1, at = days, labels = days, las = 2)
  # Left Labels 
  axis(2, at = cl.base[,2], labels = cl.base[,1], las = 2) 
  # Right Labels - Numerical
  # axis(4, at = cl.base[,2], labels = round(cl.base[,2], digits = 2), las = 2)
  ## Add a BoxPlot to the plot
  par(fig = c(0.675, 1, 0, 1), new = TRUE)
  boxplot(cl.base[,2], axes = FALSE)
}




#  NB
# 1. Make sure you specify the number of days of controls in order to plot 
#    else yll receive an error having an object days being not found

days <- 1:31

normal.control <- rnorm(1:30,5,1.3)
main.title <- "Levey Jennings Chart \n Normal Control TP (Oct-2016)"
#normal control
clspcc(normal.control)
normal.control
clqcst(normal.control)


pathological.control <- rnorm(1:30,20,3)
main.title <- "Levy Jennings \n Pathological Control TP (Oct-2016)"
#pathological control
clspcc(pathological.control)
pathological.control
clqcst(pathological.control)




