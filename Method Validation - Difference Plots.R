# Method Validation
# Defference Plots - Bland Altman Plots
# Author : Aurthur Musendame

rm(list = ls())

baplot <- function(r1, r2, ...) {
    # r1 and r2 are the results whose difference is to be plotted
    means <- (r1 + r2) / 2       # average of each pair
    diffs <- r1 - r2             # diference between pairs
    mdiff <- mean(diffs)         # average of the differences
    sddiff <- sd(diffs)          # standard deviation of the differences
    
    # Compute the figure limits
    ylimh <- mdiff + 3 * sddiff   # upper limit : mean + 3 standard deviations
    yliml <- mdiff - 3 * sddiff   # lower limit : mean - 3 standard deviations
    
    # Plot data
    plot(diffs ~ means,
         main = "BA Difference Plot \n Method Validation - Viral Load",
         xlab = "Average values", 
         ylab = "Differences", 
         ylim = c(yliml, ylimh), ...) 
    # Center line : average 
    abline(h = mdiff) 
    # Standard deviations lines
    abline(h = mdiff + 1.96 * sddiff, lty = 2)
    abline(h = mdiff - 1.96 * sddiff, lty = 2)
}

# Scatter Plot + Regression Line
spra <- function(r1,r2,...){
    # Scatter Plot
    plot(r1, r2, 
         main= "VL Method Validation Linearity Studies \n Scatter Plot plus Regression LIne", 
         xlab= "Old Machine", 
         ylab= "New Machine", 
         pch=19)
    # Add a Regression Line to the scatter plot
    abline(lm(r2 ~ r1), col="red")
    
    # Covariance
    cl.covar <- cov(r1, r2, use = "everything", method = "pearson")
    # Correlatin
    cl.corr2 <- cor(r1, r2, use = "everything", method = "pearson")
    return(c(Covariance = cl.covar, Correlation = cl.corr2))
}

# Linear Regression
fit <- function(r1, r2, ...){
    lr <- lm(r1 ~ r2, data=data)
    summary(lr) # show results
    #  Other useful functions 
    #  coefficients(fit) # model coefficients
    #  confint(fit, level=0.95) # CIs for model parameters 
    #  fitted(fit) # predicted values
    #  residuals(fit) # residuals
    #  anova(fit) # anova table 
    #  vcov(fit) # covariance matrix for model parameters 
    #  influence(fit) # regression diagnostics
}

# run the funcions for Method Verification
all <- function(r1, r2){
    baplot(r1, r2)
    spra(r1, r2) 
    fit(r1, r2)
}

# load your data into a dataframe named data
data <- read.csv("ViralLoadMethodVerification.csv", stringsAsFactors = FALSE)

all(data[,1], data[,2])
all(log(data[,1]), log(data[,2]))



##
##  Within Run Controls : sd,cv,mean and dist
## 

wrc <- function(hpc, lpc, ...){
    wrchmu <- mean(hpc)
    wrchsd <- mean(hpc)
  #  wrchcov <- cov(hpc, y = NULL, use = "everything", method = "pearson")
    wrchcv <- wrchmu / wrchsd
    hist(hpc,
         freq = TRUE,
         main = "High Positive Control")
    lines(density(hpc), col = "blue" , lwd = 2)
    rug(hpc)
}
#load dataset for wrc
data2 <- read.csv("ViralLoadControls.csv", stringsAsFactors = FALSE)

wrc(log10(data2[,2]), log10(data2[,2]))

summary(log10(data2[,1:2]))

boxplot( log10(data2[,1]),
         horizontal = TRUE)
boxplot( log10(data2[,2]),
         horizontal = TRUE)

