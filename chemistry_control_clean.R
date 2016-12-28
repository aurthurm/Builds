rm(list = ls())

chemistry_controls <- read.csv("controlsChemistry.csv",stringsAsFactors = FALSE)
head(chemistry_controls)
data <- chemistry_controls
head(data)
data$X[1]  <- NA
data$October_2016[1] <- NA
data$X.1[1] <- NA
data1 <- na.omit(data)
head(data1)
pathological <- data1[,2]
normal <- data1[,3]
dates <- data1[,1]
pathological <- as.numeric(pathological)
normal <- as.numeric(normal)

