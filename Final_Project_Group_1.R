## Group 1 - By Yahui Peng, Gulsum Arslan, Kamaniya Chatakondu

library(bootstrap)
library(fda)
library(KernSmooth)
library(sharpData)
library("writexl")

load("~/UTSA_MSDS/assignments/NonParamatric/Project/proj.RData")

df <- data.frame(X=x, Y=y)
print(df)
plot(df)
str(df)

## local poly regression with CV

h1 <- CVsharp(x=df$X, y=df$Y, deg=1, nsteps=10)  # Cross-Validation Bandwidth Selection
h3 <- CVsharp(x=df$X, y=df$Y, deg=3, nsteps=10)
h5 <- CVsharp(x=df$X, y=df$Y, deg=5, nsteps=10)
h10 <- CVsharp(x=df$X, y=df$Y, deg=10, nsteps=10)
h15 <- CVsharp(x=df$X, y=df$Y, deg=15, nsteps=10)
h20 <- CVsharp(x=df$X, y=df$Y, deg=20, nsteps=10)

locpol.1  <- locpoly(x=df$X, y=df$Y, bandwidth = h1$CVh,  degree=1,
                     range.x = range(x), gridsize = length(x))

locpol.3  <- locpoly(x=df$X, y=df$Y, bandwidth = h3$CVh,  degree=3,
                     range.x = range(x), gridsize = length(x))

locpol.5  <- locpoly(x=df$X, y=df$Y, bandwidth = h5$CVh,  degree=5,
                     range.x = range(x), gridsize = length(x))

locpol.10  <- locpoly(x=df$X, y=df$Y, bandwidth = h10$CVh,  degree=10,
                      range.x = range(x), gridsize = length(x))

locpol.15  <- locpoly(x=df$X, y=df$Y, bandwidth = h15$CVh,  degree=15,
                      range.x = range(x), gridsize = length(x))

locpol.20  <- locpoly(x=df$X, y=df$Y, bandwidth = h20$CVh,  degree=20,
                      range.x = range(x), gridsize = length(x))

(MSE.locpol.1 <- mean((y-locpol.1$y)^2))
(MSE.locpol3 <- mean((y-locpol.3$y)^2))
(MSE.locpol5 <- mean((y-locpol.5$y)^2) )
(MSE.locpol10 <- mean((y-locpol.10$y)^2))
(MSE.locpol15 <- mean((y-locpol.15$y)^2))
(MSE.locpol20 <- mean((y-locpol.20$y)^2))

plot(df$Y ~ df$X,  xlab="X", ylab="Y", main="Local Polynomial Regression with 10-fold CV")
lines(locpol.1$x, locpol.1$y, lwd=2, col="blue")
lines(locpol.3$x, locpol.3$y, lwd=2, col="purple")
lines(locpol.5$x, locpol.5$y, lwd=2, col="green")
lines(locpol.10$x, locpol.10$y, lwd=2, col="orange")
lines(locpol.15$x, locpol.15$y, lwd=2, col="red")
lines(locpol.20$x, locpol.20$y, lwd=2, col="yellow")
legend("bottomright", c("LPR with degree = 1", "LPR with degree = 3", 
                        "LPR with degree = 5", "LPR with degree = 10",
                        "LPR with degree = 15", "LPR with degree = 20"),
       lty = 1, lwd = 2, col = c("blue", "purple", "green", "orange", "red", "yellow") , cex=0.5)

##############################################################
# cubic spline
?smooth.spline
ss1 <- smooth.spline(df$X, df$Y, cv=TRUE)

ss2 <- smooth.spline(df$X, df$Y, cv=FALSE)

par(mfrow=c(1,1))

fit.ss1<-fitted(ss1)
(mse.ss1 <- mean((df$Y-fit.ss1)^2))

fit.ss2<-fitted(ss2)
(mse.ss2 <- mean((df$Y-fit.ss2)^2))


plot(df$X, df$Y,  xlab="X", ylab="Y", main="Comparison between LPR and Cubic Splines with 10-fold CV")
lines(locpol.10$x, locpol.10$y,col="orange",lwd=2 )
lines(ss1, col="blue", lwd=2)
lines(ss2, col="red", lwd=2)
legend("bottomright", c("LPR with degree = 10", "Cubic spline with LOOCV", "Cubic spline with GCV"),
       lty = 1, lwd = 2, col = c("orange", "blue", "red"),cex=0.6)

##################################
# data frame for fitted values of the selected models

Fitted_Values <- data.frame(df$Y,locpol.10$y,fitted(ss1))

write_xlsx(Fitted_Values,"~/UTSA_MSDS/assignments/NonParamatric/Project/FittedValues.xlsx")
