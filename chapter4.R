################################################################################
#                                                                              #
#      Exercise 4: Prediction, Goodness-of-Fit and Modelling Issues            # 
#                           Assignment 3                                       #
#                                                                              #
################################################################################
# package: if its not installed, type install.packages("tseries") in the console
library(tseries) # for the jarque-bera-test function


##### Assigment 3: House Prices #####

# Read data
houses <- read.csv("houseprices.csv")

# a) Interpretation b_2 and R^2
# Regressions
# Model (1)
lin <- lm(PRICE ~ SQM, data = houses)
summary(lin)

# Model (2)
logLin <- lm(log(PRICE) ~ SQM, data = houses)
summary(logLin)

# Modell (3)
logLog <- lm(log(PRICE) ~ log(SQM), data = houses)
summary(logLog)

# b) Comparison 
xVals <- seq(20, 800, 0.1)
fittedValsLogLin <- exp(logLin$coefficients[1] + logLin$coefficients[2]*xVals)
fittedValsLogLog <- exp(logLog$coefficients[1] + logLog$coefficients[2]*log(xVals))

# (i) Regression graph - scatterplot PRICE, SQM
plot(PRICE ~ SQM, data = houses, main = "Prices vs. Square Meters")
abline(lin, col = "green")
lines(fittedValsLogLin ~ xVals, col = "red")
lines(fittedValsLogLog ~ xVals, col = "blue")
legend("topleft", legend = c("Linear", "Log-Linear", "Log-Log"),
       col = c("green", "red", "blue"), lwd = 1)

# (ii) Residuals graph
# Linear model
plot(lin$residuals ~ houses$SQM, main = "Residuals Linear Model")
abline(h=0)

# Log-linear model
plot(logLin$residuals ~ houses$SQM, main = "Residuals Log-Linear Model")
abline(h=0)

# Log-log model
plot(logLog$residuals ~ houses$SQM, main = "Residuals Log-Log Model")
abline(h=0)

# (iv) Jarque Bera Test: H_0 the residuals are normally distributed
# Linear Model
jarque.bera.test(lin$residuals)

# Log-Linear Model
jarque.bera.test(logLin$residuals)

# Log-Log Model
jarque.bera.test(logLog$residuals)
