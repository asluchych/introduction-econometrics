################################################################################
#                                                                              #
#            Assignment 2: The Simple Linear Regression Model                  # 
#                       Exercise 3 and 5                                       #
#                                                                              #
################################################################################

##### Exercise 3: House Prices #####

# Read data
houses <- read.csv("houseprices.csv")

# Exploration
summary(houses)
plot(PRICE ~ SQM, data = houses) # positive relationship between price and sqm
cor(houses$PRICE, houses$SQM)

# a) Linear Regression with OLS
linModA <- lm(PRICE ~ SQM, data = houses)
summary(linModA)

# Add a regression line to the scatterplot
plot(PRICE ~ SQM, data = houses)
abline(linModA, col="red")

# c) Predict the price for the house of size 200m^2 
price_200 <- predict(linModA, data.frame(SQM = c(200)))
print(price_200) # Print the value in the console

# d) Scaling
houses$priceScaled <- houses$PRICE/1000 # price in 1000 $ 
# Linear regression with scaled price
linModC <- lm(priceScaled ~ SQM, data = houses)
summary(linModC)
# Prediction
priceScaled_200 <- predict(linModC, data.frame(SQM = c(200)))
# The prediction is still in 1000$. For interpretation, calculate in $ * 1000:
priceInDollar_200 <- priceScaled_200 * 1000 

#####  Assignment 5: Estimation Uncertainty #####

# Read data
random <- read.csv("random.csv")

# a) linear model y1 ~ x
regA <- lm(Y1 ~ X, data = random)
summary(regA)
# (i) Covariance matrix of the coefficients
vcov(regA)
# (ii) Without a constant/intercept - By default, intercept is included in 
# a regression in R. To estimate the model without an intercept, include -1 in 
# the equation
regNoInt <- lm(Y1 ~ -1 + X, data = random)
summary(regNoInt)

# b) Linear model y2 ~ x
regB <- lm(Y2 ~ X, data = random)
summary(regB)

# Two scatterplots in one plot
plot(Y1 ~ X, data = random, col = "blue",  xlab='X', ylab='Y1 and Y2',
     xlim=c(14,47), ylim=c(25,100), main='Scatterplot') # plot 1
par(new = TRUE) # plot the next plot in the previous one
plot(Y2 ~ X, data = random, col = "red", xlab='X', ylab='Y1 and Y2',
     xlim=c(14,47), ylim=c(25,100)) # plot 2
abline(a = 1, b = 2, col = "green") # true model line
legend("bottomright", legend=c('Y1', 'Y2', 'True Model'),
       col=c('blue', 'red', 'green'), pch = c(1, 1, NA_integer_),
       lty = c(0, 0, 1)) # add a legend


# c) Reduce the variation of x 
# (i) by rescaling of X
random$xScaled <- random$X/10
# Re-estimate the model from b)
regScaledX <- lm(Y2 ~ xScaled, data = random)
summary(regScaledX)
# (ii) by reducing the sample size, use the first 100 observations
regSmallSample <- lm(Y2 ~ X, data = random[1:100,])
summary(regSmallSample)

# d) inverse regression
# (1) for regression in a)
regInvA <- lm(X ~ Y1, data = random)
summary(regInvA)
# (2) for regression in b)
regInvB <- lm(X ~ Y2, data = random)
summary(regInvB)