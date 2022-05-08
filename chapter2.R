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

##### Aufgabe 5: Schätzunsicherheit #####

# Daten einlesen
random <- read.csv("random.csv")

# a) Lineares Modell y1 ~ x
regA <- lm(Y1 ~ X, data = random)
summary(regA)
# (i) Kovarianz Matrix der Koeffizienten
vcov(regA)
# (ii) Ohne Konstante/Intercept - In R wird die Konstante standardmäßig in die
# Regression mit aufgenommen, das verhindert man, indem man -1 in die Gleichung
# aufnimmt
regOhneK <- lm(Y1 ~ -1 + X, data = random)
summary(regOhneK)

# b) Lineares Modell y2 ~ x
regB <- lm(Y2 ~ X, data = random)
summary(regB)

# zwei Scatterplotts in einem Plot
plot(Y1 ~ X, data = random, col = "blue",  xlab='X', ylab='Y1 and Y2',
     xlim=c(14,47), ylim=c(25,100), main='Scatterplot') # Plot 1
par(new = TRUE) # Plotte den nächsten Plot in dem davor
plot(Y2 ~ X, data = random, col = "red", xlab='X', ylab='Y1 and Y2',
     xlim=c(14,47), ylim=c(25,100)) # Plot 2
abline(a = 1, b = 2, col = "green") # Gerade mit wahrem Zusammenhang
legend("bottomright", legend=c('Y1', 'Y2', 'True Model'),
       col=c('blue', 'red', 'green'), pch = c(1, 1, NA_integer_),
       lty = c(0, 0, 1)) # Legende hinzufügen


# c) Verringere Variation von X
# (i) durch Skalierung von X
random$xScaled <- random$X/10
# Modell aus b) neu schätzen
regScaledX <- lm(Y2 ~ xScaled, data = random)
summary(regScaledX)
# (ii) durch veringerung de Stichprobengröße, nutzt nur die ersten 100 
# Beobachtungen für die Schätzung
regSmallSample <- lm(Y2 ~ X, data = random[1:100,])
summary(regSmallSample)

# d) Umkehrregressionen
# (1) zur Regression in a)
regUmkehrA <- lm(X ~ Y1, data = random)
summary(regUmkehrA)
# (2) zur Regression in b)
regUmkehrB <- lm(X ~ Y2, data = random)
summary(regUmkehrB)
