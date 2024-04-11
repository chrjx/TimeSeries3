## Source the files in the "functions" folder
files <- dir("functions",full.names = TRUE)
for(i in 1:length(files)) source(files[i])

# Make plots
X <- read.table("data/experiment1.csv", sep = ",", header = TRUE)
# Convert to t_1 = 0
X$t <- X$t - X$t[1]


# Problem 3
# ----------------------------------------------------------------
# Use a modified version of the marima package!
# Install from file
# download.file("https://02417.compute.dtu.dk/material/marima2_0.1.tar.gz", "marima2_0.1.tar.gz")
# install.packages("marima2_0.1.tar.gz", repos=NULL)
library(marima2)
??marima2

X <- X[ ,1:6]
names(X)

# Q1
## fit <- marima("Tinner ~ AR(1:2) + Ta(1:2) + MA(1:2)", data=X) From Introduction
fit <- marima("Tinner ~ AR(1) + Ta(1)", data = X)
summary(fit)
validate(fit)
ccfplot(fit, X)

# Q2
## only 1 order
## discuss what the MA part does
## fit <- marima("Tinner ~ AR(1:1) + MA(1:1) + Ta(1:1)", data = X, penalty = 2)
fit <- marima("Tinner ~ AR(1:1) + MA(1:1) + Ta(1:1)", data = X)
## penalty didn't delete input variables
summary(fit)
validate(fit)
ccfplot(fit, X)

# Q3 
## model selection and validation.
## increase order to a high order to make penalty work

fit <- marima("Tinner ~ AR(1:2) + MA(1:2) + Ta(1:2)", data = X)
## penalty didn't delete input variables
summary(fit)
validate(fit)
ccfplot(fit, X)

fit <- marima("Tinner ~ AR(1:10) + MA(1:10) + Ta(1:10) + Pouter(1:10)", data = X, penalty = 2)
## penalty didn't delete input variables
summary(fit)
validate(fit)
ccfplot(fit, X)


# Q4 

# Q5
# Multi-step forecasts
fit <- marima("Tinner ~ AR(1:2) + MA(1:2) + Ta(1:2)", data = X)
fit <- marima("Tinner ~ AR(1:10) + MA(1:10) + Ta(1:10) + Touter(1:10) + Pinner(1:10)", data = X, penalty = 2)
val <- predict(fit, nstep=nrow(X)-1)
plot(X$Tinner)
lines(val$forecasts[1, ], type="l", col=2)


# Q6
# ----------------------------------------------------------------
## armax-simulate-step-response
fit <- marima("Tinner ~ AR(1:10) + MA(1:10) + Ta(1:10) + Touter(1:10) + Pinner(1:10)", data = X, penalty = 2)
input <- "Pinner"
Xs <- X[ ,2:6]
#Xs[ ,2:6] <- 0
Xs$Pinner <- 0
Xs$Pouter <- 0
#
Xs$Tinner <- 20
Xs$Touter <- 20
Xs$Ta <- 20
Xs[-1:-10,input] <- Xs[-1:-10,input] + 100
#
val <- predict(fit, Xs, nstep=nrow(X)-1)
#plot(M$data[ ,input], type="l")
yhat <- val$forecasts[1, ]
se <- sqrt(val$pred.var[1,1, ])
plot(yhat, type="l")
lines(yhat[-1] - qnorm(0.975)*se, lty=2)
lines(yhat[-1] + qnorm(0.975)*se, lty=2)
