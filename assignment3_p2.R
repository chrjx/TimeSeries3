## Source the files in the "functions" folder
files <- dir("functions",full.names = TRUE)
for(i in 1:length(files)) source(files[i])

# Prepare data
# ----------------------------------------------------------------
X <- read.table("data/experiment1.csv", sep = ",", header = TRUE)
# Convert to t_1 = 0
X$t <- X$t - X$t[1]

# Use the lagdf to make lags
# print(X[1,3:6]) # take only one data, from 3rd to 6th column.
# X[, 2:6] # take all data, from 2nd to 6th column.
lagdf(X[ ,2:6], 1)

# Add the lags to X
maxlag <- 10
for(i in 1:maxlag){
  tmp <- lagdf(X[ ,2:6], i)
  names(tmp) <- paste0(names(tmp),".l",i)
  X <- cbind(X, tmp)
}

# Q1
# ----------------------------------------------------------------
## Implement the ARX order 1 model in Equation (2)
## Estimate the parameters with the least squares method.
fit <- lm(ARX("Tinner", c("Ta"), 1), data = X)
summary(fit)

# Q2
# ----------------------------------------------------------------
# Validation plot
# CCF plot
validate(fit)
ccfplot(fit, X)

# Q3
# ----------------------------------------------------------------
# add inputs
# Fit an ARX
# increase order
fit <- lm(ARX("Tinner", c("Ta", "Pinner", "Touter"), 1:2), data = X)
summary(fit)

# Q4
# validation of the new model
validate(fit)
ccfplot(fit, X)
