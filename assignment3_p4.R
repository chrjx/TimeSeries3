## Source the files in the "functions" folder
files <- dir("functions",full.names = TRUE)
for(i in 1:length(files)) source(files[i])

# Make plots
X <- read.table("data/experiment1.csv", sep = ",", header = TRUE)
# Convert to t_1 = 0
X$t <- X$t - X$t[1]
library(marima2)
??marima2

X <- X[ ,1:6]
names(X)

# Problem 4

# Q1

## fit <- marima("Tinner ~ AR(1:2) + Ta(1:2) + MA(1:2)", data=X) From Introduction
fit <- marima("Touter ~ AR(1:2) + Ta(1:2) + Po(1:2) + MA(1:2)", data = X)
summary(fit)
validate(fit)
ccfplot(fit, X)
