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
fit <- marima("Tinner ~ AR(1:1) + MA(1:1) + Ta(1:1)", data = X, penalty = 2)
summary(fit)
validate(fit)

# Q3 
## increase order
## model selection and validation.



# Q4
## multi-step prediction



