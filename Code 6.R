# Clean workspace
rm(list=ls())
cat("\014")
graphics.off()
set.seed(123)

# Load library
#install.packages("stats")
#install.packages("pls")
#install.packages("DAAG")
library(stats)
library(pls)
library(DAAG)

uscrime  <- read.csv("~/Desktop/Georgia Tech Classes/ISyE 6501/Week 5 - Basic Regression/Homework 5/Data/uscrime.csv", sep="")

#principal component analysis
input = uscrime[-16]
prin_comp <- prcomp(~ ., scale = T, input)
names(prin_comp)

summary(prin_comp)

screeplot(prin_comp)
# Let's plot the resultant principal components.
biplot(prin_comp, scale = 0)
# Based on the biplot of the Eigen vectors, we see that PC1 is a most likely a function of Wealth, Ineq, M, and So 
# and that PC2 is most likely a function of Time, Pop, M.F, and L.F. 
# These are easily seen because these are most parallel to the axes and thus have the largest variances in those particular scales.

# compute standard deviation of each principal component
std_dev <- prin_comp$sdev

#compute variance
pr_var <- std_dev^2

#check variance of the components
pr_var
# To compute the proportion of variance explained by each component, we simply divide the variance by sum of total variance.
#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex
#This shows that first principal component explains 40.12% variance. 
# Second component explains 18.67% variance. 
# Third component explains 13.36% variance and so on. 
# So, how do we decide how many components should we select for modeling stage ?
  
# The answer to this question is provided by a scree plot. 
# A scree plot is used to access components or factors which explains the most of variability in the data. 
# It represents values in descending order.
#scree plot
plot(prop_varex, xlab = "Principal Component",
       ylab = "Proportion of Variance Explained",
       type = "b")

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")
sum(prop_varex[1:7])
#This plot shows that 7 components results in variance close to ~ 92%. Besides that the screeplot shows that the 
# curve flattens arount the 7th PC.
# Therefore, in this case, we will select number of components as 7 [PC1 to PC7] 
# and proceed to the modeling stage. 

# For modeling, we will use these 7 components as predictor variables.
crime = uscrime[16]
n = 7 #this is the number of PC's to use

principal_comp = prin_comp$x[, 1:n]
input2 = cbind(crime, principal_comp)

model = lm(Crime ~ ., input2)
summary(model)

data_point = data.frame(
  M = 14.0,
  So = 0,
  Ed = 10.0,
  Po1 = 12.0,
  Po2 = 15.5,
  LF = 0.640,
  M.F = 94.0,
  Pop = 150,
  NW = 1.1,
  U1 = 0.120,
  U2 = 3.6,
  Wealth = 3200,
  Ineq = 20.1,
  Prob = 0.04,
  Time = 39.0
)

#These are the coefficients fo the model
model$coefficients

#Let's convert the coefficients back into the original unscaled dimension
alphas = prin_comp$rotation[, 1:n] %*% model$coefficients[-1]

predictor_mean = sapply(input, mean)
predictor_sd = sapply(input, sd)
betas = alphas / predictor_sd
# Below the unscaled coefficents for each input
betas  

beta0 = model$coefficients[1] - sum(alphas * predictor_mean / predictor_sd)
# Below the unscaled intercept
beta0 

# perform the predition
prediction = beta0 + sum(betas * data_point)  
prediction

# In the last homework, we used the follwing predictors: M + Ed + Po1 + U2 + Ineq + Prob, for which we get
# a predicted crime rate of 1304 and an adjusted R^2 value of 0.766. For this homework, we get  a predicted 
# crime rate of 1230 and an Adjusted R-squared value of 0.688. PCA model is unable to overcome the collinearity among the predictors. 
# The model from last homework is showing a higher Adjusted R-squared, thus we can conclude that is performing a better prediction.

