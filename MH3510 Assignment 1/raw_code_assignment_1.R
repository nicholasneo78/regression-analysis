# Store the data into variable X & Y
X <- c(40,50,60,70,80,90,40,60,80,50)
Y <- c(69,175,272,335,490,415,72,265,492,180)

# a) Fit simple regression to the data
slr <- lm(Y~X)
# Display the information of this model
slr
# Plot the data
plot(X,Y)
# Fit a simple linear regression line to the plot
abline(slr)

# b) Obtain the residuals and examine them
# Obtain residuals
residual <- slr$res
# display the residuals value
residual
# examine the residuals by plotting the residual against X plot
plot(X,residual)

# Extra analysis
summary(slr)
anova(slr)
