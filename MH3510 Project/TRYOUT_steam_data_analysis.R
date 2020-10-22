# graphical display of the observed data
steam_raw <- read.table("Steam.datt", header=FALSE)
steam <- data.frame(y=steam_raw$V2, x1=steam_raw$V3, x2=steam_raw$V4, x3=steam_raw$V9)
steam
plot(steam)

## modelling multiple linear regression
# fit a MLR model
mlr <- lm(y ~ x1+x2+x3, data=steam)
summary(mlr)

# extract regression quantities we need from the model object
names(mlr)

mlrs <- summary(mlr)
names(mlrs)

## adequacy checking (same case as SLR)
# 4.1 from viewpoint of fitted model

# 4.2 from viewpoint of residuals
# normality checking
qqnorm(residuals(mlr), ylab="residuals")
qqline(residuals(mlr))

## checking for time effects, non-constant variance and higher order curvatures
# draw some plots of residuals
par(mfrow=c(1,5))
plot(residuals(mlr), ylab="Residuals", xlab="Time")
plot(residuals(mlr), fitted(mlr), ylab="Residuals", xlab="Fitted values")
plot(residuals(mlr), steam$x1, ylab="Residuals", xlab="X1")
plot(residuals(mlr), steam$x2, ylab="Residuals", xlab="X2")
plot(residuals(mlr), steam$x3, ylab="Residuals", xlab="X3")

#durbin-watson tests
library(lmtest)
dwtest(y ~ x1+x2+x3, data=steam)

## F-test for reduced model and full model
# 5.1 test whether some coefficients are zeros
# Some F-tests
# H_0: beta1=beta2=0
mlr1 <- lm(y ~ x3, data=steam)
anova(mlr1,mlr)
# implies that we have to reject the null hypothesis at the level of 0.01

# test for more complicated relationship
# beta1_hat = 0.713592, beta2_hat = 0.330497
# H_0: beta1 = 2*beta2
mlr2 <- lm(y ~ I(2*x1+x2)+x3, data=steam)
summary(mlr2)
anova(mlr2,mlr)
# conclusion: we cannot reject the null hypothesis at the level of 0.1

# test whether coefficients are constant
# beta1_hat = 0.713592
# H_0: beta1 = 0.7
mlr3 <- lm(y ~ offset(0.7*x1)+x2+x3, data=steam)
summary(mlr3)
anova(mlr3,mlr)
