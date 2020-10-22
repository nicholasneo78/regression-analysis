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

