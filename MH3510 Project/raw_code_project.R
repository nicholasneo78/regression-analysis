# read the dataset
data_raw = read.table("aadt.txt",header=FALSE)
# check data by showing the first few entries of data
head(data_raw,5)

# select only the columns with the predictor and response variable
data = data.frame(y=data_raw$V1,
                  x1=data_raw$V2,
                  x2=data_raw$V3,
                  x3=data_raw$V4,
                  x4=data_raw$V5)

#defining X4 to be 1 if there is access control and 0 if there isn't
data$x4[data$x4==1] <- 0
data$x4[data$x4==2] <- 1

# take a look at the modification to the data
head(data,5)

# plot the scatter plot matrix
pairs(data,panel=panel.smooth)

# fit a multiple linear regression model
mlr1 <- lm(y ~ x1+x2+x3+x4+I(exp(x2)), data=data)
summary(mlr1)

# F-test between 2 models (full and model without predictor variable X3)
mlr1_alt <- lm(y ~ x1+x2+x4+I(exp(x2)), data=data)
# ANOVA
anova(mlr1_alt,mlr1)

# normality checking
qqnorm(residuals(mlr1_alt), ylab='Residuals')
qqline(residuals(mlr1_alt))

# draw some plots of the residuals against each predictor variable
par(mfrow=c(1,4))
plot(fitted(mlr1_alt), residuals(mlr1_alt), ylab='Residuals', xlab='Fitted values')
plot(data$x1, residuals(mlr1_alt), ylab='Residuals', xlab='X1')
plot(data$x2, residuals(mlr1_alt), ylab='Residuals', xlab='X2')
plot(data$x4, residuals(mlr1_alt), ylab='Residuals', xlab='X4')

# fit a multiple linear regression model 2
mlr2 <- lm(y ~ x1+x2+x4+I(exp(x2))+I(x1*x2)+I(x1*x4), data=data)
summary(mlr2)

# F-test between 2 models (full and model without predictor variable X4)
mlr2_alt <- lm(y ~ x1+x2+I(exp(x2))+I(x1*x2)+I(x1*x4), data=data)
# ANOVA
anova(mlr2_alt,mlr2)

# normality checking
qqnorm(residuals(mlr2_alt), ylab='Residuals')
qqline(residuals(mlr2_alt))

# fit a multiple linear regression model 3
mlr3 <- lm(y ~ x1+x2+I(exp(x2))+I(x1*x2)+I(x1*x4), data=data)
summary(mlr3)

# F-test between 3 models (full and model without predictor variable X1X2)
mlr3_alt <- lm(y ~ x1+x2+I(exp(x2))+I(x1*x4), data=data)
# ANOVA
anova(mlr3_alt,mlr3)

# prediction
pred_data = data.frame(x1=50000, x2=3, x4=2)
# confidence interval of mean response
predict(mlr3,pred_data,interval='confidence', level=0.95)
# confidence interval of the predicted y value
predict(mlr3,pred_data,interval='prediction', level=0.95)