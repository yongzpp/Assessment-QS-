######### Reading Data #########
setwd("/Users/Asus/Downloads/")
library(MASS)
library(readr)
library(leaps)
library(car)

rm(list=ls())
data <- read_csv("cleaned1.csv")
View(data)

# Check the types of the variables in dataset
data <- data[, -1]
data <- subset(data, select=-c(floor_area_sqm))
sapply(data, class)
attach(data)

data$month = as.factor(month)
data$town = as.factor(town)
data$flat_type = as.factor(flat_type)
data$flat_model = as.factor(flat_model)
data$storey_range = as.factor(storey_range)
data$Name = as.factor(Name)
sapply(data, class)
attach(data)

######### Analysis on Response  #########
hist(log(resale_price))
range(resale_price)
qqnorm(log(resale_price))
qqline(log(resale_price))
boxplot(log(resale_price), xlab = "resale_price", main = "Boxplot of Resale Price")

######### Relationship of Response and Regressors  #########
# Find out outliers in regressors
boxplot(remaining_lease, xlab = "Remaining Lease", main = "Boxplot of Remaining Lease")
#boxplot(floor_area_sqm, xlab = "Floor Area", main = "Boxplot of Floor Area")

plot(remaining_lease, resale_price, main = "Resale Price vs Remaining Lease")
plot(floor_area_sqm, resale_price, main = "Resale Price vs Floor Area")
boxplot(resale_price ~ month, data=data, xlab = "Years", ylab = "Resale Price", main = "Resale Price vs Years")
boxplot(resale_price ~ Name, data=data, xlab = "Expressways", ylab = "Resale Price", main = "Resale Price vs Expressways")
boxplot(resale_price ~ town, data=data, xlab = "Town", ylab = "Resale Price", main = "Resale Price vs Town")
boxplot(resale_price ~ storey_range, data=data, xlab = "Storey Range", ylab = "Storey Range", main = "Resale Price vs Storey Range")
boxplot(resale_price ~ flat_type, data=data, xlab = "Flat Type", ylab = "Resale Price", main = "Resale Price vs Flat Type")
boxplot(resale_price ~ flat_model, data=data, xlab = "Flat Model", ylab = "Resale Price", main = "Resale Price vs Flat Model")

######### Model Fitting  #########
# Full model (no higher order or interaction terms)
model = lm(log(resale_price)~., data=data)
summary(model)
Anova(model, type = "II")

# Model adequacy check (Standardized Residual Plot)
plot(model$fitted.values, rstandard(model), xlab = "Fitted Values", 
     ylab = "Standardized Residuals", main = "Full Model", pch = 20)
length(rstandard(model)[which(rstandard(model) > 3)])
length(rstandard(model)[which(rstandard(model) < -3)])

# Model adequacy check (QQ Normality Plot)
qqnorm(rstandard(model),datax = TRUE, ylab = "Standardized Residuals", xlab = "Z scores", main = "Normal Probability Plot")
qqline(rstandard(model),datax = TRUE)

######### Multicollinearity  #########
# Unit length scaling before check multicollinearity 
Resale_Price = (resale_price-mean(resale_price))/sqrt(var(resale_price)*(nrow(data)-1))
Town = town
Flat_type = flat_type
Storey_range = storey_range
Flat_model = flat_model
Month = month
name = Name
#Floor_area_sqm <- (floor_area_sqm-mean(floor_area_sqm))/sqrt(var(floor_area_sqm)*(nrow(data)-1))
Remaining_lease <- (remaining_lease - mean(remaining_lease))/sqrt(var(remaining_lease)*(nrow(data)-1))

# Finding VIF
x<-cbind(Town, Flat_model, name, Flat_type, Remaining_lease, Storey_range, Month)
x<-cor(x)
C<-solve(x)
VIF<-diag(C)
VIF

# Finding eigen values
eigen(x)$values
max(eigen(x)$values)/eigen(x)$values
max(eigen(x)$values)/min(eigen(x)$values)

######### Outlier's Influence  #########
# Check for Cook's Distance
cooksd <- cooks.distance(model)
high_cook <- which(cooksd>4/nrow(data))
# Check high leverage points
high_lev <- which(hatvalues(model)>2*74/nrow(data))
# Check if coincide with outliers in SR plot
outliers = c(Reduce(intersect, list(high_lev, high_cook, which(rstandard(model) > 3))),
             Reduce(intersect, list(high_lev, high_cook, which(rstandard(model) < -3))))
outliers

######### Removing Outliers  #########
new_data <- data[-outliers, ]
sapply(new_data, class)
attach(new_data)

model1 = lm(log(resale_price)~., data=new_data)
summary(model1)
Anova(model1, type = "II")
boxcox(model1, lambda=seq(-2, 2, by=0.5), optimize=TRUE, plotit = TRUE)

# Model adequacy check (Standardized Residual Plot)
plot(model1$fitted.values, rstandard(model1), xlab = "Fitted Values", 
     ylab = "Standardized Residuals", main = "Full Model", pch = 20)
length(which(rstandard(model1) > 3))
length(which(rstandard(model1) < -3))

# Check for Cook's Distance
cooksd <- cooks.distance(model1)
high_cook <- which(cooksd>4/nrow(new_data))
# Check high leverage points
high_lev <- which(hatvalues(model1)>2*70/nrow(new_data))
# Check if coincide with outliers in SR plot
outliers = c(Reduce(intersect, list(high_lev, high_cook, which(rstandard(model1) > 3))),
             Reduce(intersect, list(high_lev, high_cook, which(rstandard(model1) < -3))))
outliers
