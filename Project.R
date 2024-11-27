setwd("/Users/le.bill/Desktop/uni /statistics")

#libraries
library(readr)
library(dplyr)
library(tidyr)
library(MASS)

#dataset
car_data <- read.csv("/Users/le.bill/Desktop/uni /statistics/scrap price.csv")

#####EDA PART#####

#check the packaging + top and bottom of data
str(car_data)
head(car_data)
tail(car_data)

#check for n values
sum(is.na(car_data))

#check for unique values 
sapply(car_data, function(x) length(unique(x)))

#descriptive statistics for dataset
summary(car_data)

desc_stats_select <- summary(car_data[c("price", "horsepower")])
desc_stats_select

#histograms
par(mfrow = c(1, 2))
# Visualizing distribution of price variable
hist(car_data$price, main="Histogram of Price", xlab="Price")
# Visualizing distribution of horsepower variable
hist(car_data$horsepower, main="Histogram of Horsepower", xlab="Horsepower")
par(mfrow = c(1, 1))

#creating boxplots of price by carbody
boxplot(price ~ carbody, data = car_data, main = "Price by Carbody Type", xlab = "Carbody", ylab = "Price")

#creating a bar plot to visualize distribution of carbody variable
carbody_counts <- table(car_data$carbody)
barplot(carbody_counts, main="Distribution of Carbody Types", xlab="Carbody Type", ylab="Frequency", col="gray")

#creating a scatterplot
plot(car_data$horsepower, car_data$price, main = "Horsepower vs. Price", xlab = "Horsepower", ylab = "Price")

#creating a scatterplot for horsepower and price for each carbody category for further insight
ggplot(car_data, aes(x = horsepower, y = price, color = carbody)) + 
  geom_point() +
  facet_wrap(~carbody) +
  labs(title = "Horsepower vs. Price by Carbody Type", x = "Horsepower", y = "Price") +
  theme_minimal()

#creating correlation matrix
library(corrplot)

numerical_data <- car_data[,sapply(car_data, is.numeric)] 
cor_matrix <- cor(numerical_data, use="complete.obs")
corrplot(cor_matrix, method="circle")

price_correlations <- cor_matrix['price',]
threshold <- 0.5
significant_variables <- names(price_correlations[abs(price_correlations) > threshold & names(price_correlations) != "price"])
significant_correlations <- price_correlations[significant_variables]
print(significant_correlations)
#we can see that the top 3 correlated variables are: engine size, horsepower, and curbweight

#fitting the model 
model_subset <- car_data[car_data$carbody %in% c("hatchback", "sedan", "wagon"), ]
model_data <- lm(price ~ horsepower * carbody, data = model_subset)

#making sure the model fits all assumptions for ANCOVA moderated regression
#we first check for linearity
ggplot(car_data, aes(x = horsepower, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "purple") +  
  labs(title = "Price vs. Horsepower", x = "Horsepower", y = "Price")
#linearity met

#then the independance of residuals
plot(model_data, which = 1)

#transforming the model for better model
c <- boxcox(model)
lambda <- bc$x[which.max(bc$y)]
car_data$price_transformed <- (car_data$price^lambda - 1) / lambda
model_bc <- lm(log(price_transformed) ~ log(horsepower) * carbody, data = model_subset)
plot(model_bc, which = 1)

#checking for normality of residuals
hist(residuals(model_bc), breaks = "FD", main = "Histogram of Residuals")

shapiro.test(residuals(model_bc))

#identifying outliers through cooks distance
cooks_d <- cooks.distance(model_bc)
#print(cooksd)

# creating threshold
threshold <- 4 / (nrow(car_data) - length(coef(model_bc)))

# Identify outliers based on the threshold
outliers <- which(cooks_d > threshold)

# Remove outliers from the dataset
car_data_clean <- car_data[-outliers, ]
model_subset <- car_data_clean[car_data_clean$carbody %in% c("hatchback", "sedan", "wagon"), ]
model_bc_clean <- lm(log(price_transformed) ~ log(horsepower) * carbody, data = model_subset)


shapiro.test(residuals(model_bc_clean))

qqnorm(residuals(model_bc_clean))
qqline(residuals(model_bc_clean))
      
#checking for homoscedasticity 
plot(model_bc, which = 3)

#checking for no perfect multicollinearity 
library(car)
vif(model_bc_clean)

#checking for homogenity of regression slopes
reduced_model <- lm(log(price_transformed) ~ log(horsepower) + carbody, data = model_subset)
model_comparison <- anova(reduced_model, model_bc_clean)

model_comparison

#fitting the chosen model into data
model_bc_clean <- lm(log(price_transformed) ~ log(horsepower) * carbody, data = model_subset)
summary(model_bc_clean)

#visualization 
ggplot(model_subset, aes(x = log(horsepower), y = log(price), color = carbody)) +
  geom_point(alpha = 0.6) +  
  geom_smooth(method = "lm", se = FALSE) +  
  labs(title = "Interaction Effect of Horsepower and Carbody on Price",
       x = "Log of Horsepower",
       y = "Log of Price",
       color = "Carbody") +
  theme_minimal()
