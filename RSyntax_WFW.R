
# Install packages

library(tidyverse)
library(lmerTest)
library(lme4)
library(caret)

# Simulate data

# Region ID
id <- rep(1:1000, each = 23)

# Year
# 2000 to 2022, 100 regions 
years <- rep(2000:2022, times = 100)

# Fire binary variable
fire <- sample(0:1,23000,replace=TRUE)

# Create data
dat <- data.frame(id, years, fire)

# Lat/Long
# 51.827575, -125.407723 = top left          51.557874, -116.705937 = top right
# 49.136905, -125.262693 = bottom left      49.310558, -116.222505 = bottom right

# Generate values so that fires mostly happen inland

# Latitude
lat <- c()
for (i in 1:23000) {
  
  if (dat[i,]$fire == 1) {
    lat <- append(lat, runif(1, -120, -116))
  } else {
    lat <- append(lat, runif(1, -125, -118))
  }
  
}
dat <- data.frame(dat, lat)

# Longitude
long <- c()
for (i in 1:23000) {
  
  if (dat[i,]$fire == 1) {
    long <- append(long, runif(1, 49.5, 51.5))
  } else {
    long <- append(long, runif(1, 48, 51))
  }
  
}
dat <- data.frame(dat, long)


# Temperature variables


apr_temp <- c()
for (i in 1:23000) {

  if (dat[i,]$fire == 1) {
    apr_temp <- append(apr_temp, runif(1, 7, 18))
  } else {
    apr_temp <- append(apr_temp, runif(1, 3, 15))
  }
  
}
dat <- data.frame(dat, apr_temp)

may_temp <- c()
for (i in 1:23000) {
  
  if (dat[i,]$fire == 1) {
    may_temp <- append(may_temp, runif(1, 10, 25))
  } else {
    may_temp <- append(may_temp, runif(1, 7, 20))
  }
  
}
dat <- data.frame(dat, may_temp)

june_temp <- c()
for (i in 1:23000) {
  
  if (dat[i,]$fire == 1) {
    june_temp <- append(june_temp, runif(1, 14, 27))
  } else {
    june_temp <- append(june_temp, runif(1, 11, 24))
  }
  
}
dat <- data.frame(dat, june_temp)

july_temp <- c()
for (i in 1:23000) {
  
  if (dat[i,]$fire == 1) {
    july_temp <- append(july_temp, runif(1, 17, 30))
  } else {
    july_temp <- append(july_temp, runif(1, 13, 28))
  }
  
}
dat <- data.frame(dat, july_temp)

aug_temp <- c()
for (i in 1:23000) {
  
  if (dat[i,]$fire == 1) {
    aug_temp <- append(aug_temp, runif(1, 17, 30))
  } else {
    aug_temp <- append(aug_temp, runif(1, 13, 28))
  }
  
}
dat <- data.frame(dat, aug_temp)



# Rain variables


apr_rain <- c()
for (i in 1:23000) {
  
  if (dat[i,]$fire == 1) {
    apr_rain <- append(apr_rain, runif(1, 0, 6))
  } else {
    apr_rain <- append(apr_rain, runif(1, 2, 10))
  }
  
}
dat <- data.frame(dat, apr_rain)


may_rain <- c()
for (i in 1:23000) {
  
  if (dat[i,]$fire == 1) {
    may_rain <- append(may_rain, runif(1, 12, 25))
  } else {
    may_rain <- append(may_rain, runif(1, 22, 30))
  }
  
}
dat <- data.frame(dat, may_rain)

june_rain <- c()
for (i in 1:23000) {
  
  if (dat[i,]$fire == 1) {
    june_rain <- append(june_rain, runif(1, 30, 60))
  } else {
    june_rain <- append(june_rain, runif(1, 50, 80))
  }
  
}
dat <- data.frame(dat, june_rain)

july_rain <- c()
for (i in 1:23000) {
  
  if (dat[i,]$fire == 1) {
    july_rain <- append(july_rain, runif(1, 30, 60))
  } else {
    july_rain <- append(july_rain, runif(1, 50, 80))
  }
  
}
dat <- data.frame(dat, july_rain)

aug_rain <- c()
for (i in 1:23000) {
  
  if (dat[i,]$fire == 1) {
    aug_rain <- append(aug_rain, runif(1, 30, 60))
  } else {
    aug_rain <- append(aug_rain, runif(1, 50, 80))
  }
  
}
dat <- data.frame(dat, aug_rain)



# Generate days since new years for when first wildfire forest occurred
# Ranges from mid July to mid Sept
# 226 - 286
# Set a normal dist so they mostly occur in Aug

date_fire <- c()
for (i in 1:23000) {
  
  if (dat[i,]$fire == 1) {
    date_fire <- append(date_fire, round(rnorm(1, mean=240, sd=5),0))
  } else {
    date_fire <- append(date_fire, round(rnorm(1, mean=266, sd=10),0))
  }
  
}

dat <- data.frame(dat, date_fire)





# Split train and test data
set.seed(123)
training.samples <- dat$id %>%
  createDataPartition(p = 0.8, list = FALSE)

train.data  <- dat[training.samples, ]
test.data <- dat[-training.samples, ]

# Build the model
mod <- lm(date_fire ~ lat + long + apr_temp + may_temp + june_temp + aug_temp + 
              apr_rain + may_rain + june_rain + july_rain + july_rain + aug_rain, 
          data = train.data)
summary(mod)

# Make predictions and compute the R2, RMSE and MAE
predictions <- mod %>% predict(test.data)
data.frame( R2 = R2(predictions, test.data$date_fire),
            RMSE = RMSE(predictions, test.data$date_fire),
            MAE = MAE(predictions, test.data$date_fire))

# Calculate the error rate
RMSE(predictions, test.data$date_fire)/mean(test.data$date_fire) # 4% error


# K-fold cross-validation

set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)

# Train the model
model <- train(date_fire ~., data = dat, method = "lm",
               trControl = train.control)

# Summarize the results
print(model)




# Testing prediction using high risk case:
# 50.905458, -116.419338
# East Kootenay
# Very dry, very hot

user.data <- data.frame(
  lat = -116.419338,
  long =  50.905458,
  apr_temp = 15,
  may_temp = 20,
  june_temp = 22,
  july_temp = 30,
  aug_temp = 30,
  apr_rain = 4,
  may_rain = 5,
  june_rain = 5,
  july_rain = 5,
  aug_rain = 6
)

predict(mod, newdata = user.data) 
# 208.6411 
# July 27

# Low risk case
# 49.097883, -124.029345
# Nanaimo
# very wet, very cool

user.data <- data.frame(
  lat = -124.029345,
  long =  49.097883,
  apr_temp = 4,
  may_temp = 10,
  june_temp = 12,
  july_temp = 14,
  aug_temp = 15,
  apr_rain = 8,
  may_rain = 22,
  june_rain = 65,
  july_rain = 79,
  aug_rain = 70
)

predict(mod, newdata = user.data)
# 272.6421 
# Sept 30

# SAve data to export to use with python 
write.csv(dat, file = "predictivedat.csv")



