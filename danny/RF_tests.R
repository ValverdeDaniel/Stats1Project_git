library(ggpubr)
library(lubridate)
library(caret)

# read data ---------------------------------------------------------------

dat <- read.csv("Lake_Powel_Inflow_Elevatoin_Outflow_Evap.csv")

# dat$days_since_beg <- as.numeric(mdy(dat$datetime))
# min(dat$days_since_beg)
# dat$days_since_beg <- dat$days_since_beg + 2196

dat$YEAR_MONTH <- paste0(year(mdy(dat$datetime)), "_", month(mdy(dat$datetime)))

dat2 <- aggregate(dat[, 2:6], list(dat$YEAR_MONTH), mean)

dat2$YEAR <- as.numeric(sapply(dat2$Group.1, function(x) unlist(strsplit(x, "_"))[1]))
dat2$MONTH <- as.numeric(sapply(dat2$Group.1, function(x) unlist(strsplit(x, "_"))[2]))

dat2$MONTHS_SINCE_BEG <- (dat2$YEAR - 1963) * 12 + (dat2$MONTH - 12)

#### SUBSETTING FOR DEMO
dat2 <- tail(dat2, 100)

# 80% - 20% split
set.seed(123)
train_idx <- sample(1:nrow(dat2), nrow(dat2) * .8)

train_dat <- dat2[train_idx, ]
test_dat <- dat2[-train_idx, ]

# fit RF model ------------------------------------------------------------

# Create control function for training with 10 folds. search method is grid.
control <- trainControl(method = "cv", 
                        number = 10, 
                        search = "grid")

# Create tunegrid with 15 values from 1:15 for mtry to tunning model. Our train function will change number of entry variable at each split according to tunegrid. 
tunegrid <- expand.grid(.mtry = (1:15)) 

rf_gridsearch <- train(pool.elevation ~ MONTHS_SINCE_BEG + total.inflows + total.release + evaporation + power.release,
                       data = train_dat,
                       method = "rf",
                       metric = "RMSE",
                       trControl = control,
                       tuneGrid = tunegrid)
print(rf_gridsearch)

predict(rf_gridsearch, newdata = test_dat)
test_dat$pool.elevation

predictions <- predict(rf_gridsearch, newdata = test_dat)
MSE_test <- sum((test_dat$pool.elevation - predictions)^2) / nrow(test_dat)
(RMSE <- sqrt(MSE_test))

## variable importance
varImp(rf_gridsearch)


# predict pool elevation at time i using last values for all other variables
i = 900
predict(rf_gridsearch, newdata = data.frame(MONTHS_SINCE_BEG = i, 
                                            total.inflows = test_dat$total.inflows[nrow(test_dat)],
                                            total.release = test_dat$total.release[nrow(test_dat)],
                                            evaporation = test_dat$evaporation[nrow(test_dat)],
                                            power.release = test_dat$power.release[nrow(test_dat)]))

# # fit kNN model -----------------------------------------------------------
# knn_model <- train(pool.elevation ~ MONTHS_SINCE_BEG + total.inflows + total.release + evaporation + power.release,
#                    data = train_dat,
#                    method = "knn",
#                    metric = "RMSE",
#                    trControl = control)

