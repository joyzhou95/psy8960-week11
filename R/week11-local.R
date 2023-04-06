# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)
## Load the parallel package for parallelization 
library(parallel)
## Load the tictoc package for recording execution time  
library(tictoc)
set.seed(67)

# Data Import and Cleaning
gss_tbl_original <- read_sav("../data/GSS2016.sav") %>%
  filter(!is.na(MOSTHRS)) %>%
  ## Remove the other two workhour variables by not selecting them from the read datafile 
  select(-HRS1, -HRS2)


gss_tbl <- select(gss_tbl_original,
                  which(colSums(is.na(gss_tbl_original))/nrow(gss_tbl_original) < 0.75)) %>%
  rename(workhours = MOSTHRS)

# Visualization
gss_tbl %>%
  ggplot(aes(workhours)) +
  geom_histogram() +
  labs(x = "Number of Hourse Worked Last Week", y = "Frequency")

# Analysis

## OLS regression model
shuffle_row <- sample(nrow(gss_tbl))
shuffled_gss <- gss_tbl[shuffle_row,]
split <- round(nrow(shuffled_gss)*0.75)
train <- shuffled_gss[1:split,]
test <- shuffled_gss[(split + 1):nrow(shuffled_gss),]
folds <- createFolds(train$workhours, k = 10)

train <- sapply(train, as.numeric)

ols_model <- train(
  workhours ~.,
  data = train,
  method = "lm",
  na.action = na.pass,
  preProcess = c("center", "scale", "nzv", "medianImpute"),
  trControl = trainControl(
    method = "cv",
    number = 10,
    verboseIter = TRUE,
    search = "grid",
    indexOut = folds
  )
)

ols_test <- predict(ols_model, test, na.action = na.pass)
ols_test_r2 <- cor(ols_test, test$workhours)^2

## Elastic net model 
glmnet_model <- train(
  workhours ~.,
  data = train,
  method = "glmnet",
  na.action = na.pass,
  preProcess = c("center", "scale", "nzv", "medianImpute"),
  trControl = trainControl(
    method = "cv",
    number = 10,
    verboseIter = TRUE,
    search = "grid",
    indexOut = folds
  )
)

glmnet_test <- predict(glmnet_model, test, na.action = na.pass)
glmnet_test_r2 <-cor(glmnet_test, test$workhours)^2

## Random forest model
rf_model <- train(
  workhours ~.,
  data = train,
  method = "ranger",
  na.action = na.pass,
  preProcess =  c("center", "scale", "nzv", "medianImpute"),
  trControl = trainControl(
    method = "cv",
    number = 10,
    verboseIter = TRUE,
    search = "grid",
    indexOut = folds,
  )
)

rf_test <- predict(rf_model, test, na.action = na.pass)
rf_test_r2 <-cor(rf_test, test$workhours)^2

## eXtreme Gradient Boosting
gbm_model <- train(
  workhours ~.,
  data = train,
  method = "xgbLinear",
  na.action = na.pass,
  preProcess =  c("center", "scale", "nzv", "medianImpute"),
  trControl = trainControl(
    method = "cv",
    number = 10,
    verboseIter = TRUE,
    search = "grid",
    indexOut = folds,
  )
)

gbm_test <- predict(gbm_model, test, na.action = na.pass)
gbm_test_r2 <-cor(gbm_test , test$workhours)^2


# Publication 
algo <- c("OLS Regression Model", "Elastic Net Model",
          "Random Forest Model", "eXtreme Gradient Boosting Model")

cv_rsq1 <- c(round(ols_model$results$Rsquared,2), round(glmnet_model$results$Rsquared[2],2), 
             round(rf_model$results$Rsquared[3],2), round(gbm_model$results$Rsquared[7],2))

cv_rsq2 <- sapply(cv_rsq1, format, nsmall = 2)
cv_rsq <- str_remove(cv_rsq2, pattern = "^0")

ho_rsq1 <- c(round(ols_test_r2,2), round(glmnet_test_r2,2), round(rf_test_r2, gbm_test_r2),2)

ho_rsq2 <- sapply(ho_rsq1, formatC, format = "f", digits = 2)
ho_rsq <- str_remove(ho_rsq2, pattern = "^0")

table1_tbl <- tibble(algo, cv_rsq, ho_rsq) 

##
original 
parallelized 
table2_tbl



