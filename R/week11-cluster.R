# Script Settings and Resources
library(tidyverse)
library(haven)
library(caret)
## Load the parallel and doParallel packages for parallelization 
library(parallel)
library(doParallel)
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

# Analysis
shuffle_row <- sample(nrow(gss_tbl))
shuffled_gss <- gss_tbl[shuffle_row,]
split <- round(nrow(shuffled_gss)*0.75)
train <- shuffled_gss[1:split,]
test <- shuffled_gss[(split + 1):nrow(shuffled_gss),]
folds <- createFolds(train$workhours, k = 10)

train <- sapply(train, as.numeric)

## OLS regression model
tic()
ols_model <- train(
  workhours ~.,
  data = train,
  method = "lm",
  na.action = na.pass,
  preProcess ="medianImpute",
  trControl = trainControl(
    method = "cv",
    number = 10,
    verboseIter = TRUE,
    search = "grid",
    indexOut = folds
  )
)
ols_runtime <- toc()

ols_test <- predict(ols_model, test, na.action = na.pass)
ols_test_r2 <- cor(ols_test, test$workhours)^2

## Elastic net model 
tic()
glmnet_model <- train(
  workhours ~.,
  data = train,
  method = "glmnet",
  na.action = na.pass,
  preProcess = "medianImpute",
  trControl = trainControl(
    method = "cv",
    number = 10,
    verboseIter = TRUE,
    search = "grid",
    indexOut = folds
  )
)
glm_runtime <- toc()

glmnet_test <- predict(glmnet_model, test, na.action = na.pass)
glmnet_test_r2 <-cor(glmnet_test, test$workhours)^2

## Random forest model
tic()
rf_model <- train(
  workhours ~.,
  data = train,
  method = "ranger",
  na.action = na.pass,
  preProcess =  "medianImpute",
  trControl = trainControl(
    method = "cv",
    number = 10,
    verboseIter = TRUE,
    search = "grid",
    indexOut = folds,
  )
)
rf_runtime <- toc()

rf_test <- predict(rf_model, test, na.action = na.pass)
rf_test_r2 <-cor(rf_test, test$workhours)^2

## eXtreme Gradient Boosting
tic()
gbm_model <- train(
  workhours ~.,
  data = train,
  method = "xgbLinear",
  na.action = na.pass,
  preProcess = "medianImpute",
  trControl = trainControl(
    method = "cv",
    number = 10,
    verboseIter = TRUE,
    search = "grid",
    indexOut = folds,
  )
)
gbm_runtime <-toc()

gbm_test <- predict(gbm_model, test, na.action = na.pass)
gbm_test_r2 <-cor(gbm_test , test$workhours)^2


##########Parallelization##########
## Make clusters for parallelization and assign the cluster to all following functions that are parallelized 
cluster <- makeCluster(7)
registerDoParallel(cluster)

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
shuffle_row <- sample(nrow(gss_tbl))
shuffled_gss <- gss_tbl[shuffle_row,]
split <- round(nrow(shuffled_gss)*0.75)
train <- shuffled_gss[1:split,]
test <- shuffled_gss[(split + 1):nrow(shuffled_gss),]
folds <- createFolds(train$workhours, k = 10)

train <- sapply(train, as.numeric)

## OLS regression model
tic()
ols_model <- train(
  workhours ~.,
  data = train,
  method = "lm",
  na.action = na.pass,
  preProcess ="medianImpute",
  trControl = trainControl(
    method = "cv",
    number = 10,
    verboseIter = TRUE,
    search = "grid",
    indexOut = folds
  )
)
ols_runtime_p <- toc()

ols_test <- predict(ols_model, test, na.action = na.pass)
ols_test_r2 <- cor(ols_test, test$workhours)^2

## Elastic net model 
tic()
glmnet_model <- train(
  workhours ~.,
  data = train,
  method = "glmnet",
  na.action = na.pass,
  preProcess = "medianImpute",
  trControl = trainControl(
    method = "cv",
    number = 10,
    verboseIter = TRUE,
    search = "grid",
    indexOut = folds
  )
)
glm_runtime_p <-toc()

glmnet_test <- predict(glmnet_model, test, na.action = na.pass)
glmnet_test_r2 <-cor(glmnet_test, test$workhours)^2

## Random forest model
tic()
rf_model <- train(
  workhours ~.,
  data = train,
  method = "ranger",
  na.action = na.pass,
  preProcess =  "medianImpute",
  trControl = trainControl(
    method = "cv",
    number = 10,
    verboseIter = TRUE,
    search = "grid",
    indexOut = folds,
  )
)
rf_runtime_p <-toc()

rf_test <- predict(rf_model, test, na.action = na.pass)
rf_test_r2 <-cor(rf_test, test$workhours)^2

## eXtreme Gradient Boosting
tic()
gbm_model <- train(
  workhours ~.,
  data = train,
  method = "xgbLinear",
  na.action = na.pass,
  preProcess =  "medianImpute",
  trControl = trainControl(
    method = "cv",
    number = 10,
    verboseIter = TRUE,
    search = "grid",
    indexOut = folds,
  )
)
gbm_runtime_p <- toc()

gbm_test <- predict(gbm_model, test, na.action = na.pass)
gbm_test_r2 <-cor(gbm_test , test$workhours)^2

# Stop parallel processing
stopCluster(cluster)
registerDoSEQ()

# Publication 
algo <- c("OLS Regression Model", "Elastic Net Model",
          "Random Forest Model", "eXtreme Gradient Boosting Model")

cv_rsq1 <- c(round(ols_model$results$Rsquared,2), round(glmnet_model$results$Rsquared[2],2), 
             round(rf_model$results$Rsquared[3],2), round(gbm_model$results$Rsquared[7],2))

cv_rsq2 <- sapply(cv_rsq1, format, nsmall = 2)
cv_rsq <- str_remove(cv_rsq2, pattern = "^0")

ho_rsq1 <- c(round(ols_test_r2,2), round(glmnet_test_r2,2), round(rf_test_r2,2), round(gbm_test_r2,2))

ho_rsq2 <- sapply(ho_rsq1, formatC, format = "f", digits = 2)
ho_rsq <- str_remove(ho_rsq2, pattern = "^0")

table3_tbl <- tibble(algo, cv_rsq, ho_rsq) %>%
  write_csv("table3.csv")

##
original <- c(round(ols_runtime$toc-ols_runtime$tic,2), round(glm_runtime$toc-glm_runtime$tic,2),
              round(rf_runtime$toc-rf_runtime$tic,2), round(gbm_runtime$toc-gbm_runtime$tic,2))
parallelized <- c(round(ols_runtime_p$toc-ols_runtime_p$tic,2), round(glm_runtime_p$toc-glm_runtime_p$tic,2),
                  round(rf_runtime_p$toc-rf_runtime_p$tic,2), round(gbm_runtime_p$toc-gbm_runtime_p$tic,2))

table4_tbl <- tibble(algo, cv_rsq, ho_rsq, original, parallelized) %>% 
  write_csv("table4.csv")
