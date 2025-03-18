---
title: "Support Vector Machines"
author: "Daniel Fuller"
date: "2024-09-23"
output:
      html_document:
        keep_md: true
---


``` r
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(tidymodels)
library(sjPlot)
library(finalfit)
library(knitr)
library(gtsummary)
library(mlbench)
library(kernlab)
library(vip)
library(rsample)
library(tune)
library(recipes)
library(yardstick)
library(parsnip)
library(glmnet)
library(themis)
library(microbenchmark)
```

# Support Vector Machines

## Research question and data

We are using an imputed (ie. no missing data) version of the CanPath student dataset [https://canpath.ca/student-dataset/](https://canpath.ca/student-dataset/). The nice thing about this dataset is that it's pretty big in terms of sample size, has lots of variables, and we can use it for free. 

Our research question is:  

- **Can we develop a model that will predict type 2 diabetes**

### Reading in data

Here are reading in data and getting organized to run our models. 


``` r
data <- read_csv("mice_all_imp.csv")
```

```
## Rows: 41187 Columns: 93
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (1): ID
## dbl (92): ADM_STUDY_ID, SDC_GENDER, SDC_AGE_CALC, SDC_MARITAL_STATUS, SDC_ED...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
data <- data %>% mutate_at(3, factor)
data <- data %>% mutate_at(5:6, factor)
data <- data %>% mutate_at(8:9, factor)
data <- data %>% mutate_at(12:13, factor)
data <- data %>% mutate_at(15:81, factor)
data <- data %>% mutate_at(83:93, factor)

table(data$DIS_DIAB_EVER)
```

```
## 
##     0     1     2 
## 36714  3114  1359
```

``` r
data <- data %>%
	mutate(diabetes = case_when(
		DIS_DIAB_EVER == 0 ~ 0,
		DIS_DIAB_EVER == 1 ~ 1,
		DIS_DIAB_EVER == 2 ~ 0)) %>%
		mutate(diabetes = as.factor(diabetes))

table(data$DIS_DIAB_EVER, data$diabetes)
```

```
##    
##         0     1
##   0 36714     0
##   1     0  3114
##   2  1359     0
```

``` r
data$DIS_DIAB_EVER <- NULL
```


``` r
data <- select(data, diabetes, SDC_AGE_CALC, SDC_EDU_LEVEL, PM_BMI_SR, HS_GEN_HEALTH, WRK_FULL_TIME, SMK_CIG_EVER, SDC_INCOME, PA_TOTAL_SHORT, HS_ROUTINE_VISIT_EVER, PSE_ADULT_WRK_DURATION, DIS_RESP_SLEEP_APNEA_EVER, SDC_EDU_LEVEL_AGE, SDC_GENDER)
```

# Support Vector Machine

### Creating training and testing data


``` r
set.seed(10)

#### Cross Validation Split
cv_split <- initial_validation_split(data, 
                            strata = diabetes, 
                            prop = c(0.70, 0.20))

# Create data frames for the two sets:
train_data <- training(cv_split)
table(train_data$diabetes)
```

```
## 
##     0     1 
## 26640  2190
```

``` r
test_data  <- testing(cv_split)
table(test_data$diabetes)
```

```
## 
##    0    1 
## 3813  306
```

### V folds


``` r
folds <- vfold_cv(training(cv_split), v = 5, strata = diabetes)
```

## Recipe


``` r
svm_recipe <- 
  recipe(diabetes ~ ., data = train_data) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors(), -all_outcomes()) %>%
  step_normalize(all_numeric_predictors())
```

step_smotenc(diabetes, over_ratio = 0.9) %>%

### Model 

Here we use the tidy models to setup a model using `kernlab` and `classification` and we call the specific model we want to fit. 

* __mtry__: An integer for the number of predictors that will be randomly sampled at each split when creating the tree models.
* __trees__: An integer for the number of trees contained in the ensemble.
* __min_n__: An integer for the minimum number of data points in a node that are required for the node to be split further.

https://emilhvitfeldt.github.io/ISLR-tidymodels-labs/09-support-vector-machines.html



``` r
svm_model <- svm_poly(degree = 1) %>% 
  set_mode("classification") %>%
  set_engine("kernlab")
svm_model
```

```
## Polynomial Support Vector Machine Model Specification (classification)
## 
## Main Arguments:
##   degree = 1
## 
## Computational engine: kernlab
```

### Workflow

```{}
svm_grid <- grid_regular(
              rbf_sigma(range = c(-2, 2)),
              levels = 2  
            )
```



``` r
roc_res <- metric_set(roc_auc) 
```

##### NOT RUN

```{}
svm_workflow <- 
  workflow() %>% 
  add_model(svm_model) %>% 
  add_recipe(svm_recipe) %>% 
    tune_grid(resamples = folds,
              metrics = roc_res,
                control = control_grid(save_pred = FALSE, 
                verbose = TRUE)) ## Edit for running live
```

##### NOT RUN

```{}
collect_metrics(svm_initial)
```



# Resources

1. https://r4ds.github.io/bookclub-tmwr/svm-model-as-motivating-example.html
2. https://emilhvitfeldt.github.io/ISLR-tidymodels-labs/09-support-vector-machines.html
