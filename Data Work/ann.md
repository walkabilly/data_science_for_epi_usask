---
title: "Artificial Neural Networks"
author: "Daniel Fuller"
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
library(AppliedPredictiveModeling)
library(torch)
library(brulee)
library(neuralnet)
```

# Artificial Neural Networks

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
data <- data %>% mutate_at(12:12, factor)
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
data_continuous <- select(data, diabetes, 
                            PSE_ADULT_WRK_DURATION, 
                            PM_BMI_SR, 
                            PA_TOTAL_SHORT, 
                            SDC_HOUSEHOLD_CHILDREN_NB, 
                            SDC_HOUSEHOLD_ADULTS_NB, 
                            SDC_EDU_LEVEL_AGE, 
                            SDC_AGE_CALC, 
                            SDC_GENDER)
```

# Artificial Neural Networks

### Creating training and testing data


``` r
set.seed(10)

#### Cross Validation Split
cv_split <- initial_validation_split(data_continuous, 
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
folds <- vfold_cv(training(cv_split), v = 3, strata = diabetes)
```

## Recipe


``` r
diabetes_recipe <- 
  recipe(diabetes ~ ., data = train_data) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors(), -all_outcomes()) %>%
  step_normalize(all_numeric_predictors())
```


### Model 

The `mlp` argument in Tidymodels is for a Multi-Layer Perceptron (basically another name for the artificial neural network). There are 5 different engines that can be used with the `mlp` argument 

* nnet (default)
* brulee
* brulee_two_layer
* h2o (requires parsnip extension)
* keras

The default `nnet` engine is pretty basic and only does a feed-forward neural networks with a single hidden layer (for multinomial log-linear models). Arguably [keras](https://rstudio.github.io/cheatsheets/html/keras.html) is the most powerful of these engines and is supported in lots of different languages. Keras is similar to Tidymodels in that it has a set of specific verbs that are used to develop the model. 

* Define
* Compile
* Fit
* Evaluate
* Predict 

For the sake of consistency we are going to use the `brulee` engine because we do a single a two layer model without having to learn the new `keras` specific workflow. The book [Deep Learning with R](https://www.manning.com/books/deep-learning-with-r-second-edition) provides a comprehensive guide to `keras` and `tensorflow` which you will need to learn if you are working with really big data. 

#### Hyperparameters

As discussed in the lecture. The hyperparameters for ANN's are were things go from manageable to complex very quickly. Our previous models had at most 3 hyperparameters and now we are dealing with 6 hyperparameters. The defaults of these hyperparameters will change depending on the engine used. The defaults for `brulee` are [available here](https://brulee.tidymodels.org/reference/brulee_mlp.html).

* __hidden_units__: An integer for the number of units in the hidden model.
* __penalty__: A non-negative numeric value for the amount of weight decay.
* __dropout__: A number between 0 (inclusive) and 1 denoting the proportion of model parameters randomly set to zero during model training.
* __epochs__: An integer for the number of training iterations.
* __activation__: A single character string denoting the type of relationship between the original predictors and the hidden unit layer. The activation function between the hidden and output layers is automatically set to either "linear" or "softmax" depending on the type of outcome. 
* __learn_rate__: A number for the rate at which the boosting algorithm adapts from iteration-to-iteration (specific engines only). This is sometimes referred to as the shrinkage parameter.

## Very basic model 

One of the downsides of Tidymodels is that you often lose the ability to plot models because tidymodels doesn't import those plotting functions very well. Let's train a very basic model with the [neuralnet](https://cran.r-project.org/web/packages/neuralnet/index.html) package so we can show what a model might look like. 

##### Run during class

```{}
## I've removed all categorical variables because they need to be dummy coded and I'm lazy

basic_ann <- neuralnet(diabetes ~ PSE_ADULT_WRK_DURATION + 
                            PM_BMI_SR + 
                            PA_TOTAL_SHORT +
                            SDC_HOUSEHOLD_CHILDREN_NB + 
                            SDC_HOUSEHOLD_ADULTS_NB + 
                            SDC_EDU_LEVEL_AGE,
                data = train_data,
                hidden = c(3, 2), ## Here we specify 3 hidden nodes and 1 layer
                linear.output = FALSE
                )

plot(basic_ann, rep = "best")
```

## Tidymodels implementation


``` r
set.seed(10)

mlp_model <- mlp(epochs = tune(), hidden_units = tune(), penalty = tune(), learn_rate = tune(), activation = tune()) %>% 
                  set_engine("brulee", validation = 0) %>% 
                  set_mode("classification")
```

### Workflow


``` r
mlp_workflow <- 
  workflow() %>% 
  add_model(mlp_model) %>% 
  add_recipe(diabetes_recipe) %>% 
    tune_grid(resamples = folds,
                control = control_grid(save_pred = FALSE, 
                verbose = FALSE)) ## Edit for running live
```

### Workflow results


``` r
metrics_tune <- collect_metrics(mlp_workflow)

show_best(mlp_workflow, metric='accuracy', n=5)  # only show the results for the best 5 models
```

```
## # A tibble: 5 × 11
##   hidden_units     penalty epochs activation learn_rate .metric .estimator  mean
##          <int>       <dbl>  <int> <chr>           <dbl> <chr>   <chr>      <dbl>
## 1            2     5.99e-3    445 tanh          0.0359  accura… binary     0.924
## 2            7     2.15e-7    115 relu          0.001   accura… binary     0.924
## 3           23     7.74e-2      5 tanh          0.0736  accura… binary     0.924
## 4           34     1   e+0    280 elu           0.00419 accura… binary     0.924
## 5           28     1.29e-9    500 log_sigmo…    0.00858 accura… binary     0.924
## # ℹ 3 more variables: n <int>, std_err <dbl>, .config <chr>
```

``` r
plot(autoplot(mlp_workflow, metric = 'accuracy'))
```

![](ann_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

### Final model - Test data


``` r
mlp_best <- 
  mlp_workflow %>% 
  select_best(metric = "accuracy")

mlp_final_model <- finalize_model(
                          mlp_model,
                          mlp_best
                          )
mlp_final_model
```

```
## Single Layer Neural Network Model Specification (classification)
## 
## Main Arguments:
##   hidden_units = 2
##   penalty = 0.00599484250318942
##   epochs = 445
##   activation = tanh
##   learn_rate = 0.0359381366380463
## 
## Engine-Specific Arguments:
##   validation = 0
## 
## Computational engine: brulee
```

``` r
final_mlp_workflow <- workflow() %>%
                      add_recipe(diabetes_recipe) %>%
                      add_model(mlp_final_model)

final_mlp_results <- final_mlp_workflow %>%
                    last_fit(cv_split)

mlp_results <- final_mlp_results %>% collect_metrics()
```

## Final Results


``` r
kable(mlp_results)
```



|.metric     |.estimator | .estimate|.config              |
|:-----------|:----------|---------:|:--------------------|
|accuracy    |binary     | 0.9257101|Preprocessor1_Model1 |
|roc_auc     |binary     | 0.6676441|Preprocessor1_Model1 |
|brier_class |binary     | 0.0671579|Preprocessor1_Model1 |

## Variable Importance

No variable importance? Any idea why? 

# Resources

1. https://www.tidymodels.org/learn/models/parsnip-nnet/
2. https://www.datacamp.com/tutorial/neural-network-models-r


## Session Info


``` r
sessionInfo()
```

```
## R version 4.4.2 (2024-10-31)
## Platform: aarch64-apple-darwin20
## Running under: macOS Sequoia 15.3.2
## 
## Matrix products: default
## BLAS:   /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRblas.0.dylib 
## LAPACK: /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## time zone: America/Regina
## tzcode source: internal
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] neuralnet_1.44.2                brulee_0.4.0                   
##  [3] torch_0.14.2                    AppliedPredictiveModeling_1.1-7
##  [5] microbenchmark_1.5.0            themis_1.0.3                   
##  [7] glmnet_4.1-8                    Matrix_1.7-1                   
##  [9] vip_0.4.1                       kernlab_0.9-33                 
## [11] mlbench_2.1-6                   gtsummary_2.1.0                
## [13] knitr_1.49                      finalfit_1.0.8                 
## [15] sjPlot_2.8.17                   yardstick_1.3.2                
## [17] workflowsets_1.1.0              workflows_1.2.0                
## [19] tune_1.3.0                      rsample_1.2.1                  
## [21] recipes_1.1.1                   parsnip_1.3.1                  
## [23] modeldata_1.4.0                 infer_1.0.7                    
## [25] dials_1.4.0                     scales_1.3.0                   
## [27] broom_1.0.7                     tidymodels_1.3.0               
## [29] lubridate_1.9.4                 forcats_1.0.0                  
## [31] stringr_1.5.1                   dplyr_1.1.4                    
## [33] purrr_1.0.4                     readr_2.1.5                    
## [35] tidyr_1.3.1                     tibble_3.2.1                   
## [37] ggplot2_3.5.1                   tidyverse_2.0.0                
## 
## loaded via a namespace (and not attached):
##   [1] rstudioapi_0.17.1   jsonlite_1.9.1      shape_1.4.6.1      
##   [4] datawizard_1.0.1    magrittr_2.0.3      jomo_2.7-6         
##   [7] farver_2.1.2        nloptr_2.2.0        rmarkdown_2.29     
##  [10] vctrs_0.6.5         minqa_1.2.8         sparsevctrs_0.3.0  
##  [13] htmltools_0.5.8.1   plotrix_3.8-4       sjmisc_2.8.10      
##  [16] mitml_0.4-5         sass_0.4.9          parallelly_1.42.0  
##  [19] bslib_0.9.0         plyr_1.8.9          CORElearn_1.57.3.1 
##  [22] cachem_1.1.0        lifecycle_1.0.4     iterators_1.0.14   
##  [25] pkgconfig_2.0.3     sjlabelled_1.2.0    R6_2.6.1           
##  [28] fastmap_1.2.0       rbibutils_2.3       future_1.34.0      
##  [31] digest_0.6.37       colorspace_2.1-1    furrr_0.3.1        
##  [34] ps_1.9.0            ellipse_0.5.0       labeling_0.4.3     
##  [37] timechange_0.3.0    compiler_4.4.2      bit64_4.6.0-1      
##  [40] withr_3.0.2         backports_1.5.0     performance_0.13.0 
##  [43] pan_1.9             MASS_7.3-61         lava_1.8.1         
##  [46] sjstats_0.19.0      tools_4.4.2         future.apply_1.11.3
##  [49] nnet_7.3-19         glue_1.8.0          callr_3.7.6        
##  [52] nlme_3.1-166        grid_4.4.2          cluster_2.1.6      
##  [55] reshape2_1.4.4      generics_0.1.3      gtable_0.3.6       
##  [58] tzdb_0.4.0          class_7.3-22        data.table_1.17.0  
##  [61] hms_1.1.3           utf8_1.2.4          foreach_1.5.2      
##  [64] pillar_1.10.1       vroom_1.6.5         splines_4.4.2      
##  [67] lhs_1.2.0           lattice_0.22-6      sfd_0.1.0          
##  [70] survival_3.7-0      bit_4.6.0           tidyselect_1.2.1   
##  [73] coro_1.1.0          reformulas_0.4.0    safetensors_0.1.2  
##  [76] xfun_0.51           hardhat_1.4.1       timeDate_4041.110  
##  [79] stringi_1.8.4       DiceDesign_1.10     yaml_2.3.10        
##  [82] boot_1.3-31         evaluate_1.0.3      codetools_0.2-20   
##  [85] rpart.plot_3.1.2    cli_3.6.4           rpart_4.1.23       
##  [88] Rdpack_2.6.2        processx_3.8.6      munsell_0.5.1      
##  [91] jquerylib_0.1.4     Rcpp_1.0.14         ROSE_0.0-4         
##  [94] globals_0.16.3      ggeffects_2.2.1     parallel_4.4.2     
##  [97] gower_1.0.2         GPfit_1.0-8         lme4_1.1-36        
## [100] listenv_0.9.1       ipred_0.9-15        prodlim_2024.06.25 
## [103] crayon_1.5.3        insight_1.1.0       rlang_1.1.5        
## [106] mice_3.17.0
```


