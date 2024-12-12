---
title: "Data Wrangling"
output:
      html_document:
        keep_md: true
---


``` r
library(rstatix)
library(tidyverse)
library(pastecs)
library(knitr)
library(epitools)
library(Epi)
options(scipen=999) 
```



``` r
data <- read_csv("can_path_data.csv")
```

```
## Warning: One or more parsing issues, call `problems()` on your data frame for details,
## e.g.:
##   dat <- vroom(...)
##   problems(dat)
```

```
## Rows: 41187 Columns: 440
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr   (5): ID, MSD11_PR, MSD11_REG, MSD11_ZONE, MSD11_CMA
## dbl (425): ADM_STUDY_ID, SDC_GENDER, SDC_AGE_CALC, SDC_MARITAL_STATUS, SDC_E...
## lgl  (10): DIS_MH_BIPOLAR_EVER, DIS_GEN_DS_EVER, DIS_GEN_SCA_EVER, DIS_GEN_T...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

## General instructions

### 1. Identify the variable types for each variable in the dataset


``` r
glimpse(data)
```

All of the variables here currently coded as _dbl_ except `ID` which is _chr_. That's incorrect and we are going to need to fix that when we recode.


``` r
### Select specific columns
cols <- c("SDC_EDU_LEVEL", "HS_GEN_HEALTH", "HS_ROUTINE_VISIT_EVER", "HS_ROUTINE_VISIT_LAST", "HS_DENTAL_VISIT_EVER", "HS_DENTAL_VISIT_LAST")
data <- data %>% mutate_at(cols, factor)
```


``` r
glimpse(data)
```


``` r
### Select by column number
data <- data %>% mutate_at(10:15, factor)
```


``` r
glimpse(data)
```

### 3. Recode and label categorical variables as necessary


``` r
data <- data %>%
	mutate(gender_recode = case_when(
		SDC_GENDER == 1 ~ "Male",
    SDC_GENDER == 2 ~ "Female"
	))

data$gender_recode <- as.factor(data$gender_recode) 

### Checking our code to make sure our recode is correct
table(data$SDC_GENDER, data$gender_recode)
```

```
##    
##     Female  Male
##   1      0 15200
##   2  25987     0
```

### Add the `NUT_VEG_QTY` and the `NUT_FRUITS_QTY` to create one variable
    * Create a categorical variable that represents the recommended fruit and vegetable consumption per day


``` r
summary(data$NUT_VEG_QTY)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.000   2.000   2.000   2.672   3.000  35.000    2549
```

``` r
summary(data$NUT_FRUITS_QTY)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.000   1.000   2.000   2.132   3.000  25.000    2426
```

``` r
### No missing data the no weird codings for the numbers. Great! 

data <- data %>%
	mutate(fruit_veg_tot = NUT_VEG_QTY + NUT_FRUITS_QTY)

summary(data$fruit_veg_tot)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.000   3.000   4.000   4.816   6.000  55.000    2908
```

``` r
data <- data %>%
	mutate(fruit_veg_cat = case_when(
		fruit_veg_tot <= 7  ~ "Not Meeting Guidelines",
    fruit_veg_tot > 7 ~ "Meeting Guidelines"
	))

table(data$fruit_veg_cat)
```

```
## 
##     Meeting Guidelines Not Meeting Guidelines 
##                   5237                  33042
```

``` r
data <- data %>%
	mutate(fruit_veg_dic = case_when(
		fruit_veg_tot <= 7 ~ 0,
    fruit_veg_tot > 7 ~ 1
	))

table(data$fruit_veg_tot, data$fruit_veg_cat)
```

```
##     
##      Meeting Guidelines Not Meeting Guidelines
##   0                   0                    404
##   1                   0                   1561
##   2                   0                   5015
##   3                   0                   5807
##   4                   0                   6720
##   5                   0                   5525
##   6                   0                   4955
##   7                   0                   3055
##   8                2200                      0
##   9                1213                      0
##   10                816                      0
##   11                433                      0
##   12                226                      0
##   13                125                      0
##   14                 87                      0
##   15                 29                      0
##   16                 45                      0
##   17                 13                      0
##   18                 12                      0
##   19                  4                      0
##   20                  7                      0
##   21                  1                      0
##   22                  2                      0
##   24                  1                      0
##   25                  1                      0
##   26                  1                      0
##   27                  2                      0
##   28                  4                      0
##   30                  2                      0
##   31                  1                      0
##   32                  3                      0
##   34                  1                      0
##   35                  1                      0
##   44                  1                      0
##   45                  1                      0
##   49                  2                      0
##   50                  1                      0
##   51                  1                      0
##   55                  1                      0
```

### Create a categorical variable for the `PA_TOTAL_SHORT` variable the defines low, moderate, and high activity per week
    * Low - < 600 MET minutes per week
    * Moderate - >=600 to <3000 MET minutes per week
    * High - >=3000 MET minutes per week

### Calculate the mean and standard deviation
    * For the variables were it is appropriate
    * Including the new variables you have created
    

``` r
summary(data$PA_TOTAL_SHORT)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##       0     600    1782    2574    3732   19278    6763
```

``` r
data <- data %>%
	mutate(pa_cat = case_when(
		PA_TOTAL_SHORT < 600  ~ "Low Activity",
		PA_TOTAL_SHORT >= 3000 ~ "High Activity",
		PA_TOTAL_SHORT >= 600 ~ "Moderate Activity"
	))

### Flag we need to put the low and how activity first otherwise we don't get the right answer
```

### Calculate the percents and frequencies 
    * For the variables were it is appropriate to do so

There are __MANY__ different ways to do this in R. There is no one best way. 


``` r
## First we need to make sure we get the variables recoded and organized

### Example with gender where we already recoded
gender_table <- bind_rows(table(data$gender_recode), prop.table(table(data$gender_recode)))

### Example with fruit_veg_cat where we already recoded
fruit_veg_table <- bind_rows(table(data$fruit_veg_cat), prop.table(table(data$fruit_veg_cat)))
```

### Are there missing data?


``` r
summary(data$fruit_veg_tot)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.000   3.000   4.000   4.816   6.000  55.000    2908
```

### Create a 2*2 table for fruit and vegetable consumption by gender
    * Interpret the 2*2 table result


``` r
gender_fv_table <- table(data$gender_recode, data$fruit_veg_cat)
gender_fv_table
```

```
##         
##          Meeting Guidelines Not Meeting Guidelines
##   Female               4004                  20213
##   Male                 1233                  12829
```

## Joining datasets

### Stacking 2 datasets on top of each other

If we want to stack two datasets we need to use the `bind` set of functions. We can either use `bind_rows` or `bind_cols` from the `tidyverse` set of packages to do this. Let's say we have two datasets with the same information but different participants. We want to stack them so we can do some bigger analysis. 


``` r
data1 <- read_csv("data1.csv")
```

```
## Rows: 200 Columns: 11
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## dbl (11): id, sex, ethgrp, weight, age, cvd, stroke, smoking, Cancer, ldl1, ...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
data2 <- read_csv("data2.csv")
```

```
## Rows: 200 Columns: 11
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## dbl (11): id, sex, ethgrp, weight, age, cvd, stroke, smoking, Cancer, ldl1, ...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

Data1 and Data2 have the some number and information in the columns but they are not together. We can join them using `bind_rows`. 


``` r
data_1_2 <- bind_rows(data1, data2)
```

2. Join by ID

If you have 2 datasets with different data that you want to join you can use different join methods. These terms are from computer science. `full_join`, `left_join`, `right_join`, etc. 


``` r
names_data <- read_csv("names.csv")
```

```
## Rows: 800 Columns: 2
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (1): parent
## dbl (1): id
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
glimpse(names_data)
```

```
## Rows: 800
## Columns: 2
## $ id     <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, …
## $ parent <chr> "Cesar", "Brandon", "Molly", "Mathew", "Gabrielle", "Chloe", "H…
```

``` r
data_all <- left_join(data_1_2, names_data, by = join_by(id))
table(data_all$parent)
```

```
## 
##        Aasim Abdul Baasid        Adjoa       Adrena       Afraah       Alanna 
##            1            1            1            1            2            1 
##      Alberto         Alec    Alexander    Alexandra   Alexandria       Alexis 
##            1            1            1            1            1            2 
##      Alfonso      Aliceia       Alisha       Almira       Alyssa        Amaya 
##            1            1            2            1            1            1 
##        Amber       Ammaar          Ana     Anamaria       Andrew        Angel 
##            1            1            2            1            1            1 
##     Angelica     Angelina         Anna        Anwar       Arissa      Asashia 
##            3            2            1            1            1            1 
##     Ashleigh       Ashley        Asmaa       Audrey       Austin       Baaqir 
##            1            2            1            1            1            1 
##        Badri   Badruddeen       Baheej       Baosan      Bethany       Bobbie 
##            1            1            1            1            1            1 
##       Brandi      Brandon      Braxton       Breana      Britney     Brittany 
##            1            5            1            1            1            4 
##       Brooks        Bryce       Caigen       Cannon       Carlos      Carmela 
##            1            1            1            1            1            1 
##       Carson     Casandra    Catherine    Celistina        Cesar      Chaerin 
##            1            1            1            1            2            1 
##       Chance     Charlene      Charles    Charlotte        Chase     Cheyenne 
##            1            1            2            1            1            1 
##        Chloe    Christian  Christopher       Colton       Crysta      Cynthia 
##            1            1            1            1            1            1 
##       Dakota       Damian       Damien        Damon       Daniel     Danielle 
##            1            2            1            1            2            1 
##        David   Dayveontay       Deanna    Demetrius        Devin     Doan Anh 
##            2            1            1            1            1            1 
##      Dominic    Dominique       Dontae        Edgar       Edrick       Elijah 
##            1            1            1            1            1            1 
##    Elizabeth       Emilio        Emily         Emma         Eric    Estefania 
##            1            1            1            1            1            1 
##       Estela       Eun-Ji         Evan      Faarooq       Faatin        Fawzi 
##            1            1            1            1            1            1 
##       Faydra   Fransiscus    Frederick    Friedrich    Gabriella    Gabrielle 
##            1            1            1            1            1            1 
##       Garett      Gavriel    Genevieve     Gilberto       Gracie        Grant 
##            1            1            1            1            1            1 
##    Guadalupe    Guillermo       Hailey        Haley        Hamdi       Hannah 
##            2            1            1            1            1            2 
##      Harmony       Harold        Hasan         Hawe       Haylie      Heather 
##            1            1            1            1            1            1 
##       Heddie         Hope       Hunter     Ignatius        Ihaab       Ilhaam 
##            1            1            1            1            1            1 
##        Imani        Isaac       Iyonna        Izzat         Jack      Jackson 
##            1            1            1            1            1            2 
##        Jacob         Jade      Jaelynn        Jaime        Jalen        Jamal 
##            2            1            1            1            1            1 
##        James        Janae      Janelle       Jaquay        Jared        Jenny 
##            1            1            1            1            1            1 
##       Jeremy      Jessica         John       Jordan       Joshua     Jourdayn 
##            1            2            2            4            3            1 
##      Juanita        Julia     Julianna       Junior       Justen      Justice 
##            1            1            2            1            1            1 
##        Kacey      Kaitlyn       Kamrin        Karen         Kari        Karin 
##            1            1            1            2            1            1 
##    Kassandra       Kateri    Katherine      Kathryn      Katrina      Kayanna 
##            1            1            1            1            1            1 
##        Kayla        Kelly Kelton David          Ken      Kennedy        Kenny 
##            1            2            1            1            1            1 
##        Kevin         Khoa       Kiante        Kiara      Kirsten   Kristopher 
##            1            1            1            1            1            1 
##         Kyle         Kyra        Ladan        Lance       Lauren         Leon 
##            2            1            1            1            1            1 
##       Leslie          Lia      Lindsey        Logan         Lois        Lucas 
##            1            1            1            2            1            1 
##        Lucky        Lukas     Lutfiyya    Mackenzie      Madalyn      Madison 
##            1            1            2            1            1            1 
##      Mahalia         Maia       Manaal        Maria       Mariah       Marina 
##            1            1            1            2            1            1 
##       Marisa       Martin      Marwaan        Mason      Mastoor       Mathew 
##            1            1            1            1            1            1 
##      Matthew      Maurice         Maya     Mckenzie      Mechale     Michelle 
##            3            1            4            1            1            1 
##       Miguel      Mikayla       Milton       Minnah      Mohamed        Molly 
##            1            1            1            1            1            1 
##      Monique       Morgan       Muslim      Mustaba       Mystic      Na'Inoa 
##            1            2            1            1            1            1 
##      Nafeesa      Najiyya       Nariah     Nashelle       Nathan       Nawaar 
##            1            1            1            1            1            1 
##     Nawwaara        Nbyee     Nicholas        Niomi         Omar      Paisley 
##            1            1            2            1            1            1 
##       Pamela        Pearl      Raabiya         Rabi       Rachel     Rachelee 
##            1            1            1            1            1            1 
##      Raihaan       Ramesh     Randilyn       Rashad      Rashele         Raul 
##            1            1            1            1            1            1 
##       Razeen      Rebecca       Reilly      Ricardo      Richard       Rickia 
##            1            1            1            1            1            1 
##       Robert      Roselyn       Roshae       Rushdi      Rutaiba         Ryan 
##            1            1            1            1            1            1 
##      Saabira      Saarang        Saeed         Saje     Samantha      Sameeha 
##            1            1            1            1            1            1 
##       Samuel       Sandra         Sara        Sarah     Savannah       Shaafi 
##            1            2            2            3            1            1 
##     Shakeela        Shane      Shannon       Shayla       Shelby     Shontice 
##            1            1            1            1            1            1 
##      Shuraih      Si Hien       Sierra       Smokey       Stacie     Stefanie 
##            1            1            1            1            1            1 
##      Stephon     Stephvon       Steven       Stevie       Sultan      Sumayya 
##            1            1            1            1            1            1 
##       Sydney        Tahma      Tamanna     Tausolia      Tawfeeq       Taylor 
##            2            1            1            1            1            3 
##       Tayyib     Terrance     Theodore       Tiesha      Timothy        Torey 
##            1            1            1            1            1            1 
##         Trae          Tre         Trey         Tuan        Turfa           Ty 
##            1            1            1            1            1            1 
##       Tyesha        Tyler        Tyrel        Tyrin       Ubaida      Ulysses 
##            1            4            1            1            1            1 
##       Usaama       Vanesa      Vanessa     Victoria        Vilok      Vincent 
##            1            1            3            1            1            1 
##       Waatiq      Waheeda        Wendy       Wesley         Will       Xavier 
##            1            1            1            1            1            1 
##       Yadira      Zachary      Zakkary      Zayyaan          Zoe 
##            2            1            1            2            1
```

## Data challenge

We can also do more advanced things like select and bind all of the files in a folder with a specific filename or file extension. 

```{}
paths <- list.files(pattern = "data.*\\.csv", full.names = TRUE)
paths

files <- map(paths, read_csv)
length(files)

data_all <- list_rbind(files)
#write_csv(data_all, "data_all.csv")
```


