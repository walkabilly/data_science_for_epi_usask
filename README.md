---
output:
  html_document: default
  pdf_document: default
---

# CHEP 898: Data Science for Epidemiology

## Course Syllabus

**Code**:	CHEP 898	

**Term**:	2025 Winter

**Delivery**:	In person

**Location**: 	HLTH	3312

**Start Date**:	January 8th, 2025

**Time**:	Wednesdays 9:00- 11:50 am

## Course Description
This course introduces students to the principles of data science as applied to epidemiological research. Emphasis is on data wrangling, version control with Git and GitHub, high-performance computing, and machine learning techniques. It also compares traditional epidemiologic analysis approaches with contemporary machine learning methods.

## Official Syllabus
The official syllabus for this course is available for download [here](https://github.com/walkabilly/data_science_for_epi_usask/blob/main/CHEP898_Data_Science_for_Epi_Syllabus_2025.pdf)

## Land Acknowledgement
I acknowledge our shared connection to the land and recognize that Indigenous and Métis peoples on Treaty 6 Territory and all Indigenous peoples have been and continue to be stewards for social justice, equity, and land-based education. In the spirit of reconciliation may we all strive to learn and support the work of Indigenous communities as allies. 

## Artificial Intelligence 
This course will follow the general USask Guidelines about AI for Educators and Students (https://leadership.usask.ca/initiatives/ai/index.php). The University has developed high level guidance based on the [European Network for Academic Integrity (ENAI) recommendations](https://edintegrity.biomedcentral.com/articles/10.1007/s40979-023-00133-4). The principles are descriptions of USask intentions for, and beliefs about, the use of AI. They include 4 categories:
•	Ethical and Responsible Use
•	Literacy
•	Tool Use
•	Change and Innovation

### AI Rules for this course
In general, my opinion is that you should exploring these tools, what they can do, and how you can integrate them into your work. These tools are great for editing, formatting, generating ideas, and writing very basic code. USask faculty and students have access to Microsoft Co-Pilot (https://teaching.usask.ca/learning-technology/tools/microsoft-copilot.php). It's critical that when you use these tools you are very aware of bias and that you intervene to correct the text. Here are my general rules for AI in this course.

1. You can use AI tools for any or all parts of the work.

2. If you do you must cite your work (as above).
  
    2.1. Acknowledge AI tools: “All persons, sources, and tools that influence the ideas or generate the content should be properly acknowledged” (p. 3). Acknowledgement may be done in different ways, according to context and discipline, and should include the input to the tool.
    
    2.2. Do not list AI tools as authors: Authors must take responsibility and be accountable for content and an AI tool cannot do so.
    
    2.3. Recognize limits and biases of AI tools: Inaccuracies, errors, and bias are reproduced in AI tools in part because of the human produced materials used for training.

3. If you do you must include a 500 word reflective essay about the experience as part of your self-evaluation.

4. Be very careful with reference. Many of these tools just make up random references.

5. I will not use tools like [GPTZero](https://gptzero.me/faq#i-m-an-educator-who-has-f) to detect whether you have used AI tools or not. We are making an agreement to be honest with each other here. This is small class. We have that luxury. 

## Contact Information
Dr. Daniel Fuller
daniel.fuller@usask.ca 

## Learning Outcomes 
1.	Understand the basics of data wrangling and data management in epidemiology.
2.	Gain proficiency in using Git and GitHub for version control.
3.	Learn to leverage high-performance computing resources for epidemiologic data analysis.
4.	Explore various machine learning techniques and their applications in epidemiology.
5.	Compare and contrast traditional epidemiological analysis methods with machine learning approaches.

## Readings/Textbooks

There is not one textbook for this course. We will use various components of different open access resources. 

> R for Data Science (2e). 2024. Hadley Wickham, Mine Çetinkaya-Rundel, and Garrett Grolemund. [https://r4ds.hadley.nz/](https://r4ds.hadley.nz/)

> An Introduction to Statistical Learning with Applications in R (2e). 2024. Gareth James, Daniela Witten, Trevor Hastie, Robert Tibshirani. [https://www.statlearning.com/](https://www.statlearning.com/)

> Learn Tidymodels. [https://www.tidymodels.org/learn/](https://www.tidymodels.org/learn/)

### Other Required Materials

Use of a statistical software program (R) is required for this course. You will also be asked to install other software including PostGRES (SQL) and Git. 

## Dataset

In this course we will use the [CanPath Student Dataset](https://canpath.ca/student-dataset/)  that provides students the unique opportunity to gain hands-on experience working with CanPath data. The CanPath Student Dataset is a synthetic dataset that was manipulated to mimic CanPath’s nationally harmonized data but does not include or reveal actual data of any CanPath participants.

The CanPath Student Dataset is available to instructors at a Canadian university or college for use in an academic course, at no cost. CanPath will provide the Student Dataset and a supporting data dictionary.

* Large sample size (Over 40,000 participants)
* Real-world population-level Canadian data
* Variety of areas of information allowing for a wide range of research topics
* No cost to faculty
* Potential for students to apply for real CanPath data to publish their findings

## General Class Schedule

Week |	Date |	Topic |	Data Work | Assignment Due |
-----|-------|--------|---------- | ------ |
1	| January 8	| Intro to Data Science 	| [Intro R + Data Wrangling](https://github.com/walkabilly/data_science_for_epi_usask/blob/main/Data%20Work/data_wrangling.md) | |
2	| January 15	| R Wrangling and Visualization	| [Data Visualization](https://github.com/walkabilly/data_science_for_epi_usask/blob/main/Data%20Work/data_visualization.md) | |
3	| January 21	| Version Control with Git/Github	| [HappyGitwithR](https://happygitwithr.com/) | Data Wrangling | 
4	| January 29	| Missing Data	| [Missing Data](https://github.com/walkabilly/data_science_for_epi_usask/blob/main/Data%20Work/missing_data.md) | Git/Github | 
5	| February 5	| Linear Regression | [Linear Regression](https://github.com/walkabilly/data_science_for_epi_usask/blob/main/Data%20Work/linear_regression.md) | Missing Data |
6	| February 12	| Logistic Regression | [Logistic Regression](https://github.com/walkabilly/data_science_for_epi_usask/blob/main/Data%20Work/logistic_regression.md)
7	| February 19	| Reading Week	| 
8	| February 26	| Random Forest	| [Random Forest](https://github.com/walkabilly/data_science_for_epi_usask/blob/main/Data%20Work/random_forest.md) | Independent Analysis 1 |
9	| March 5	| Causal Inference | [Causal Quartet](https://github.com/walkabilly/data_science_for_epi_usask/blob/main/Data%20Work/causal_quartet_R.md) | Random Forest |
10	| March 12	| Matching Methods	| [Matching](https://github.com/walkabilly/data_science_for_epi_usask/blob/main/Data%20Work/matching.md) |  |
11	| March 19 | Support Vector Machines | [SVM](https://github.com/walkabilly/data_science_for_epi_usask/blob/main/Data%20Work/svm.md) | Matching |
12	| March 26	| Scientific Computing	| [Scientific Computing/Big Data](https://github.com/walkabilly/data_science_for_epi_usask/blob/main/Data%20Work/scientific_computing.md) + [Full ML Implementation](https://github.com/walkabilly/data_science_for_epi_usask/blob/main/Data%20Work/full_ml_implementation.md) |  |
13	| April 2	| Artificial Neural Networks	| [ANN](https://github.com/walkabilly/data_science_for_epi_usask/blob/main/Data%20Work/ann.md) | Independent Analysis 2 |
* Subject to change depending on speed

### Attendance and Participation
Attendance and participation and reading ahead are critical to this course. There will a lot of time for discussion and working on assignments allocated in this course but reading ahead is a critical aspect of the learning process. 

## Assignment Grading Scheme

You can find the detailed descriptions for all assignments below or in the assignments folder [here](https://github.com/walkabilly/data_science_for_epi_usask/tree/main/Assignments)

Assignment | Grade %
-----------|------
[Data Wrangling and Visualization](https://github.com/walkabilly/data_science_for_epi_usask/blob/main/Assignments/data_wrangling_visualization.md) | 10%
[Github](https://github.com/walkabilly/data_science_for_epi_usask/blob/main/Assignments/git_github.md) |	10%
[Missing Data](https://github.com/walkabilly/data_science_for_epi_usask/blob/main/Assignments/missing_data.md) |	15%
[Independent Analysis](https://github.com/walkabilly/data_science_for_epi_usask/blob/main/Assignments/analysis_part1.md) - Part 1 |	15%
[Random Forest](https://github.com/walkabilly/data_science_for_epi_usask/blob/main/Assignments/random_forest.md) | 15%
[Matching](https://github.com/walkabilly/data_science_for_epi_usask/blob/main/Assignments/matching.md)	| 15%
[Independent Analysis](https://github.com/walkabilly/data_science_for_epi_usask/blob/main/Assignments/analysis_part2.md) – Part 2	| 20%
Total	| 100%

## Assignment Descriptions

### Data Wrangling and Visualization   
Value: 		10% of final grade  
Description: In this assignment you will complete a data wrangling assignment that will involve data cleaning, descriptive statistics, understanding missing data, and joining datasets together. 

### Github	
Value: 		10% of final grade  
Description: In this assignment you will create a Github account, install Git on your local computer, create a Github repository and commit and push your work to that Github repository. 

### Missing Data	
Value: 		15% of final grade  
Description: In this assignment you will apply and compare different methods for imputing missing data on large health administrative dataset. 

### Independent Analysis 1
Value: 		15% of final grade  
Description: This is part 1 of the independent analysis. You will need to find a dataset, develop an analysis plan to includes the major components of the course (ie., Github, Scientific Computing), and conduct descriptive statistics and data wrangling on your chosen dataset. 

### Random Forest 
Value: 		10% of final grade  
Description: In this analysis you will complete an Random Forest analysis using the Can Path student dataset. You will need to run the analysis, conduct detailed hyperparameter tuning, and conduct model comparisons.

### Matching 
Value: 		15% of final grade  
Description: In this analysis you will complete an machine learning based matching analysis using the Can Path student dataset.

### Independent Analysis 
Value: 		20% of final grade  
Description: This is part 2 (final part) of the independent analysis. You will need to conduct a complete analysis including data wrangling, missing data handling, and apply at least 2 different machine learning methods to your data. 

### Self-Evaluation
Type: 	Written report
Description: 	Complete the student self-evaluation form. 

## Submitting Assignments 
All assignments should be submitted to the appropriate place in Canvas or Github. All assignments are due at 5pm (CST) on the due date. Please don’t stay up until midnight to get the work done. Remember there are no late penalties so just take an extra day if you need and get some sleep. 

## Late and Missing Assignments
There is no penalty for late assignments. However, because many assignments have two parts, it is critical to the first assignment of the sections in around the due date. Missing assignments that are not submitted by the end of the course will receive a grade of zero. 

