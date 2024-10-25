---
output:
  pdf_document: default
  html_document: default
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
1	| January 8	| Intro to Data Science 	| [Intro R]() | |
2	| January 15	| R Wrangling and Visualization	| [Data Wrangling]() | |
3	| January 21	| Version Control with Git/Github	| [Data Visualization]() | Data Wrangling | 
4	| January 29	| Missing Data	| [Missing Data]() | Version Control | 
5	| February 5	| Linear Regression | [Linear Regression]() | Missing Data |
6	| February 12	| Logistic Regression | [Logistic Regression]()
7	| February 19	| Reading Week	| 
8	| February 26	| Scientific Computing	| [Scientific Computing/Big Data]() | Independent Analysis 1 |
9	| March 5	| Causal Inference | [Causal Quartet]() | Scientific Computing |
10	| March 12 | Support Vector Machines | [Random Forest]() |  |
11	| March 19	| Random Forest	| [Matching]() | Random Forest |
12	| March 26	| Matching Methods	| [SVM]() | Matching |
13	| April 2	| Artificial Neural Networks	| [ANN]() | Independent Analysis 2 |
* Subject to change depending on speed

### Attendance and Participation
Attendance and participation and reading ahead are critical to this course. There will a lot of time for discussion and working on assignments allocated in this course but reading ahead is a critical aspect of the learning process. 

## Assignment Grading Scheme

Assignment | Grade %
-----------|------
[Data Wrangling and Visualization]() | 10%
[Github]() |	10%
[Missing Data]() |	15%
[Independent Analysis]() - Part 1 |	10%
[Random Forest]() | 15%
[Scientific Computing/Big Data]() | 10%
[Matching]()	| 15%
[Independent Analysis]() – Part 2	| 15%
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
Value: 		10% of final grade  
Description: This is part 1 of the independent analysis. You will need to find a dataset, develop an analysis plan to includes the major components of the course (ie., Github, Scientific Computing), and conduct descriptive statistics and data wrangling on your chosen dataset. 

### Random Forest 
Value: 		10% of final grade  
Description: In this analysis you will complete an Random Forest analysis using the Can Path student dataset. You will need to run the analysis, conduct detailed hyperparameter tuning, and conduct model comparisons.

### Scientific Computing/Big Data 
Value: 		10% of final grade  
Description: In this assignment you will use the [USask Plato High Performance Computing](https://wiki.usask.ca/display/ARC/Plato+HPC+Cluster) to run a large scale machine learning on a large (~1GB) dataset. 

### Matching 15%
Value: 		15% of final grade  
Description: In this analysis you will complete an machine learning based matching analysis using the Can Path student dataset.

### Independent Analysis 15%
Value: 		15% of final grade  
Description: This is part 2 (final part) of the independent analysis. You will need to conduct a complete analysis including data wrangling, missing data handling, and apply at least 2 different machine learning methods to your data. 

### Self-Evaluation
Type: 	Written report
Description: 	Complete the student self-evaluation form. 

## Submitting Assignments 
All assignments should be submitted to the appropriate place in Canvas. All assignments are due at 5pm (CST) on the due date. Please don’t stay up until midnight to get the work done. Remember there are no late penalties so just take an extra day if you need and get some sleep. 

## Late and Missing Assignments
There is no penalty for late assignments. However, because many assignments have two parts, it is critical to the first assignment of the sections in around the due date. Missing assignments that are not submitted by the end of the course will receive a grade of zero. 

## Readings


## Access and Equity Services (AES) 
Access and Equity Services (AES) is available to provide support to students who require accommodations due to disability, family status, and religious observances. 
Students who have disabilities (learning, medical, physical, or mental health) are strongly encouraged to register with Access and Equity Services (AES) if they have not already done so. Students who suspect they may have disabilities should contact AES for advice and referrals at any time. Those students who are registered with AES with mental health disabilities and who anticipate that they may have responses to certain course materials or topics, should discuss course content with their instructors prior to course add / drop dates. 
Students who require accommodations for pregnancy or substantial parental/family duties should contact AES to discuss their situations and potentially register with that office. 
Students who require accommodations due to religious practices that prohibit the writing of exams on religious holidays should contact AES to self-declare and determine which accommodations are appropriate. In general, students who are unable to write an exam due to a religious conflict do not register with AES but instead submit an exam conflict form through their PAWS account to arrange accommodations. 
Any student registered with AES, as well as those who require accommodations on religious grounds, may request alternative arrangements for mid-term and final examinations by submitting a request to AES by the stated deadliness. Instructors shall provide the examinations for students who are being accommodated by the deadlines established by AES.  
 
For more information or advice, visit https://students.usask.ca/health/centres/access-equity-services.php, or contact AES at 306-966-7273 (Voice/TTY 1-306-966-7276) or email aes@usask.ca. 

## Academic Integrity
The University of Saskatchewan is committed to the highest standards of academic integrity and honesty. Students are expected to be familiar with these standards regarding academic honesty and to uphold the policies of the University in this respect. Students are particularly urged to familiarize themselves with the provisions of the Student Conduct & Appeals section of the University Secretary Website and avoid any behavior that could potentially result in suspicions of cheating, plagiarism, misrepresentation of facts and/or participation in an offence. Academic dishonesty is a serious offence and can result in suspension or expulsion from the University. 
All students should read and be familiar with the Regulations on Academic Student Misconduct (https://governance.usask.ca/student-conduct-appeals/academic-misconduct.php) as well as the Standard of Student Conduct in Non-Academic Matters and Procedures for Resolution of Complaints and Appeals (https://governance.usask.ca/student-conduct-appeals/non-academic-misconduct.php) 
For more information on what academic integrity means for students see the Academic Integrity section of the University Library Website at: https://library.usask.ca/academic-integrity.php
You are encouraged to complete the Academic Integrity Tutorial to understand the fundamental values of academic integrity and how to be a responsible scholar and member of the USask community - https://libguides.usask.ca/AcademicIntegrityTutorial
There are also valuable resources on the Integrity Matters website: https://academic-integrity.usask.ca/

# Copyright
Course materials are provided to you based on your registration in a class, and anything created by your professors and instructors is their intellectual property and cannot be shared without written permission. If materials are designated as open education resources (with a creative commons license) you can share and/or use in alignment with the CC license. This includes exams, PowerPoint/PDF slides and other course notes. Additionally, other copyright-protected materials created by textbook publishers and authors may be provided to you based on license terms and educational exceptions in the Canadian Copyright Act (see http://laws-lois.justice.gc.ca/eng/acts/C-42/index.html).
Before you copy or distribute others’ copyright-protected materials, please ensure that your use of the materials is covered under the University’s “Use of Materials Protected By Copyright” Policy available at https://policies.usask.ca/policies/operations-and-general-administration/copyright.php. For example, posting others’ copyright-protected materials on the open internet is not permitted by this policy or by the university Copyright Guidelines (available at https://library.usask.ca/copyright/general-information/copyright-guidelines.php) and requires permission from the copyright holder
For more information about copyright, please visit https://library.usask.ca/copyright/ where there is information for students available at https://library.usask.ca/copyright/students/your-course-materials.php, or contact the University’s Copyright Coordinator at copyright.coordinator@usask.ca or 306-966-8817. 

## Student Supports
### Academic Support for Students  
Visit the Learning Hub to learn how the University Library supports undergraduate and graduate students. Attend online or in-person workshops, review online resources or book 1-1 appointments for help with
* First year experience
* Research
* Study strategies and skills
* Writing
* Math and Statistics

## Teaching, Learning and Student Experience 
Teaching, Learning and Student Experience (TLSE) provides developmental and support services and programs to students and the university community. For more information, see the students’ website http://students.usask.ca. 

## Financial Support 
Any student who faces unexpected challenges securing their food or housing and believes this may affect their performance in the course is urged to contact Student Central https://students.usask.ca/student-central.php.

## Aboriginal Students’ Centre
The Aboriginal Students’ Centre (ASC) is dedicated to supporting Indigenous student academic and personal success. The ASC offers personal, social, cultural and some academic supports to Métis, First Nations, and Inuit students. The ASC is in the Gordon Oakes Red Bear Students Centre, which is an intercultural gathering space that brings Indigenous and non-Indigenous students together to learn from, with and about one another in a respectful, inclusive, and safe environment. Visit https://students.usask.ca/indigenous/index.php or students are encouraged to visit the ASC’s Facebook page https://www.facebook.com/aboriginalstudentscentre/ 

##  International Student and Study Abroad Centre
The International Student and Study Abroad Centre (ISSAC) supports student success and facilitates international education experiences at USask and abroad. ISSAC is here to assist all international undergraduate, graduate, exchange, and English as a Second Language students in their transition to the University of Saskatchewan and to life in Canada. ISSAC offers advising and support on matters that affect international students and their families and on matters related to studying abroad as University of Saskatchewan students. Visit https://students.usask.ca/international/issac.php for more information.  


