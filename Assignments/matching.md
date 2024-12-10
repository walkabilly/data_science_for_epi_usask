---
title: "Assignment: Machine Learning-Based Matching Analysis"
output:
  html_document:
    keep_md: yes
  pdf_document: default
---

**Value:** 15% of Final Grade  

---

#### **Description**  
In this assignment, you will perform a machine learning-based matching analysis using the **Can Path Student Dataset**. Matching is a technique often used to balance datasets or control for confounding variables, especially in causal inference. This assignment will require you to apply a machine learning approach to match cases and controls (or equivalent groups), analyze the quality of the matches, and interpret the results.

---

#### **Assignment Objectives**  
By completing this assignment, you will:  
1. Learn to apply machine learning algorithms for matching analysis.  
2. Develop skills in preprocessing and feature engineering for matching.  
3. Assess the quality of matches using statistical and visual methods.  
4. Interpret and report findings from a matching analysis.  

---

#### **Assignment Tasks**  

1. **Data Exploration and Preprocessing**  
   - Load and explore the **Can Path Student Dataset**.  
   - Identify key variables for matching, ensuring they are relevant to your analysis goals.  
   - Perform preprocessing, including scaling, encoding, or imputing missing values, to prepare data for the matching algorithm.  

2. **Machine Learning-Based Matching**  
   - Apply the Propensity Score Matching (PSM) method.  
   - Train the model to predict group membership based on the selected features.  
   - Use the model’s predictions or distance measures to match cases and controls (or equivalent groups).  

3. **Match Quality Assessment**  
   - Evaluate the quality of the matches using metrics such as standardized mean differences, balance statistics, or graphical methods.  
   - Document improvements in balance between groups after matching.  

4. **Analysis of Matched Data**  
   - Conduct an exploratory or inferential analysis on the matched dataset.  
   - Compare results between the unmatched and matched datasets to highlight the impact of the matching process.  

5. **Interpretation and Reporting**  
   - Summarize the steps and results of your matching analysis.  
   - Discuss the implications of your findings and any limitations of the matching approach.  

---

#### **Deliverables**  

Submit a structured report in .Rmd format and upload to your personal Github page in a new repo. I will be running your .Rmd file so please make sure your submission is executable and replicable. Remember to document your process thoroughly as with descriptions in the text of the .Rmd file. 

1. **Analysis Report**  
   - A structured report that includes:  
     - A description of the dataset and variables selected for matching.  
     - Evaluation of match quality and analysis results on the matched dataset.  
     - Interpretation of findings and a discussion of limitations.  

2. **Code and Documentation**  
   - Submit all scripts or notebooks used for the analysis.  
   - Ensure the code is well-documented, with comments explaining each step.  

3. **Visualizations**  
   - Include visual representations of match quality and analysis results.  

---

#### **Assessment Criteria**  

1. **Dataset Preparation (15%)**  
   - Completeness and accuracy in preprocessing the data for matching.  

2. **Matching Implementation (30%)**  
   - Correctness and rigor in applying the machine learning-based matching technique.  

3. **Match Quality Assessment (20%)**  
   - Thoroughness in evaluating the quality of matches and achieving balance.  

4. **Analysis of Matched Data (20%)**  
   - Depth and clarity of insights derived from the matched dataset.  

5. **Documentation and Presentation (15%)**  
   - Quality of the report, code, and visualizations, with clear and professional formatting.  
