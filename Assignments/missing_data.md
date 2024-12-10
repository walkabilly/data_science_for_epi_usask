---
title: "Assignment: Missing Data Analysis"
output:
  html_document:
    keep_md: yes
  pdf_document: default
---

**Value:** 15% of Final Grade  

---

#### **Description**  
In this assignment, you will address missing data issues in a large health administrative dataset. Missing data is a common challenge in health research, and choosing the right imputation method is crucial for accurate analysis. You will explore, apply, and compare multiple methods for imputing missing data, evaluating their effectiveness and impact on the dataset. The goal is to demonstrate an understanding of various imputation techniques and their implications for analysis.

---

#### **Assignment Objectives**  

By completing this assignment, you will

1. Understand the nature and patterns of missing data in a large dataset.  
2. Apply different imputation methods and assess their suitability for the dataset.  
3. Compare the results of different imputation techniques using statistical and visual methods.  
4. Develop insights into the impact of missing data handling on subsequent analyses.  

---

#### **Assignment Tasks**  

1. **Dataset Exploration**  
   - Load the provided health administrative dataset.  
   - Explore the dataset to identify the extent, patterns, and potential reasons for missing data.  
   - Summarize findings using tables, charts, or heatmaps to visualize missingness.  

2. **Apply Imputation Methods**  
   - Select and apply at least three different methods for imputing missing data, such as:  
     - Mean/Median/Mode imputation.  
     - K-Nearest Neighbors (KNN) imputation.  
     - Multiple Imputation by Chained Equations (MICE).  
   - Document the implementation process for each method.  

3. **Evaluation of Imputation Methods**  
   - Compare the imputed datasets by analyzing:  
     - Changes in key summary statistics (e.g., means, variances).  
     - The preservation of relationships between variables (e.g., correlations).  
     - Visual comparisons of distributions before and after imputation.  

4. **Analysis of Imputed Data**  
   - Conduct a simple statistical analysis on the imputed datasets to illustrate the downstream effects of different imputation methods.  
   - Discuss how the choice of imputation method impacts the analysis results.  

5. **Interpretation and Reporting**  
   - Provide a detailed comparison of the methods, discussing their strengths, weaknesses, and suitability for the dataset.  
   - Reflect on the challenges of handling missing data in health research.  

---

#### **Deliverables**  

Submit a structured report in .Rmd format and upload to your personal Github page in a new repo. I will be running your .Rmd file so please make sure your submission is executable and replicable. Remember to document your process thoroughly as with descriptions in the text of the .Rmd file. 

1. **Analysis Report**  
   - A structured report that includes:  
     - Description and visualization of missing data patterns.  
     - Explanation and implementation of the chosen imputation methods.  
     - Comparison of results from different methods.  
     - Insights from the analysis of imputed datasets.  
     - Discussion on the implications of imputation for health data research.  

2. **Code and Documentation**  
   - Submit all scripts or notebooks used for the analysis.  
   - Ensure the code is well-documented with comments explaining each step.  

3. **Visualizations**  
   - Include visualizations such as heatmaps of missingness, distribution comparisons, and analysis results to support your findings.  
   
---

#### **Assessment Criteria**  

1. **Exploration and Understanding of Missing Data (20%)**  
   - Thoroughness and clarity in identifying and visualizing missing data patterns.  

2. **Application of Imputation Methods (25%)**  
   - Correctness and appropriateness of the implemented methods.  

3. **Evaluation and Comparison (25%)**  
   - Depth of analysis in comparing the imputation methods and their impact on the data.  

4. **Analysis of Imputed Data (20%)**  
   - Quality of the downstream analysis and interpretation of results.  

5. **Documentation and Presentation (10%)**  
   - Organization and professionalism in the report, code, and visualizations.  
