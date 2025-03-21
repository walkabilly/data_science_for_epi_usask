---
title: "Assignment: Random Forest Analysis"
output:
  html_document:
    keep_md: yes
  pdf_document: default
---

**Value:** 10% of Final Grade  

---

#### **Description**  
This assignment focuses on developing your understanding and application of Random Forest algorithms. You will work with the **Can Path Student Dataset** to perform a Random Forest analysis, conduct detailed hyperparameter tuning, and compare the performance of your model with other models or configurations. This exercise emphasizes building robust models and evaluating their performance critically.

---

#### **Assignment Objectives**  
By completing this assignment, you will:  
1. Develop proficiency in implementing Random Forest algorithms for predictive analysis.  
2. Understand the importance of hyperparameter tuning and its impact on model performance.  
3. Gain experience in comparing models to select the best-performing configuration.  
4. Learn to interpret Random Forest results, including feature importance.  

---

#### **Assignment Tasks**  

1. **Dataset Exploration**  
   - Load and examine the **Can Path Student Dataset**.  
   - Conduct an initial exploratory data analysis (EDA) to understand key features and relationships.  
   - Handle missing data, outliers, or inconsistencies if necessary.  

2. **Baseline Random Forest Model**  
   - Split the dataset into training and testing sets.  
   - Implement a baseline Random Forest model with default parameters.  
   - Evaluate the model using appropriate metrics (e.g., accuracy, precision, recall, F1-score, or ROC-AUC).  

3. **Hyperparameter Tuning**  
   - Identify key hyperparameters to tune.  
   - Use techniques to find the optimal hyperparameter configuration.  
   - Document the tuning process, including parameters tested and evaluation results.  

4. **Model Comparisons**  
   - Compare the tuned Random Forest model against:  
     - The baseline Random Forest model.  
   - Analyze differences in performance using quantitative metrics and discuss possible reasons for the results.  

5. **Feature Importance Analysis**  
   - Extract and interpret feature importance scores from the Random Forest model.  
   - Visualize the top contributing features and discuss their relevance to the dataset.  

---

#### **Deliverables**  

Submit a structured report in .Rmd format and upload to your personal Github page. I will be running your .Rmd file so please make sure your submission is executable and replicable. Remember to document your process thoroughly as with descriptions in the text of the .Rmd file. 

1. **Analysis Report**  
     - A summary of the dataset and EDA results.  
     - Steps and rationale for building the baseline and tuned models.  
     - Comparisons between models with performance metrics and observations.  
     - Interpretation of feature importance results.  

2. **Code and Documentation**  
   - Submit well-documented code, organized into scripts or notebooks.  
   - Ensure reproducibility by providing clear instructions and comments.  

3. **Visualizations**  
   - Include at least two visualizations to support your analysis (e.g., feature importance plots, ROC curves, confusion matrix).  

---

#### **Assessment Criteria**  
1. **Dataset Preparation and EDA (15%)**  
   - Thoroughness of exploration and preprocessing steps.  

2. **Baseline Model Implementation (15%)**  
   - Correctness and clarity in building and evaluating the baseline Random Forest model.  

3. **Hyperparameter Tuning (25%)**  
   - Rigor in tuning process and documentation of results.  

4. **Model Comparisons (25%)**  
   - Thoughtfulness in model selection and depth of analysis in comparing performance.  

5. **Feature Importance Analysis (10%)**  
   - Quality of interpretation and presentation of feature importance.  

6. **Documentation and Presentation (10%)**  
   - Clarity, organization, and professionalism in the report and code.  
