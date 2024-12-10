---
title: "Assignment: Scientific Computing and Big Data Analysis"
output:
  html_document:
    keep_md: yes
  pdf_document: default
---

**Value:** 10% of Final Grade  

---

#### **Description**  
This assignment focuses on leveraging high-performance computing (HPC) resources to perform large-scale machine learning. Using the **USask Plato High Performance Computing (HPC) Cluster**, you will analyze a large dataset (approximately 1GB) to build, train, and evaluate a machine learning model. This exercise emphasizes scaling data processing and computation for big data workflows.

---

#### **Assignment Objectives**  
By completing this assignment, you will:  
1. Gain practical experience using an HPC cluster for computationally intensive tasks.  
2. Learn how to handle large datasets effectively in a distributed computing environment.  
3. Implement and evaluate a machine learning model on a large dataset.  
4. Understand the principles of resource management and optimization in HPC.  

---

#### **Assignment Tasks**  

1. **Accessing the Plato HPC Cluster**  
   - Set up access to the Plato HPC Cluster using the provided guidelines.  
   - Familiarize yourself with the cluster’s environment, including job submission and file management.  

2. **Dataset Preparation**  
   - Load and explore the provided dataset.  
   - Preprocess the data as necessary, ensuring compatibility with the machine learning model.  
   - Document any challenges or adjustments made due to the dataset's size.  

3. **Machine Learning Model**  
   - Implement the a random forest model with libraries compatible with HPC environments.

4. **HPC Job Submission**  
   - Write and execute a job script to run your machine learning model on the Plato HPC Cluster.  
   - Optimize resource requests (e.g., CPU, GPU, memory) to ensure efficient use of the cluster.  

5. **Model Evaluation**  
   - Evaluate the performance of your model using appropriate metrics (e.g., accuracy, precision, recall, F1-score, ROC-AUC).
   - Analyze the results and discuss the impact of HPC resources on processing time and scalability.  

6. **Documentation and Reflection**  
   - Document your workflow, including the steps to access Plato, preprocess the data, run the model, and evaluate its performance.  
   - Reflect on the advantages and challenges of using HPC for large-scale machine learning.  

---

#### **Deliverables**  

Submit a structured report in .Rmd format and upload to your personal Github page in a new repo. I will be running your .Rmd file so please make sure your submission is executable and replicable. Remember to document your process thoroughly as with descriptions in the text of the .Rmd file. If there are other codes you ran (e.g., your job scheduling code) include those in the repo is well. Ensure the repo is well documented. 

1. **Code and Scripts**  
   - Submit all scripts used for dataset preparation, model training, and job submission.  
   - Ensure scripts are well-commented and organized for reproducibility.  

2. **Job Script**  
   - Include the job submission script with explanations of the resource requests and commands.  

3. **Results and Analysis**  
   - Provide a report summarizing:  
     - The dataset and preprocessing steps.  
     - The machine learning model and evaluation metrics.  
     - Performance analysis, including resource usage and computation time.  

---

#### **Assessment Criteria**  

1. **HPC Setup and Utilization (15%)**  
   - Successful access to Plato HPC and effective use of resources.  

2. **Dataset Preparation (15%)**  
   - Completeness and accuracy in preprocessing the dataset.  

3. **Model Implementation (25%)**  
   - Correctness and efficiency of the machine learning model implementation.  

4. **Job Script and Execution (20%)**  
   - Proper configuration of the job script and successful execution on the HPC cluster.  

5. **Results and Analysis (15%)**  
   - Clarity and depth of model evaluation and performance analysis.  

6. **Documentation (10%)**  
   - Quality of workflow documentation and insights on the HPC experience.  

---

#### **Tools and Resources**  
- [USask Plato HPC Cluster Documentation](https://wiki.usask.ca/display/ARC/Plato+HPC+Cluster)  
- Job schedulers: Slurm or equivalent on the Plato HPC  

---
