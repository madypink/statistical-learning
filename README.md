# statistical-learning
# 💻 Statistical Learning Project: Laptop Price Prediction & Clustering

This repository contains a project completed for the course **Statistical Learning** at the **University of Milan**, as part of the Master’s program in *Data Science for Economics*.  

The project applies **both supervised and unsupervised learning techniques** to analyze laptop specifications and prices. The main focus is on predicting laptop prices using regression models and clustering similar laptops based on their hardware characteristics.  

---

## 🎯 Project Overview

The main objectives of this project are:
- To perform **data visualization** to explore patterns and trends in laptop specifications and prices.  
- To apply **data preprocessing** techniques:
  - Handling missing values  
  - Standardizing numerical features  
  - Encoding categorical features  
- To apply **supervised learning algorithms** for predicting laptop prices.  
- To apply **unsupervised learning algorithms** for clustering similar laptops.  
- To evaluate model performance and interpret results to ensure accuracy and meaningful insights.  

---

## 📂 Contents

- **`report/statistical_project_Rabiee.pdf`** – Final written report (methodology, results, discussion).  
- **`code/statistical_codes_Rabiee.R`** – R script for data cleaning, analysis, and modeling.  
- **`presentation/STATISTICAL_LEARNING_MAEDEH_RABIEE.pptx`** – Slides summarizing the project’s approach and findings.  
- **`data/laptops.csv`** – Dataset used for analysis.  

---

## 📊 Dataset

The dataset used in this project is the [Laptops Price Dataset](https://www.kaggle.com/datasets/juanmerinobermejo/laptopsprice-dataset/data), hosted on Kaggle.  
It contains detailed laptop specifications (brand, processor, RAM, storage, GPU, etc.) along with their prices.  

---

## ⚙️ Methods

### 🔹 Data Preparation
- Data visualization to explore price trends and feature distributions.  
- Handling missing values and outliers.  
- Standardizing numerical variables (e.g., price, RAM, storage).  
- Encoding categorical features (brand, processor, GPU, etc.).  

### 🔹 Supervised Learning
- **Multiple Linear Regression**  
- **Ridge Regression**  
- **Lasso Regression**  
- Performance metrics: **RMSE**, **R²**  

### 🔹 Unsupervised Learning
- **K-Means Clustering** to group laptops with similar hardware features.  
- Cluster interpretation to identify consumer segments and product categories.  

---

## 📈 Results

- Supervised models (Ridge & Lasso) improved predictive performance compared to simple regression.  
- Key predictors of laptop prices included **processor type, RAM size, and storage capacity**.  
- K-Means clustering revealed distinct laptop groups (e.g., budget, mid-range, high-end).  
- The combined supervised + unsupervised approach provided both **accurate price predictions** and **useful insights into market segmentation**.  

---

## 🚀 How to Run

1. Clone this repository.  
2. Open `code/statistical_codes_Rabiee.R` in RStudio.  
3. Install required R packages:  
   ```r
   packages <- c("tidyverse", "caret", "glmnet", "ggplot2", "cluster", "factoextra")
   install.packages(setdiff(packages, installed.packages()[,1]))
