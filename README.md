# Car Price Prediction with Regression and Decision Trees (1970–2024 Dataset)

This project focuses on building predictive models for second-hand car prices using a dataset of more than 90,000 cars spanning from 1970 to 2024.

## 📌 Project Overview
The goal of this project is to design and evaluate regression models and decision trees that can estimate used car prices based on technical and categorical features. The dataset comes from Kaggle and covers a broad spectrum of vehicle attributes such as manufacturer, model, year, engine size, transmission type, fuel type, mileage, and more.

### Research Questions
1. How do technical characteristics (year, engine size, transmission, model) influence car prices?  
2. Which features are the strongest predictors of second-hand car values?  
3. Can combining multiple variables improve the accuracy of price predictions?  

## 📊 Dataset
- Source: [Kaggle – 90,000 Cars Data (1970–2024)](https://www.kaggle.com/datasets/meruvulikith/90000-cars-data-from-1970-to-2024/data)  
- Number of entries: 90,000+  
- Key features:  
  - **Model**  
  - **Year**  
  - **Price**  
  - **Transmission**  
  - **Mileage**  
  - **FuelType**  
  - **Tax**  
  - **MPG**  
  - **EngineSize**  
  - **Manufacturer**

## 🛠 Methods
- **Linear Regression (lm in R):**  
  Tested models on individual variables and combined models (engine size, year, transmission, manufacturer, model).  
- **Decision Trees:**  
  Built using R to identify hierarchical importance of features.  
- **Model Evaluation:**  
  Used R², Adjusted R², F-statistic, p-values, and RMSE to assess accuracy.  
- **Training & Testing:**  
  70% training data, 30% test data split.

## 📈 Results
- Full regression model explained ~90% of the variance (R² = 0.9024).  
- Engine size and model were strong predictors of car prices.  
- Complex regression model outperformed individual-variable models (lowest RMSE ~4316).  
- Decision trees offered interpretability and feature importance, highlighting **model**, **year**, and **engine size** as most influential.  

## 🚗 Key Insights
- Car **model** and **manufacturer** strongly influence second-hand value due to brand perception.  
- **Newer cars** and those with **larger engines** generally achieve higher prices.  
- Combining multiple predictors provides the best accuracy for price prediction.  

## 🔧 Tools & Technologies
- **Language:** R  
- **IDE:** RStudio  
- **Techniques:** Regression, Decision Trees, Cross-validation  
- **Libraries:** base R (lm), rpart, caret, ggplot2  

