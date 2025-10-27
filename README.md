# Marketing Mix Modeling on Chocolate Sales

This project analyzes the relationship between marketing activities and chocolate sales across multiple regions, using regression and time-series forecasting in R. The objective is to identify the most cost-effective marketing strategy and forecast future sales for Heavenly Chocolate, a growing confectionery company.

## Key Features
- **Multiple Linear Regression** to quantify sales impact from TV ads, banner ads, and store promotions  
- **ROAS (Return on Ad Spend) analysis** comparing cost-effectiveness of marketing channels  
- **PCA-SARIMAX Forecasting** for multi-region time-series prediction with exogenous factors  
- **Statistical Validation:** Normality, multicollinearity, heteroscedasticity, and autocorrelation checks  

## Results
- **Banner ads** are significantly more cost-effective than **TV ads** (p < 0.05)  
- Key drivers of sales: wage increase (4.29% per 0.01 rise), TV and banner advertising intensity  
- PCA-SARIMAX model achieved overall **MAPE ≈ 28%**, with accurate forecasts per region  

## Tech Stack
R · ggplot2 · forecast · tseries · stats · car  

## Repository Structure
- `Marketing Mix Modeling.R` – Main R script for data cleaning, modeling, and forecasting  
- `CourseWorkData.RData` – Dataset used for regression and forecasting models  

## How to Run
1. Open `Marketing Mix Modeling.R` in RStudio  
2. Load `CourseWorkData.RData`  
3. Run all sections sequentially to reproduce regression and forecasting results  

