#### Install Required Packages ####
# Install packages used in the codes (if not yet installed) 
  packages <- c("ggplot2", "dplyr", "corrplot", "caret","tseries","forecast","factoextra","car","lmtest")
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      install.packages(pkg)
    }
  }

# Load the file data
  load("CourseWorkData.Rdata")

#### Install Required Packages ####
# 1. Data Cleaning
  # a. Renaming column name 
    names(chocolate) <- c("sales_units", "price", "ad1_grp", "ad2_banners", "promo_stores", "wage_increase", "time", "product", "region", "month", "year")
    str(chocolate) # see the new column name

  # b. Make numeric month & date column 
    # 1) Add numeric month column
      month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
      chocolate$month_numeric <- match(chocolate$month, month_names)
  
    # 2) Add date column
      chocolate <- chocolate %>%
        mutate(date = as.Date(paste(year, month_numeric, "01", sep = "-"), format = "%Y-%m-%d"))
      chocolate <- chocolate[order(chocolate$region,chocolate$time),]
      
  # c. Check if there are any missing value 
    sum(is.na(chocolate))  # no missing value in the data
  
  
# 2. Exploratory Data Analysis
  # a. Check outlier in each region using boxplot
    library(ggplot2)
    ggplot(chocolate, aes(x = region, y = sales_units)) +
      geom_boxplot(fill = "white", color = "darkblue", outlier.color = "red") +
      labs(
        title = "Boxplot of Chocolate Sales per Region",
        x = "Region",
        y = "Sales (units sold)"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size=10)  # Rotate x-axis labels for readability
      )
    
  # b. Make correlation matrix
    # 1) Select only numeric column (drop product, region, month)
      library(dplyr)
      numeric_columns <- chocolate[sapply(chocolate, is.numeric)]
    
    # 2) Make correlation matrix
      library(corrplot)
      cor_matrix <- cor(numeric_columns)
      corrplot(cor_matrix, method = "circle", tl.col = "black")
      # ‘wage’ exhibits a strong relationship with variables related to ‘time’ and ‘year’ (relevant for time series model)
  
    # 3) Make new variable from combining 2 variable (wage as a basis since it shows strong correlation with time & year)
      chocolate$wage_ad2 <- chocolate$wage_increase * chocolate$ad2_banners
      chocolate$wage_month <- chocolate$wage_increase * chocolate$month_numeric
      chocolate$wage_month_time <- chocolate$wage_increase * chocolate$month_numeric * chocolate$time
      chocolate$wage_time <- chocolate$wage_increase * chocolate$time
      
# 3. Data Transformation
  # a. Normalising Sales Data
    # 1) See the Sales Distribution
      ggplot(chocolate, aes(x = sales_units)) +
        geom_histogram(binwidth = 50, fill = "skyblue", color = "black") +
        labs(title = "Distribution of Units Sold", x = "Units Sold", y = "Frequency") +
        theme_minimal()  # distribution is not symmetrical
      
    # 2) Do log transformation for sales
      chocolate$sales_log <- log(chocolate$sales_units)
      # 2) a. Graph the new distribution
           ggplot(chocolate, aes(x = sales_log)) +
            geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
            labs(title = "Distribution of Log Units Sold", x = "Units Sold", y = "Frequency") +
            theme_minimal()  # distribution is more symmetrical
        
  # a. Splitting data for each region
    mexico_sales <- chocolate[chocolate$region == "Mexico", ]
    singapore_sales <- chocolate[chocolate$region == "Singapore", ]
    spain_sales <- chocolate[chocolate$region == "Spain", ]
    uk_sales <- chocolate[chocolate$region == "UK", ]
    us_sales <- chocolate[chocolate$region == "US", ]
    
  # b. Outlier Handling (lower bound at 10th percentile, upper bound 90th percentile)
    quantile_floor_cap <- function(data, column, lower_quantile = 0.1, upper_quantile = 0.9) {
      # 1) Calculate lower and upper bounds in a data
        lower_bound <- quantile(data[[column]], lower_quantile, na.rm = TRUE)
        upper_bound <- quantile(data[[column]], upper_quantile, na.rm = TRUE)
      # 2) Apply flooring and capping to the data
        data[[column]] <- ifelse(data[[column]] < lower_bound, lower_bound, data[[column]])
        data[[column]] <- ifelse(data[[column]] > upper_bound, upper_bound, data[[column]])
        return(data)
    }
      mexico_floor_cap <- quantile_floor_cap(mexico_sales, column = "sales_log")  
      singapore_floor_cap <- quantile_floor_cap(singapore_sales, column = "sales_log")
      spain_floor_cap <- quantile_floor_cap(spain_sales, column = "sales_log")
      uk_floor_cap <- quantile_floor_cap(uk_sales, column = "sales_log")
      us_floor_cap <- quantile_floor_cap(us_sales, column = "sales_log")
  
    # d. Time-Series Decomposition
      # 1) for each region
        decompose_and_plot <- function(region_name, data, start_year = 2010, freq = 12) {
          region_data <- data[data$region == region_name, ]
          ts_data <- ts(region_data$sales_log, start = c(start_year, 1), frequency = freq)
          ts_decomp <- stl(ts_data, s.window = "periodic")
          plot(ts_decomp, main = paste("Log - Chocolate Sales Decomposition for", region_name))
          
          return(ts_decomp)
        }
        regions <- unique(chocolate$region)
        decomp_results <- list()
        for (region in regions) {
          decomp_results[[region]] <- decompose_and_plot(region, chocolate)
        }
        
      # 2) Overall sales across all regions
        overall_data <- aggregate(sales_log ~ date, data = chocolate, sum)
        ts_overall <- ts(overall_data$sales_log, start = c(2010, 1), frequency = 12, end = c(2014, 12))
        overall_decomp <- stl(ts_overall, s.window = "periodic")
        plot(overall_decomp, main = "Log - Overall Chocolate Sales Decomposition")
        
      
# 4. Linear Regression Model
  # a. Set a seed for reproducibility
    set.seed(123)
  
  # b. Split the data into 80% training and 20% testing
    train_indices <- createDataPartition(chocolate$sales_units, p = 0.8, list = FALSE)
    chocolate_train <- chocolate[train_indices, ]
    chocolate_test <- chocolate[-train_indices, ]
    chocolate_train$log_sales_units <- log(chocolate_train$sales_units) 
    chocolate_test$log_sales_units <- log(chocolate_test$sales_units) 
    # Check the dimensions of train and test sets
    cat("Training Set Size:", nrow(chocolate_train), "\n")
    cat("Test Set Size:", nrow(chocolate_test), "\n")
    
  # c. Train the model with 5-fold cross-validation
    train_control <- trainControl(method = "cv", number = 5, savePredictions = "final")
    cv_model <- train(log(sales_units) ~ price + ad1_grp + ad2_banners + promo_stores + log(wage_increase) + month_numeric + region, 
                        data = chocolate_train, 
                        method = "lm", 
                        trControl = train_control)
    print(cv_model)  # RMSE: 0.7433385 R2: 85.04%, MAE: 0.6369182
    
  # d. Retrain the model on the full training set
    logitmod <- lm(log(sales_units) ~ price + ad1_grp + ad2_banners + promo_stores + log(wage_increase) + month_numeric + region, 
                     data = chocolate_train)
    summary(logitmod) # Adj R^2: 0.8522
      
  # e. Calculate the train data RMSE
    predictions_train <- predict(logitmod, newdata = chocolate_train)
    rmse_train <- sqrt(mean((log(chocolate_train$sales_units) - predictions_train)^2))
    cat("Train RMSE:", rmse_train, "\n") # Train RMSE: 0.7110297 
    
  # f. Calculate the test data RMSE
    predictions_test <- predict(logitmod, newdata = chocolate_test)
    rmse_test <- sqrt(mean((log(chocolate_test$sales_units) - predictions_test)^2))
    cat("Test RMSE:", rmse_test, "\n") # Test RMSE: 0.7072557
    
  # g. Testing LM Model
    predicted_log_sales_units <- predict(logitmod, chocolate_test)
    predicted_sales_units <- exp(predicted_log_sales_units)
    residuals <- chocolate_test$sales_units - predicted_sales_units
    check_actual_prediction <- data.frame(predicted_sales_units=predicted_sales_units,actual_sales=chocolate_test$sales_units)
  
    # 1) Normality testing
      shapiro.test(log(residuals))  # W = 0.95702, p-value = 0.4314
    
    # 2) Multicollinearity testing
      library("car")      
      vif(logitmod)  # all variable VIF Value < 2
    
    # 3) QQ Plot
      plot(logitmod, which = 2)  # reasonably normal in the central part of the distribution, outliers in the tails
    
    # 4) Heteroscedasticity Scatter Plot
      plot(logitmod, which = 3)  # Even spread of residuals, showing signs of homoscedasticity
    
    # 5) Residual plot
      plot(fitted(logitmod), residuals(logitmod),
           main = "Residuals vs Fitted",
           xlab = "Fitted Values",
           ylab = "Residuals")
      abline(h = 0, col = "red")  # residuals appears to be randomly distributed
      
    # 6) Component + Residual plot
      crPlots(logitmod)
    
    # 7) Breusch-Pagan Test
      bptest_log <- bptest(logitmod) 
      print(bptest_log)  # BP = 12.548, df = 10, p-value = 0.2501
      
    # 8) Durbin-Watson test
      dwtest_log <- dwtest(logitmod)
      print(dwtest_log)  # DW = 1.1274, p-value = 4.449e-13
      
      
# 5. Question 1: How many units sold are associated with each of the three marketing activities?
  # a. TV Ads
    chocolate_test_tv <- chocolate_test
    chocolate_test_tv$ad1_grp <- chocolate_test_tv$ad1_grp + 1
    predicted_log_sales_units_tv <- predict(logitmod, chocolate_test_tv)
    predicted_sales_units_tv <- exp(predicted_log_sales_units_tv)
        
    comparison_tv <- data.frame(predicted_sales_units=predicted_sales_units,predicted_sales_units_tv=predicted_sales_units_tv)
    comparison_tv$percentage<- comparison_tv$predicted_sales_units_tv/comparison_tv$predicted_sales_units - 1
    comparison_tv$absolut<- comparison_tv$predicted_sales_units_tv - comparison_tv$predicted_sales_units
    average_percent_change_tv <- mean(comparison_tv$percentage)
    print(average_percent_change_tv) # 0.0148766
        
  # b. Banner Ads
    chocolate_test_banner <- chocolate_test
    chocolate_test_banner$ad2_banners <- chocolate_test_tv$ad2_banners + 1
    predicted_log_sales_units_banner <- predict(logitmod, chocolate_test_banner)
    predicted_sales_units_banner <- exp(predicted_log_sales_units_banner)
        
    comparison_banner <- data.frame(predicted_sales_units=predicted_sales_units,predicted_sales_units_banner=predicted_sales_units_banner)
    comparison_banner$percentage<- comparison_banner$predicted_sales_units_banner/comparison_banner$predicted_sales_units - 1
    comparison_banner$absolut<- comparison_banner$predicted_sales_units_banner - comparison_banner$predicted_sales_units
    average_percent_change_banner <- mean(comparison_banner$percentage)
    print(average_percent_change_banner)  # 0.01023418
  
  # c. Store Promotions    
    chocolate_test_promo_stores <- chocolate_test
    chocolate_test_promo_stores$promo_stores <- chocolate_test_tv$promo_stores + 1
    predicted_log_sales_units_promo_stores <- predict(logitmod, chocolate_test_promo_stores)
    predicted_sales_units_promo_stores <- exp(predicted_log_sales_units_promo_stores)
      
    comparison_promo_stores <- data.frame(predicted_sales_units=predicted_sales_units,predicted_sales_units_promo_stores=predicted_sales_units_promo_stores)
    comparison_promo_stores$percentage<- comparison_promo_stores$predicted_sales_units_promo_stores/comparison_promo_stores$predicted_sales_units - 1
    comparison_promo_stores$absolut<- comparison_promo_stores$predicted_sales_units_promo_stores - comparison_promo_stores$predicted_sales_units
    average_percent_change_promo_stores <- mean(comparison_promo_stores$percentage)
    print(average_percent_change_promo_stores) # 0.007873215
    
    
# 6. Question 2: Our TV ads cost us £2,000,000 a year and our Banners £500,000 a year (in total for all regions). Which one is more cost-effective?
  # a) Sales prediction for TV Ads 
    chocolate$revenue <- chocolate$sales_units * chocolate$price
    forecast_data_TV <- chocolate
    forecast_data_TV$ad1_grp <- 0
    forecast_data_TV$ForecastTV0 <- exp(predict(logitmod, forecast_data_TV)) #Forecast if TV = 0
    forecast_data_TV$ForecastTV <- exp(predict(logitmod,chocolate)) #Forecast if TV = existing TV Ads
    
  # b) Sales prediction for Banner Ads 
    forecast_data_banner <- chocolate
    forecast_data_banner$ad2_banners <- 0
    forecast_data_banner$ForecastBanner0 <- exp(predict(logitmod, forecast_data_banner)) #Forecast if Banner = 0
    forecast_data_banner$ForecastBanner <- exp(predict(logitmod,chocolate)) #Forecast if Banner = existing Banner Ads
    
  # c) Ads impact per month
    comparison_per_month <- chocolate
    comparison_per_month$ForecastTV0 <- forecast_data_TV$ForecastTV0
    comparison_per_month$ForecastTV <- forecast_data_TV$ForecastTV
    comparison_per_month$ForecastSalesDiffTV <- comparison_per_month$ForecastTV - comparison_per_month$ForecastTV0
    comparison_per_month$ForecastBanner0 <- forecast_data_banner$ForecastBanner0
    comparison_per_month$ForecastBanner <- forecast_data_banner$ForecastBanner
    comparison_per_month$ForecastSalesDiffBanner <- comparison_per_month$ForecastBanner - comparison_per_month$ForecastBanner0
    comparison_per_month$ForecastRevDiffTV <- comparison_per_month$ForecastSalesDiffTV * comparison_per_month$price
    comparison_per_month$ForecastRevDiffBanner <- comparison_per_month$ForecastSalesDiffBanner * comparison_per_month$price
    
  # d) Calculating ROAS per year
    revenue_per_year <- comparison_per_month %>%
      group_by(year) %>%
      summarize(
        ForecastTV0 = sum(ForecastTV0),
        ForecastTV = sum(ForecastTV),
        ForecastSalesDiffTV = sum(ForecastSalesDiffTV),
        ForecastBanner0 = sum(ForecastBanner0),
        ForecastBanner = sum(ForecastBanner),
        ForecastSalesDiffBanner = sum(ForecastSalesDiffBanner),
        TVImpact = sum(ForecastRevDiffTV) *1000,
        BannerImpact = sum(ForecastRevDiffBanner) *1000,
        ad1_grp_Cost = 2000000,
        ad2_banners_Cost = 500000
      )
  
  # e) t-test to compare Banner and TV ROAS
    revenue_per_year$ad1_grp_ROAS <- revenue_per_year$TVImpact/revenue_per_year$ad1_grp_Cost
    revenue_per_year$ad2_banners_ROAS <- revenue_per_year$BannerImpact/revenue_per_year$ad2_banners_Cost
    t_test_result <- t.test(revenue_per_year$ad2_banners_ROAS, revenue_per_year$ad1_grp_ROAS, paired = TRUE, alternative = "greater")
    print(t_test_result) # t = 6.1175, df = 4, p-value = 0.001808
  
    
# 7. Question 3: Our sales often show a lot of variation. Can you explain to us possible sources of the variation, other than the marketing activities?
  # a. Time series decomposition - overall
    plot(overall_decomp, main = "Log - Overall Chocolate Sales Decomposition")
    
  # b. Contribution of wage to sales
    chocolate_test_wage_increase <- chocolate_test
    chocolate_test_wage_increase$wage_increase <- chocolate_test_wage_increase$wage_increase + 0.01
    predicted_log_sales_units_wage_increase <- predict(logitmod, chocolate_test_wage_increase)
    predicted_sales_units_wage_increase <- exp(predicted_log_sales_units_wage_increase)
    comparison_wage_increase <- data.frame(predicted_sales_units=predicted_sales_units,predicted_sales_units_wage_increase=predicted_sales_units_wage_increase)
    comparison_wage_increase$percentage<- comparison_wage_increase$predicted_sales_units_wage_increase/comparison_wage_increase$predicted_sales_units - 1
    comparison_wage_increase$absolut<- comparison_wage_increase$predicted_sales_units_wage_increase - comparison_wage_increase$predicted_sales_units
    average_percent_change_wage_increase <- mean(comparison_wage_increase$percentage)
    print(average_percent_change_wage_increase) # 0.04292144
    
  # c. Contribution of price to sales 
    chocolate_test_price <- chocolate_test
    chocolate_test_price$price <- chocolate_test_price$price + 0.01
    predicted_log_sales_units_price <- predict(logitmod, chocolate_test_price)
    predicted_sales_units_price <- exp(predicted_log_sales_units_price)
    comparison_price <- data.frame(predicted_sales_units=predicted_sales_units,predicted_sales_units_price=predicted_sales_units_price)
    comparison_price$percentage<- comparison_price$predicted_sales_units_price/comparison_price$predicted_sales_units - 1
    comparison_price$absolut<- comparison_price$predicted_sales_units_price - comparison_price$predicted_sales_units
    average_percent_change_price <- mean(comparison_price$percentage)
    print(average_percent_change_price) # 0.001484414

    
# 8. Forecast Model
  # a. Stationary check
    library(tseries)
    combined_data <- bind_rows(mexico_floor_cap, singapore_floor_cap,
                               spain_floor_cap, us_floor_cap, uk_floor_cap)
    
    # 1) ADF for each region
      adf_results <- combined_data %>%
        group_by(region) %>%
        summarise(
          adf_p_value = adf.test(sales_log, alternative = "stationary")$p.value,
          adf_statistic = adf.test(sales_log, alternative = "stationary")$statistic
        )
      print(adf_results) 
    
    # 2) ADF overall
      total_sales_log <- combined_data %>%
        group_by(time) %>%
        summarise(sum_sales_log = sum(sales_log, na.rm = TRUE), .groups = "drop")
      adf_overall <- adf.test(total_sales_log$sum_sales_log)
      print(adf_overall)
  
  # b. Calculate MAPE & Predict Next Month Sales for each region using PCA-SARIMAX 
    # using a function so we don't have to repeat for each region
      calculate_mape_region <- function(region_data, original_data, region_name) {
        library(ggplot2)
        library(dplyr)
        library(forecast)
        library(factoextra)  
        
        # 1) Prepare exogenous variables for PCA
          exog_data <- region_data %>%
            select(price, ad1_grp, ad2_banners, promo_stores, wage_increase, time, year, month_numeric, wage_ad2, wage_month, wage_month_time, wage_time)
          
          exog_scaled <- scale(exog_data) # Standardize the data (mean 0, variance 1)
          
        # 2) Applying PCA to the exogenous variables
          pca_result <- prcomp(exog_scaled, center = TRUE, scale. = TRUE)
          
          pca_components <- as.data.frame(pca_result$x[, 1:2])  # Select the first 2 principal components
          pca_components$date <- region_data$date
          
          data_with_pca <- region_data %>%
            left_join(pca_components, by = "date")              # Merge PCA components with the main dataset
          
          data_with_original <- data_with_pca %>%
            left_join(original_data %>% select(date, sales_units_original = sales_units), by = "date")
          
        # 3) Split data into training and testing sets
          train_data <- data_with_original %>% filter(year <= 2013)
          test_data <- data_with_original %>% filter(year == 2014)
          train_data <- train_data %>% arrange(year, month_numeric) # Ensure data is ordered
          test_data <- test_data %>% arrange(year, month_numeric)
          
          train_ts <- ts(train_data$sales_log, start = c(min(train_data$year), 1), frequency = 12) # create time series object for training
          xreg_train <- as.matrix(train_data[, c("PC1", "PC2")]) # set PCA components as exogenous variables
          
        # 4) Fit the SARIMAX model
          sarimax_model <- auto.arima(train_ts, xreg = xreg_train)
        
        # 5) Forecast the test data
          xreg_test <- as.matrix(test_data[, c("PC1", "PC2")])
          forecast_test <- forecast(sarimax_model, xreg = xreg_test, h = nrow(test_data))
          
        # 6) Back-transform forecasts and compare with original data
          actual_with_outliers <- test_data$sales_units_original  # Original sales values
          actual_without_outliers <- exp(test_data$sales_log)     # Sales with outlier handling
          forecasted_sales <- exp(forecast_test$mean)             # Forecasted sales
          
        # 7) Calculate MAPE
          mape1 <- mean(abs((actual_with_outliers - forecasted_sales) / actual_with_outliers)) * 100
          mape2<- mean(abs((actual_without_outliers - forecasted_sales) / actual_without_outliers)) * 100
          print(paste("MAPE (with outlier handling) for", region_name, ":", mape2))
          print(paste("MAPE (without outlier handling) for", region_name, ":", mape1))
          
        # 8) PCA plot - Individuals Graph
          individuals_plot <- fviz_pca_ind(pca_result,
                                           geom.ind = "point",  # Represent individuals as points
                                           pointsize = 3,       # Size of the points
                                           col.ind = "cos2",    # Color by quality of representation
                                           gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),  # Color gradient
                                           repel = TRUE         # Avoid text overlapping
          ) + ggtitle(paste("PCA - Individuals Plot -", region_name))
          
          print(individuals_plot)
          
        # 9) PCA plot - Variables Graph
          variables_plot <- fviz_pca_var(pca_result,
                                         col.var = "contrib",   # Color by contributions to the PCs
                                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),  # Color gradient
                                         repel = TRUE) + ggtitle(paste("PCA - Variables Plot -", region_name))
          
          print(variables_plot)
          
        # 10) Plot actual vs forecasted
          test_forecast_data <- data.frame(date = test_data$date,actual_sales = actual_with_outliers,
            forecasted_sales = forecasted_sales)
          
          plot_test_vs_forecast <- ggplot(test_forecast_data, aes(x = date)) +
            geom_line(aes(y = actual_with_outliers, color = "Actual (without outlier handling)"), size = 1) +
            geom_line(aes(y = actual_without_outliers, color = "Actual (with outlier handling)"), linetype = "solid", size = 1) +
            geom_line(aes(y = forecasted_sales, color = "Forecasted Sales"), linetype = "dashed", size = 1) +
            labs(title = paste(region_name, "Chocolate Sales - Actual vs Forecasted (Year 2014)"),
              x = "Date", y = "Sales (Units)") +
            scale_color_manual(name = NULL, 
                               values = c("Actual (without outlier handling)" = "black",
                                          "Actual (with outlier handling)" = "orange",
                                          "Forecasted Sales" = "#0173b2")) +
            scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  legend.position = "bottom",  # Move the legend to the bottom
                  legend.text = element_text(size = 10),  # Reduce legend text size
                  legend.key.size = unit(0.8, "lines"))
          
          print(plot_test_vs_forecast)
          
        # 11) Forecast for the next month (January 2015)
          latest_data <- data_with_pca %>% filter(year == 2014 & month_numeric == 12)
          latest_jan_data <- data_with_pca %>% filter(year == 2014 & month_numeric == 1)
          next_month_data <- latest_data %>%  # adjust the exogenous variable
            summarise(
              price = last(price), # using the latest data (Dec 2014)
              ad1_grp = latest_jan_data$ad1_grp, # using latest Seasonality data (Jan 2014)
              ad2_banners = latest_jan_data$ad2_banners, # using latest Seasonality data (Jan 2014)
              promo_stores = latest_jan_data$promo_stores, #using latest Seasonality data (Jan 2014)
              wage_increase = last(wage_increase), # using the latest data (Dec 2014)
              time = max(time) + 1,  # Increment time by 1 to represent the next time period
              year = 2015,  # Forecast for the year 2015
              month_numeric = 1,  # January is month 1
              wage_ad2 = wage_increase * ad2_banners,
              wage_month = wage_increase * month_numeric,
              wage_month_time = wage_increase * month_numeric * (max(time) + 1),  # Use incremented time
              wage_time = wage_increase * (max(time) + 1)  # Use incremented time
            )
          
          mean_values <- attr(exog_scaled, "scaled:center") # Use the mean and sd from original scaling
          sd_values <- attr(exog_scaled, "scaled:scale")
          next_month_data_scaled <- as.data.frame(scale(next_month_data, center = mean_values, scale = sd_values)) # Standardize the next month's data
          next_month_pca <- as.data.frame(predict(pca_result, newdata = next_month_data_scaled)) # Apply PCA
          xreg_next_month <- as.matrix(next_month_pca[, 1:2])  # Create the exogenous variable matrix for the next month (using same number of PCs)
          forecast_next_month <- forecast(sarimax_model, xreg = xreg_next_month, h = 1) # Forecast using the SARIMAX model
          forecasted_sales_next_month <- exp(forecast_next_month$mean) # Back-transform from log scale
          
          print(paste("Forecasted Sales for January 2015 (", region_name, "):", forecasted_sales_next_month))
          
          # 12) Plot next month's forecast for visualization
          combined_data <- data.frame( date = c(train_data$date, test_data$date),
                                       sales = c(exp(train_data$sales_log), actual_with_outliers),
                                       type = rep("Actual", length(train_data$sales_log) + length(test_data$sales_log)))
          
          forecast_data <- data.frame(date = as.Date("2015-01-01"), 
                                      sales = forecasted_sales_next_month,
                                      type = "Next Month Forecast")
          
         combined_data <- rbind(combined_data, forecast_data)
          
         plot <- ggplot(combined_data, aes(x = date, y = sales, color = type)) +
            geom_line(data = combined_data %>% filter(type == "Actual"), size = 1) +
            geom_point(data = combined_data %>% filter(type == "Next Month Forecast"), size = 3) +
            labs(title = paste(region_name, " Sales: Actual vs Next Month Forecast"),
              x = "Date", y = "Sales (Units)") +
            scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
          
          print(plot)
          
        # Return as a data frame
        return(
          data.frame(region = region_name, 
                     date = test_data$date,
                     actual_with_outliers = actual_with_outliers, 
                     actual_without_outliers = actual_without_outliers,
                     forecasted = forecasted_sales
                     ))
      }
      
  # c. Apply function for each region
    mexico_mape_results <- calculate_mape_region(mexico_floor_cap, mexico_sales, "Mexico")
    mexico_mape_results
      
    singapore_mape_results <- calculate_mape_region(singapore_floor_cap, singapore_sales, "Singapore")
    singapore_mape_results      
      
    spain_mape_results <- calculate_mape_region(spain_floor_cap, spain_sales, "Spain")
    spain_mape_results      
      
    uk_mape_results <- calculate_mape_region(uk_floor_cap, uk_sales, "UK")
    uk_mape_results  
    
    us_mape_results <- calculate_mape_region(us_floor_cap, us_sales, "US")
    us_mape_results      
    
  # d. Calculate overall MAPE
    # 1) combine all of the prediction
      filtered_date <- mexico_sales %>%
        filter(format(as.Date(date), "%Y") == "2014") %>% 
        arrange(as.Date(date))
      
      overall_mape_result <- mexico_mape_results %>%
        mutate(
          date = filtered_date$date,
          actual_with_outliers = actual_with_outliers + singapore_mape_results$actual_with_outliers + spain_mape_results$actual_with_outliers + uk_mape_results$actual_with_outliers + us_mape_results$actual_with_outliers,
          actual_without_outliers = actual_without_outliers + singapore_mape_results$actual_without_outliers + spain_mape_results$actual_without_outliers + uk_mape_results$actual_without_outliers + us_mape_results$actual_without_outliers,
          forecasted = forecasted + singapore_mape_results$forecasted + spain_mape_results$forecasted + uk_mape_results$forecasted + us_mape_results$forecasted
        )
      
      overall_mape_result$region <- "Overall"  # change region name to Overall
      print(overall_mape_result)  
    
    # 2) Calculate the overall MAPE
      overall_mape_1 <- mean(abs((overall_mape_result$actual_with_outliers - overall_mape_result$forecasted) / overall_mape_result$actual_with_outliers)) * 100
      overall_mape_2 <- mean(abs((overall_mape_result$actual_without_outliers - overall_mape_result$forecasted) / overall_mape_result$actual_without_outliers)) * 100
      print(paste("Overall MAPE with outlier handling:", overall_mape_2))
      print(paste("Overall MAPE without outlier handling:", overall_mape_1))
      
    # 3) Plot Actual vs Forecast
      plot_test_vs_forecast <- ggplot(overall_mape_result, aes(x = date)) +
        geom_line(aes(y = actual_with_outliers, color = "Actual (without outlier handling)"), size = 1) +
        geom_line(aes(y = actual_without_outliers, color = "Actual (with outlier handling)"), linetype = "solid", size = 1) +
        geom_line(aes(y = forecasted, color = "Forecasted Sales"), linetype = "dashed", size = 1) +
        labs(
          title = paste("Overall Chocolate Sales - Actual vs Forecasted (Year 2014)"),
          x = "Date",
          y = "Sales (Units)") +
        scale_color_manual(name = NULL, 
                           values = c("Actual (without outlier handling)" = "black",
                                      "Actual (with outlier handling)" = "orange",
                                      "Forecasted Sales" = "#0173b2")) +
        scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom",  # Move the legend to the bottom
              legend.text = element_text(size = 10),  # Reduce legend text size
              legend.key.size = unit(0.8, "lines"))
      
      print(plot_test_vs_forecast)
    