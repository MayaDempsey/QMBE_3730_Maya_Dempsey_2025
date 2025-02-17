# Script Name: ABA Assignment 1
# 
#   Purpose:Assignment 1
#   Author(s): Jorge Zelaya Velasquez
#   Date Created: 02/16/2025
#   
#   Notes: 
#   
#

--------------------------------------------------------------------------------

  #liberary and packages
  
install.packages("readr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
install.packages("zoo")
install.packages("forecast")

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(zoo)
library(forecast)


--------------------------------------------------------------------------------
setwd("C:/Users/jorge/OneDrive/Business Analytics/Advanced Data Analytics")
getwd()

#Load the data

summary(Sales) #Summary of the data


##A. COMPUTING MA FOR WK3,WK4,WK5 -- CREATING 3 NEW COLUMNS
sales_data <- Sales %>%   
  mutate(MA_3 = zoo::rollmean(Sales, 3, fill = NA),
         MA_4 = zoo::rollmean(Sales, 4, fill = NA),
         MA_5 = zoo::rollmean(Sales, 5, fill = NA))   #Creates 3 columns with the moving averages

View(sales_data) #View the data

new_sales_data <- sales_data %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) #Replaces missing values with 0 


##B. COMPUTING EXPONENTIAL SMOOTHING FORECASTS
#alpha = 0.5

alpha <- 0.5
exponential_smoothing <- ses(new_sales_data$Sales, alpha=alpha) #Computes the exponential smoothing forecasts using 'ses' function
sales_data_and_exp_smooth <- cbind(new_sales_data, exponential_smoothing$fitted) #Creates a column with the exponential smoothing forecasts

View(sales_data_and_exp_smooth) #View the data

##C. COMPUTING MSE, MAE, AND MAPE FOR EACH OF THE FORECASTS

#MSE
MSE_MA_3 <- mean((sales_data_and_exp_smooth$Sales - sales_data_and_exp_smooth$MA_3)^2, na.rm = TRUE) #Computes the Mean Squared Error))
MSE_MA_4 <- mean((sales_data_and_exp_smooth$Sales - sales_data_and_exp_smooth$MA_4)^2, na.rm = TRUE) #Computes the Mean Squared Error))
MSE_MA_5 <- mean((sales_data_and_exp_smooth$Sales - sales_data_and_exp_smooth$MA_5)^2, na.rm = TRUE) #Computes the Mean Squared Error))

#MAE
MAE_MA_3 <- mean(abs(sales_data_and_exp_smooth$Sales - sales_data_and_exp_smooth$MA_3), na.rm = TRUE) #Computes the Mean Absolute Error))
MAE_MA_4 <- mean(abs(sales_data_and_exp_smooth$Sales - sales_data_and_exp_smooth$MA_4), na.rm = TRUE) #Computes the Mean Absolute Error))
MAE_MA_5 <- mean(abs(sales_data_and_exp_smooth$Sales - sales_data_and_exp_smooth$MA_5), na.rm = TRUE) #Computes the Mean Absolute Error))

MAPE_MA_3 <- mean(abs(sales_data_and_exp_smooth$Sales - sales_data_and_exp_smooth$MA_3)/sales_data_and_exp_smooth$Sales * 100, na.rm = TRUE) #Computes the Mean Absolute Percentage Error))))
MAPE_MA_4 <- mean(abs(sales_data_and_exp_smooth$Sales - sales_data_and_exp_smooth$MA_4)/sales_data_and_exp_smooth$Sales * 100, na.rm = TRUE) #Computes the Mean Absolute Percentage Error))))
MAPE_MA_5 <- mean(abs(sales_data_and_exp_smooth$Sales - sales_data_and_exp_smooth$MA_5)/sales_data_and_exp_smooth$Sales * 100, na.rm = TRUE) #Computes the Mean Absolute Percentage Error))))

##D. Find value for alpha that minimizes the MSE for exponential forecast

#Convert to time series object
ts_data <- ts(new_sales_data$Sales, start = c(2020, 1), frequency = 52)

#Creeting Numeric Vector
new_sales_data_vector <- as.numeric(ts_data) #Converts the data frame to a numeric vector

#Create a function that computes the MSE for the exponential smoothing forecasts
compute_mse <- 
  function(alpha, new_sales_data_vector) {
    exponential_smoothing <- ses(new_sales_data_vector, alpha=alpha) #Computes the exponential smoothing forecasts using 'ses' function
    fitted_values <- fitted(exponential_smoothing) #Extracts the fitted values; gets the predicted sales values from the model to compare with actual sales data
    valid_indices <- which(!is.na(new_sales_data_vector[-1]) & !is.infinite(new_sales_data_vector[-1]) & !is.na(fitted_values[-1]) & !is.infinite(fitted_values[-1])) ##Removing NA and Inf Values
    mse <- mean((new_sales_data_vector[valid_indices] - fitted_values[valid_indices]) ^ 2, na.rm = TRUE)
    return(mse)
  }


 
#Optimal Alpha -- 0.1208263
optimal_alpha <- optimize(compute_mse, c(0, 1), new_sales_data_vector=new_sales_data_vector)$minimum #Finds the value of alpha that minimizes the MSE

print(optimal_alpha) #Prints the optimal alpha value


##E. Comparing MSE, MAE, and MAPE -- which of all of your forecast performs the best?
#The best performing forecast is the MSE since it has the closest sales value compared to the other forecasts. 

##BA Chapter 8; Case Problem 1: Forecasting Food and Beverage Sales for Karen
#Include regreassing that accounts for a trend and seasonal effects
#Make sure to answer all questions in the problem

#Load the data
install.packages("data.table")
install.packages("ggplot2")
library(data.table)
library(ggplot2)

#Create the data table
Vintage_Restaruant <- data.table(
  Month = c('January','February','March','April','May','June','July','August','September','October','November','December'),
  First_Year = c(242,235,232,178,184,140,145,152,110,130,152,203),
  Second_Year = c(263,238,247,193,193,149,157,161,122,130,167,230),
  Third_Year = c(282,255,265,205,210,160,166,174,126,148,173,235)
)

#Reshape Data for GGPLOT
Vintage_Restaurant_long <- melt(Vintage_Restaruant, id.vars = "Month", 
                                variable.name = "Year", value.name = "Sales")

#1. Create time series plot. Comment on the underlying patterin in the time series
ggplot(Vintage_Restaurant_long, aes(x = Month, y = Sales, color = Year, group = Year)) +
  geom_line() +
  geom_point() +
  labs(title = "Vintage Restaurant Sales", 
       y = "Sales", 
       x = "Month") +
  theme_minimal() +

  #The underlying pattern in the time series is that the sales increase from the first year to the third year. The Sales are at its highest when entering the winter season.
  
#2. Using the dummy variable approach, forecast sales for January through December of the fourth year. How would you explain this model to Karen?

#Forecasting Sales for the Fourth Year
Vintage_Restaruant_V2 <- data.table(
  Month = factor(c('January','February','March','April','May','June','July','August','September','October','November','December')),
  First_Year = c(242,235,232,178,184,140,145,152,110,130,152,203),
  Second_Year = c(263,238,247,193,193,149,157,161,122,130,167,230),
  Third_Year = c(282,255,265,205,210,160,166,174,126,148,173,235),
  Fourth_Year = rollmean(Vintage_Restaruant$Third_Year, 3, fill = "extend") #Creates a column with the moving averages
)

#Converting Month to Factor Variable
Vintage_Restaurant_V2<- factor(Vintage_Restaruant_V2$Month, 
                                      levels = c('January','February','March','April','May','June','July','August','September','October','November','December'))
#Creating a Linear Regression Model
model <- lm(Fourth_Year ~ Month, data = Vintage_Restaruant_V2) #Creates a linear regression model using the dummy variable approach
summary(model) #In this model the interecept represents the average sales for the first year


#Model Explanation
#The intercept at 226.67 represents the baseline sales. It is predicted to be higher than the baseline sale
#in January, February, and March. The resot of the other months are predicted to be lower than that baselie
#sales. The model predicts that the sales will increase in the first half of the year and decrease in the
#second half of the year.
#In order to find out the actual sales in that month, all you need to do is find the coefficient and either add
#or subtract it from the baseline sales (226.67).


#How to resolve Karens uncertainty?
#The model can be used to forecast sales but will need to be updated as new data becomes available. The model
#do help highlight the trends and seasonality in the data. This helps read any potential patterns and prepare for
#months where it looks like sales will be lower than the baseline.