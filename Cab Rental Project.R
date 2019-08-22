# =========================================================================================#
#
#                                   Installing Libraries
#
# =========================================================================================#
library_tobe_used = c('ggplot2', 'corrgram', 'corrplot', 'caret', 'class',
                      'e1071', 'rpart', 'dplyr','caTools','LiblineaR',
                      'DMwR','usdm', 'mlr','grid', 'randomForest')
install.packages(library_tobe_used)

# =========================================================================================#
#
#                                   Setting Directory
#
# =========================================================================================#
getwd()
setwd("~/Documents/Job Search/edWisor/MyProjects/1. CabRental/R Final")
#setwd(dir = 'Path_to_folder_containing_data_files')
getwd()

# =========================================================================================#
#
#                                   1. Loading Data Files
#
# =========================================================================================#
training_data_full = read.csv('train_cab.csv')
testing_data_full = read.csv('test.csv')

# =========================================================================================#
#
#                                   2. Exploring Data
#
# =========================================================================================#
str(training_data_full)
sapply(training_data_full, class)
sapply(training_data_full, typeof)
summary(training_data_full)

# =========================================================================================#
#
#                                   3. Preprocessing Dara
#
# =========================================================================================#

# ============================== #
# 3.1 Missing Value Analysis
# ============================== #
show_missing_vals = function(data_frame){sapply(data_frame, function(feature) sum(is.na(feature)))}
show_missing_vals(training_data_full)

# ============================== #
# 3.2 Converting types of data
# ============================== #
training_data_full$fare_amount = as.numeric(levels(training_data_full$fare_amount))[training_data_full$fare_amount]
training_data_full$passenger_count = as.integer(training_data_full$passenger_count)
training_data_full$pickup_datetime = as.POSIXlt(strptime(training_data_full$pickup_datetime, "%Y-%m-%d %H:%M:%S"))

show_missing_vals(training_data_full)
str(training_data_full)
sapply(training_data_full, class)
summary(training_data_full)

# ============================== #
# 3.2 Removing 0s & NAs rows
# ============================== #
remove_na_rows = function(data_frame){
  na_rows = apply(data_frame, 1, function(df_row) any(is.na(df_row)))
  data_frame = data_frame[!na_rows, ]
  return(data_frame)
}
clean_training_data = remove_na_rows(training_data_full)

remove_0s_rows = function(data_frame){
  rows_0s = apply(data_frame, 1, function(df_row) any(df_row == 0))
  data_frame = data_frame[!rows_0s, ]
  return(data_frame)
}
clean_training_data = remove_0s_rows(clean_training_data)

# ============================== #
# 3.3 Outliers
# ============================== #
# boxplot function to plot boxplot of columns
boxplot(clean_training_data$fare_amount, data = clean_training_data)

#histgram function to plot histogram
hist(clean_training_data$passenger_count, col = 'green', border = 'red')

library('ggplot2')
#Box Plot using ggplot2 library
box_plot = function(column, data_frame){
  ggplot(data_frame, aes_string(y=column))+
    geom_boxplot(outlier.color = 'seagreen', outlier.shape = 8, outlier.size = 2)
}
box_plot(clean_training_data$pickup_longitude, clean_training_data)

#Box Plot using ggplot2 library
hist_plot = function(column, data_frame, binsize){
  ggplot(data = data_frame, aes_string(column)) + geom_histogram(binwidth = binsize)
}
hist_plot('passenger_count', clean_training_data, 1000)

find_outlier_range = function(column, data_frame){
  #column - string
  q1 = quantile(data_frame[ ,column], 0.25)
  q3 = quantile(data_frame[ ,column], 0.75)
  iqr = q3 - q1
  lower_range = q1 - 1.5*iqr
  upper_range = q3 + 1.5*iqr
  print(lower_range)
  print(upper_range)
  return(c(lower_range, upper_range))
}
manual_outlier_range = data.frame(
  fare_amount = c(1, 100),
  pickup_longitude = c(-74.8, -72.8),
  pickup_latitude = c(39.45, 41.45),
  dropoff_longitude = c(-74.8, -72.8),
  dropoff_latitude = c(39.45, 41.45),
  passenger_count = c(1, 6)
)

iqr_outlier_range = data.frame(
  fare_amount = c(0, 22.25),
  passenger_count = c(0, 3.5),
  onroad_distance = c(0, 10380),
  onroad_time = c(0, 1961),
  amnt_per_km = c(0.05, 5.8),
  amnt_per_hr = c(8.15, 87)
)

is_in_range = function(num, column){
  #column - string
  lr = manual_outlier_range[1,column]
  ur = manual_outlier_range[2,column]
  if(num >= lr && num <= ur){
    return(TRUE)
  }
  else return(FALSE)
}

# ============== Removing Outliers ================
remove_outliers = function(df){
  df = df[df$fare_amount >= manual_outlier_range[1,1] & df$fare_amount <= manual_outlier_range[2,1], ]
  df = df[df$pickup_longitude >= manual_outlier_range[1,2] & df$pickup_longitude <= manual_outlier_range[2,2], ]
  df = df[df$pickup_latitude >= manual_outlier_range[1,3] & df$pickup_latitude <= manual_outlier_range[2,3], ]
  df = df[df$dropoff_longitude >= manual_outlier_range[1,4] & df$dropoff_longitude <= manual_outlier_range[2,4], ]
  df = df[df$dropoff_latitude >= manual_outlier_range[1,5] & df$dropoff_latitude <= manual_outlier_range[2,5], ]
  df = df[df$passenger_count >= manual_outlier_range[1,6] & df$passenger_count <= manual_outlier_range[2,6], ]
  return(df)
}

remove_outliers_iqr = function(df){
  df = df[df$fare_amount >= iqr_outlier_range[1,1] & df$fare_amount <= iqr_outlier_range[2,1], ]
  df = df[df$passenger_count >= iqr_outlier_range[1,2] & df$passenger_count <= iqr_outlier_range[2,2], ]
  df = df[df$onroad_distance >= iqr_outlier_range[1,3] & df$onroad_distance <= iqr_outlier_range[2,3], ]
  df = df[df$onroad_time >= iqr_outlier_range[1,4] & df$onroad_time <= iqr_outlier_range[2,4], ]
  df = df[df$amnt_per_km >= iqr_outlier_range[1,5] & df$amnt_per_km <= iqr_outlier_range[2,5], ]
  df = df[df$amnt_per_hr >= iqr_outlier_range[1,6] & df$amnt_per_hr <= iqr_outlier_range[2,6], ]
  return(df)
}

clean_training_data = remove_outliers(clean_training_data)

hist_plot('fare_amount', clean_training_data, binsize = 10)
box_plot(column = 'passenger_count', clean_training_data)

# =========================================================================================#
#
#                                   3. Feature Engineering
#
# =========================================================================================#
is_weekday = function(x){
  if (x==0 || x==6){
    return(0)
  }
  return(1)
}

is_nght_hr = function(x){
  if (x==4 || x==5 || x==6){
    return(1)
  }
  return(0)
}

# ================ Stripping DateTime into year, month, wday & hour =======================
clean_training_data$year = clean_training_data$pickup_datetime$year + 1900
clean_training_data$mnth = clean_training_data$pickup_datetime$mon
clean_training_data$hour = clean_training_data$pickup_datetime$hour
clean_training_data$wday = clean_training_data$pickup_datetime$wday

#Dropping the pickup_datetime column
clean_training_data = clean_training_data[-c(2)]

# Finding Aerial Distance betweem 2 points
install.packages("geosphere")
library(geosphere)
clean_training_data$aerial_distance = distVincentyEllipsoid(clean_training_data[,2:3], clean_training_data[4:5])

#library(tidyr)
#b = unite(b, col = 'coords', ... = c('pickup_longitude', 'pickup_latitude', 'dropoff_longitude', 'dropoff_latitude'), sep = ",", remove = TRUE)
#separate(data = b, col = 'coords', into = c('plo','pla','dlo','dla'), sep = ',')

# ========= Making categorical variables from the above stripped datetime features ========
clean_training_data$weekday = lapply(clean_training_data$wday, function(x) sapply(x, is_weekday))
clean_training_data$weekday = as.integer(clean_training_data$weekday)
clean_training_data$nght_hr = lapply(clean_training_data$hour, function(x) sapply(x, is_nght_hr))
clean_training_data$nght_hr = as.integer(clean_training_data$nght_hr)

# ========= Finding onroad_distance & onroad_time ===================================================================
clean_training_data$onroad_distance = 0
clean_training_data$onroad_time = 0
clean_training_data$onroad_distance = as.integer(clean_training_data$onroad_distance)
clean_training_data$onroad_time = as.integer(clean_training_data$onroad_time)

#Resetting index numbers due to removal of some rows
rownames(clean_training_data) = 1:nrow(clean_training_data)

# ============= Making a copy of this data ===========================
clean_training_data_copy = clean_training_data

install.packages('gmapsdistance')
library(gmapsdistance)
gmaps_key = 'your_google_api_key'
results = gmapsdistance(origin = "Washington+DC", 
                        destination = "New+York+City+NY", 
                        mode = "driving", key = gmaps_key)

####################################################################################################################################
#========================= DO NOT RUN THE BELOW CODE ON COMPLETE DATA FRAME: EXTREMELY TIME CONSUMING =============================#
####################################################################################################################################
# ================ For demo, uncomment and run on a small sample to check working or not =============================
sample_df = clean_training_data[6700:6710,]
df = sample_df
# >>> onroad_distance & time are all 0s
summary(df)
# Fetching from DistanceMatrixAPIs
for (i in 1:nrow(df)) {
  origin_coord = paste(... = toString(df$pickup_latitude[i]),toString(df$pickup_longitude[i]), sep = '+')
  dest_coord = paste(... = toString(df$dropoff_latitude[i]),toString(df$dropoff_longitude[i]), sep = '+')
  api_resp = gmapsdistance(origin = origin_coord,
                           destination = dest_coord,
                           mode = 'driving',
                           key = gmaps_key)
  df$onroad_distance[i] = api_resp$Distance
  df$onroad_time[i] = api_resp$Time
}
# >>> onroad_distance & time are filled now
summary(df)
####################################################################################################################################
#========================= DO NOT RUN THE ABOVE CODE ON COMPLETE DATA FRAME: EXTREMELY TIME CONSUMING =============================#
####################################################################################################################################

# ================================================================================================ #
# Filling the onroad_distance & time using already fetched data from GoogleMaps API stored in file #
# ================================================================================================ #
gmaps_data = read.csv('gmaps_data.csv')
gmaps_data = gmaps_data[-c(1)]
# For verification - see nrow(clean_training_data) == nrow(gmaps_data)
clean_training_data$onroad_distance = gmaps_data$ord
clean_training_data$onroad_time = gmaps_data$ort

# Dropping coordinates columns
clean_training_data = clean_training_data[-c(2,3,4,5)]

# Making a copy of training_data
clean_training_data_copy = clean_training_data

# Remove Noisy Data
remove_noisy = function(df){
  df = df[df$onroad_distance >= 100, ]
}
clean_training_data = remove_noisy(clean_training_data)
summary(clean_training_data)

# Making a copy of training_data
clean_training_data_copy = clean_training_data

# Making More features
clean_training_data$amnt_per_km = clean_training_data$fare_amount * 1000 / clean_training_data$onroad_distance
clean_training_data$amnt_per_hr = clean_training_data$fare_amount * 3600 / clean_training_data$onroad_time
clean_training_data = clean_training_data[clean_training_data$amnt_per_km > 1 & clean_training_data$amnt_per_km < 20, ]
clean_training_data = clean_training_data[clean_training_data$amnt_per_hr > 20 & clean_training_data$amnt_per_hr < 200, ]

#Resetting index numbers due to removal of some rows
rownames(clean_training_data) = 1:nrow(clean_training_data)

# Making a copy of training_data
clean_training_data_copy = clean_training_data

#Writing to .CSV file for later use. DO NOT OVERWRITE IT
# write.csv(clean_training_data, file = "clean_training_data_r.csv", row.names = TRUE)

numerical_cols = c('fare_amount','passenger_count','aerial_distance','onroad_distance','onroad_time','amnt_per_km','amnt_per_hr')
categorical_cols = c('year','mnth','hour','wday','weekday','nght_hr')

box_plot(clean_training_data$fare_amount, clean_training_data)
hist_plot(clean_training_data$fare_amount, clean_training_data, 1)
box_plot(clean_training_data$passenger_count, clean_training_data)
hist_plot(clean_training_data$passenger_count, clean_training_data, 1)
box_plot(clean_training_data$aerial_distance, clean_training_data)
hist_plot(clean_training_data$aerial_distance, clean_training_data, 100)
box_plot(clean_training_data$onroad_distance, clean_training_data)
hist_plot(clean_training_data$onroad_distance, clean_training_data, 100)
box_plot(clean_training_data$onroad_time, clean_training_data)
hist_plot(clean_training_data$onroad_time, clean_training_data, 30)

hist_plot_2 = function(column, data_frame, binsize){
  # column - string
  ggplot(aes_string(column), data = data_frame)+
    geom_histogram(fill = 'skyblue2', binwidth = binsize)+
    geom_density()+
    labs(x = gsub('\\.', ' ', column))+
    ggtitle(paste(" Histogram :",gsub('\\.', ' ', column)))
}

hist_plot_2('aerial_distance', clean_training_data, 100)
hist_plot_2('fare_amount', clean_training_data, 1)

bar_plot = function(cat_col){
  ggplot(clean_training_data, aes_string(x = factor(clean_training_data[,cat_col])))+
    geom_bar()
}
bar_plot('mnth')
bar_plot('year')
bar_plot('wday')
bar_plot('hour')

# ===============================================
# Finding Correlation & VIF & Chi-square Test
# ===============================================
# correlation plot for numerical feature
library('corrgram')
correl_mat = cor(x = clean_training_data, method = c("pearson", "kendall", "spearman"))
corrgram(clean_training_data[,numerical_cols], order = FALSE,
         upper.panel = panel.cor, text.panel = panel.txt,
         lower.panel = panel.pie, main = "Correlation Plot")
# Have to drop onroad_distance or aerial_distance
drop_num_cols = c('aerial_distance')

# Chi-Square test for categorical features
paired_features = combn(x = categorical_cols, m = 2, simplify = FALSE)
for(i in paired_features){
  print(i)
  feat1 = i[1]
  feat2 = i[2]
  chisq_res = chisq.test(table(clean_training_data[,feat1], clean_training_data[,feat2]))
  print(chisq_res$p.value)
}

#VIF of all numerical columns
install.packages('usdm')
library('usdm')
# With correlated column
vif(clean_training_data[,c(1,2,7,10,11,12,13)])
# Without correlated column
vif(clean_training_data[,c(2,10,11,12,13)])

drop_num_cols = c('aerial_distance')
drop_cat_cols = c('mnth','year','wday','hour')

#Factoring the categorical columns
clean_training_data[,categorical_cols] = lapply(clean_training_data[,categorical_cols], as.factor)

summary(clean_training_data)
str(clean_training_data)

# Dropping unwanted columns
drop_cols = c(drop_num_cols, drop_cat_cols)
clean_training_data = clean_training_data[, !names(clean_training_data) %in% drop_cols]
rownames(clean_training_data) = 1:nrow(clean_training_data)

clean_training_data_copy = clean_training_data

# =========================================================================================#
#
#                                   4. Making Models
#
# =========================================================================================#

# ================================================================================================================= #
# Preparing the Test Data
# ================================================================================================================= #
testing_data_full$passenger_count = as.integer(testing_data_full$passenger_count)
testing_data_full$pickup_datetime = as.POSIXlt(strptime(testing_data_full$pickup_datetime, "%Y-%m-%d %H:%M:%S"))
clean_testing_data = remove_na_rows(testing_data_full)
clean_testing_data = remove_0s_rows(testing_data_full)
clean_testing_data$year = clean_testing_data$pickup_datetime$year + 1900
clean_testing_data$mnth = clean_testing_data$pickup_datetime$mon
clean_testing_data$hour = clean_testing_data$pickup_datetime$hour
clean_testing_data$wday = clean_testing_data$pickup_datetime$wday
clean_testing_data = clean_testing_data[-c(1)]
clean_testing_data$aerial_distance = distVincentyEllipsoid(clean_testing_data[,1:2], clean_testing_data[3:4])
clean_testing_data$weekday = lapply(clean_testing_data$wday, function(x) sapply(x, is_weekday))
#clean_testing_data$weekday = as.integer(clean_testing_data$weekday)
clean_testing_data$weekday = as.numeric(clean_testing_data$weekday)
clean_testing_data$nght_hr = lapply(clean_testing_data$hour, function(x) sapply(x, is_nght_hr))
#clean_testing_data$nght_hr = as.integer(clean_testing_data$nght_hr)
clean_testing_data$nght_hr = as.numeric(clean_testing_data$nght_hr)
clean_testing_data$onroad_distance = 0
clean_testing_data$onroad_time = 0
clean_testing_data$onroad_distance = as.integer(clean_testing_data$onroad_distance)
clean_testing_data$onroad_time = as.integer(clean_testing_data$onroad_time)

####################################################################################################################################
#========================= DO NOT RUN THE BELOW CODE ON COMPLETE DATA FRAME: EXTREMELY TIME CONSUMING =============================#
####################################################################################################################################
# ================ For demo, uncomment and run on a small sample to check working or not =============================
sample_df = clean_testing_data[6700:6710,]
df = sample_df
# >>> onroad_distance & time are all 0s
summary(df)
# Fetching from DistanceMatrixAPIs
for (i in 1:nrow(df)) {
  origin_coord = paste(... = toString(df$pickup_latitude[i]),toString(df$pickup_longitude[i]), sep = '+')
  dest_coord = paste(... = toString(df$dropoff_latitude[i]),toString(df$dropoff_longitude[i]), sep = '+')
  api_resp = gmapsdistance(origin = origin_coord,
                           destination = dest_coord,
                           mode = 'driving',
                           key = gmaps_key)
  df$onroad_distance[i] = api_resp$Distance
  df$onroad_time[i] = api_resp$Time
}
# >>> onroad_distance & time are filled now
summary(df)
####################################################################################################################################
#========================= DO NOT RUN THE ABOVE CODE ON COMPLETE DATA FRAME: EXTREMELY TIME CONSUMING =============================#
####################################################################################################################################
gmaps_data_test = read.csv('gmaps_data_test.csv')
gmaps_data_test = gmaps_data_test[-c(1)]
# For verification - see nrow(clean_testing_data) == nrow(gmaps_data_test)
clean_testing_data$onroad_distance = gmaps_data_test$ord
clean_testing_data$onroad_time = gmaps_data_test$ort
clean_testing_data = clean_testing_data[-c(1,2,3,4)]
clean_testing_data$amnt_per_km = mean(clean_training_data$amnt_per_km)
clean_testing_data$amnt_per_hr = mean(clean_training_data$amnt_per_hr)
clean_testing_data = clean_testing_data[-c(2,3,4,5)]
clean_testing_data = clean_testing_data[-c(2)]
#clean_testing_data = clean_testing_data[-c(6,7)]
clean_testing_data$weekday = factor(clean_testing_data$weekday)
clean_testing_data$nght_hr = factor(clean_testing_data$nght_hr)

# ====================================== #
# Training data without any outliers
# ====================================== #
clean_training_data_iqr = clean_training_data
clean_training_data_iqr = remove_outliers_iqr(clean_training_data_iqr)
rownames(clean_training_data_iqr) = 1:nrow(clean_training_data_iqr)

# ------------- Randomizing the dataframe -------------#
clean_training_data = clean_training_data[sample(nrow(clean_training_data)), ]
rownames(clean_training_data) = 1:nrow(clean_training_data)
clean_training_data_iqr = clean_training_data_iqr[sample(nrow(clean_training_data_iqr)), ]
rownames(clean_training_data_iqr) = 1:nrow(clean_training_data_iqr)
clean_testing_data = clean_testing_data[sample(nrow(clean_testing_data)), ]
rownames(clean_testing_data) = 1:nrow(clean_testing_data)

X_train = clean_training_data
#y_train = clean_training_data[c(1)]
X_train_iqr = clean_training_data_iqr
#y_train_iqr = clean_training_data_iqr[c(1)]
# ======== Dropping amnt_per_km & amnt_per_hr ==========
#X_train = X_train[-c(7,8)]
#X_train_iqr = X_train_iqr[-c(7,8)]

X_test = clean_testing_data

# ================================================================ #
#                Function to perform training
# ================================================================ #
install.packages('caret')
library(readxl)
library(ggplot2)
install.packages('corrplot')
library(corrplot)
library('caret')
perform_learning = function(model, train_dataset, X_test = X_test, cv_partition){
  # model - string
  # train_dataset - dataframe having even the target variable
  # Method to fit the model onto the data
  # regression_fit = caret::train(fare_amount~., data = train_dataset, method = model)
  regression_fit = train(train_dataset[,-1], train_dataset[,1], method = model, preProcess = c("center", "scale"))
  # Finding Scores on Train Dataset
  y_train_pred = predict(regression_fit, train_dataset[,-1])
  #r2_score = R2(y_train_pred, train_dataset[,1])
  print("---------------- Regression Scores & Errors on Train Dataset --------------------")
  scores_reg = colMeans(regression_fit$results[c("RMSE", "MAE", "Rsquared")])
  print(scores_reg)
  
  # Performing Cross-Validation using Training Dataset
  # Dividing train_dataset into cv parts, stored index number of those parts in cv_parts
  cv_parts = createFolds(train_dataset$fare_amount, k=cv_partition)
  # Function to perform CV
  perform_cv = function(fold_of_dataframe){
    # Making training data by removing one fold
    training_data_fold = train_dataset[-fold_of_dataframe, ]
    # Making CV data by taking only that fold
    cv_data_fold = train_dataset[fold_of_dataframe, ]
    # Fitting model using training data fold
    regression_fit_cv = train(training_data_fold[,-1], training_data_fold[,1], method = model, preProcess = c("center","scale"))
    # Using trained model to predict cv_data_fold's output
    y_cv_pred = predict(regression_fit_cv, cv_data_fold[,-1])
    # Calculating r2 score using the predicted value of the cv_fold and actual value of the cv_fold
    scores_reg_cv = regression_fit_cv$results[c("RMSE", "MAE", "Rsquared")]
    return(scores_reg_cv)
  }
  
  # Performing CV operations on each fold
  cv_outputs = lapply(cv_parts, FUN = perform_cv)
  
  print("-----------------------Cross-Validation Scores Output-------------------------")
  print(cv_outputs)
  #print("MEAN of Cross-Validation Scores")
  #print(mean(as.numeric(cv_outputs)))
  
  y_test_pred = predict(regression_fit, X_test)
  return(y_test_pred)
}

# ============================================================================================= #
#
# If any error in library loading, just restart R under session menu & reload library('caret')  #
#
# ============================================================================================= #

# ===================================================== #
#               4.1 Linear Regression
# ===================================================== #
# With Outliers
y_test_pred_lr = perform_learning(model = 'lm', train_dataset = X_train, X_test = X_test, cv_partition = 5)

# Without Outliers
y_test_pred_lr_iqr = perform_learning(model = 'lm', train_dataset = X_train_iqr, X_test = X_test, cv_partition = 5)

# ===================================================== #
#               4.2 K-Nearest Neighbor
# ===================================================== #
# With Outliers
y_test_pred_knn = perform_learning(model = 'knn', train_dataset = X_train, X_test = X_test, cv_partition = 5)

# Without Outliers
y_test_pred_knn_iqr = perform_learning(model = 'knn', train_dataset = X_train_iqr, X_test = X_test, cv_partition = 5)

# ===================================================== #
#               4.3 Decision Tree
# ===================================================== #
# With Outliers
y_test_pred_dt = perform_learning(model = 'rpart2', train_dataset = X_train, X_test = X_test, cv_partition = 5)

# Without Outliers
y_test_pred_dt_iqr = perform_learning(model = 'rpart2', train_dataset = X_train_iqr, X_test = X_test, cv_partition = 5)

#install.packages(c("libcoin", "multcomp", "coin", "party"), lib="/home/shitbot009/anaconda3/envs/rstudio/lib/R/library")

# ===================================================== #
#               4.4 Random Forest
# ===================================================== #
# With Outliers
y_test_pred_rf = perform_learning(model = 'rf', train_dataset = X_train, X_test = X_test, cv_partition = 5)

# Without Outliers
y_test_pred_rf_iqr = perform_learning(model = 'rf', train_dataset = X_train_iqr, X_test = X_test, cv_partition = 5)

# ===================================================== #
#               4.5 XGBoost Regressor
# ===================================================== #
# Doesn't work fine with categorical data, so removing those columns
# With Outliers
y_test_pred_xgb = perform_learning(model = 'xgbTree', train_dataset = X_train[,-c(3,4)], X_test = X_test[,-c(2,3)], cv_partition = 5)

# Without Outliers
y_test_pred_xgb_iqr = perform_learning(model = 'xgbTree', train_dataset = X_train_iqr[,-c(3,4)], X_test = X_test[,-c(2,3)], cv_partition = 5) 