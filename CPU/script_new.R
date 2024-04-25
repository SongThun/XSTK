#install.packages('dplyr')
#install.packages('ggplot2')
#install.packages('corrplot')
#install.packages('stringrr')
#install.packages('naniar')
#install.packages('mice')
#install.packages('stringr')
#install.packages('kknn')
#install.packages('car')
#install.packages('ggpubr')
#install.packages('nortest')
#install.packages('caret')
#install.packages("randomForest")
#install.packages("missForest")
#install.packages("VIM")
#install.packages("FNN")


library('ggpubr')
library('car')
library('dplyr')
library('ggplot2')
library('corrplot')
library('stringr')
library('naniar')
library('mice')
library('stringr')
library('tidyr')
library('kknn')
library('nortest')
library('caret')
library('randomForest')
library('missForest')
library('VIM')
library('FNN')

initial_df <- read.csv('Intel_CPUs.csv', na.strings = c('N/A', ''))
str(initial_df)

# Selecting the important columns
df <- initial_df[, c("Product_Collection","Vertical_Segment", "Status", "Launch_Date", 
                    "Lithography", "Recommended_Customer_Price","nb_of_Cores", "nb_of_Threads","Processor_Base_Frequency",
                    "Cache", "TDP", "Max_Memory_Size", "Max_Memory_Bandwidth", "Graphics_Base_Frequency", 
                    "Intel_Hyper_Threading_Technology_", "Intel_Virtualization_Technology_VTx_", "Instruction_Set")]

######### DATA MODIFY ##########

#### FUNCTIONS ####

# Converting "Q4'15" -> "2015", "Q1'99" -> "1999"
convert_year <- function(string) {
  if (!is.na(string) && grepl("'\\d{2}", string)) {
    year <- sub(".*'(\\d{2})", "\\1", string)
    
    year <- as.numeric(year)
    if (year >= 0 & year <= 21) {
      year <- 2000 + year
    } else {
      year <- 1900 + year
    }
    return(year)
  } else {
    return(NA) 
  }
}

convert_currency_to_numeric <- function(currency_string) {
  # Remove dollar sign, commas, and split the string by '-'
  numeric_strings <- strsplit(gsub("[$,]", "", currency_string), " - ")[[1]]
  # Convert strings to numeric
  numeric_values <- as.numeric(numeric_strings)
  # If it's a range, take the mean, otherwise return the value
  if (length(numeric_values) > 1) {
    return(mean(numeric_values))
  } else {
    return(numeric_values)
  }
}

clean_max_memory_size <- function(string){
  digits <- as.numeric(substr(string, 1, nchar(string)-3))
  if (grepl("TB", string)) {
    digits <- digits * 1024
  }
  
  return(digits)
}

boolean_convert <- function(df, col) {
  df[[col]] <- factor(case_when(
    df[[col]] == 'Yes' ~ TRUE,
    df[[col]] == 'No' ~ FALSE,
    TRUE ~ NA
  ), levels=c(TRUE, FALSE, NA))
  return (df)
}

cache_size_extract <- function(string){
  if (is.na(string)) return (NA)
  if (grepl("MB", string)){
    size <- str_extract(string, '\\d+ MB')
    return (size)
  }
  if (grepl("KB", string)){
    size <- str_extract(string, '\\d+ KB')
    return (size)
  }
  return(NA)
}

clean_cache_size <- function(string){
  digits <- as.numeric(substr(string, 1, nchar(string)-3))
  if (grepl("KB", string)) {
    digits <- digits / 1024
  }
  return(digits)
}

clean_base_frequency <- function(string) {
  # Extract the first 3 digits using regular expression
  digits <- as.numeric(substr(string, 1, nchar(string)-4))
  
  # If the string contains "GHz", multiply the number by 1000
  if (grepl("GHz", string)) {
    digits <- digits * 1000
  }
  
  return(digits)
}

remove_outliers <- function(df, col) {
  quartiles <- quantile(df[[col]], probs=c(.25, .75), na.rm = FALSE)
  IQR_val <- IQR(df[[col]], na.rm = FALSE)

  Lower <- quartiles[1] - 1.5*IQR_val
  Upper <- quartiles[2] + 1.5*IQR_val 

  cleaned_df <- subset(df, df[[col]] > Lower & df[[col]] < Upper)
  
  return(cleaned_df)
}

#### APPLY ####

product_collect <- c('Legacy', 'Celeron', 'Pentium', 'Quark', 'Atom', 'Itanium', 'Xeon','Core')
for (i in product_collect) {
  df$Product_Collection <- ifelse(grepl(i, df$Product_Collection), i, df$Product_Collection)
}

df$Launch_Date <- sapply(df$Launch_Date, convert_year, USE.NAMES = FALSE)

df$Lithography <-gsub(" nm", '', df$Lithography)
df$Lithography <- as.numeric(df$Lithography)

df$Recommended_Customer_Price <- sapply(df$Recommended_Customer_Price, convert_currency_to_numeric, USE.NAMES = FALSE)

df$Processor_Base_Frequency <- sapply(df$Processor_Base_Frequency, clean_base_frequency)
df$Processor_Base_Frequency <- gsub(" MHz", '', df$Processor_Base_Frequency)
df$Processor_Base_Frequency <- as.numeric(df$Processor_Base_Frequency)

df$Cache <- sapply(df$Cache, cache_size_extract)
df$Cache <- sapply(df$Cache, clean_cache_size)
df$Cache <- gsub(" MB", '', df$Cache)
df$Cache <- as.numeric(df$Cache)

df$TDP <- gsub(" W", '', df$TDP)
df$TDP <- as.numeric(df$TDP)

df$Max_Memory_Size <- sapply(df$Max_Memory_Size, clean_max_memory_size)
df$Max_Memory_Size <- gsub(" GB", '', df$Max_Memory_Size)
df$Max_Memory_Size <- as.numeric(df$Max_Memory_Size)

df$Max_Memory_Bandwidth <- gsub(" GB/s", '', df$Max_Memory_Bandwidth)
df$Max_Memory_Bandwidth <- as.numeric(df$Max_Memory_Bandwidth)

df$Graphics_Base_Frequency <- sapply(df$Graphics_Base_Frequency, clean_base_frequency)
df$Graphics_Base_Frequency <-gsub(" MHz", '', df$Graphics_Base_Frequency)
df$Graphics_Base_Frequency <- as.numeric(df$Graphics_Base_Frequency)

df <- boolean_convert(df, 'Intel_Hyper_Threading_Technology_')
df <- boolean_convert(df, 'Intel_Virtualization_Technology_VTx_')

df$Instruction_Set <-gsub("-bit", '', df$Instruction_Set)
df$Instruction_Set <-gsub("Itanium ", '', df$Instruction_Set)
df$Instruction_Set <- as.numeric(df$Instruction_Set)

str(df)

#### FILLING OUT NA VALUES ####
apply(is.na(df),2,sum) # Count the number of NA in each column
apply(is.na(df),2,mean) # Show the NA ratio

numeric_columns <- sapply(df, is.numeric)
numeric_data <- df[, numeric_columns]
categorical_data <- df[, !numeric_columns]

# Using Random Forest to impute missing numerical values
imputed_rf <- missForest(numeric_data)

# Convert the imputed data back to a dataframe
imputed_df <- data.frame(imputed_rf$ximp)
str(imputed_df)

# Fill the imputed values back into the original dataframe
for (col in names(imputed_df)) {
  df[is.na(df[[col]]), col] <- imputed_df[is.na(df[[col]]), col]
}

# Using K-nearest neighbor to impute missing categorical values
imputed_knn <- kNN(categorical_data)
imputed_knn <- subset(imputed_knn, select = Product_Collection:Intel_Virtualization_Technology_VTx_)

for (col in names(imputed_knn)) {
  df[is.na(df[[col]]), col] <- imputed_knn[is.na(df[[col]]), col]
}

apply(is.na(df),2,mean)

# Box plot for categorical columns
box1<-boxplot(df$Recommended_Customer_Price~df$Product_Collection, xlab = "Product Collection", ylab = "Recommended Customer Price")$stats
abline(lm(Recommended_Customer_Price~Product_Collection,data=df),col='blue')
box2<-boxplot(df$Recommended_Customer_Price~df$Vertical_Segment, xlab = "Vertical Segment", ylab = "Recommended Customer Price")$stats
abline(lm(Recommended_Customer_Price~Vertical_Segment,data=df),col='blue')
box3<-boxplot(df$Recommended_Customer_Price~df$Status, xlab = "Status", ylab = "Recommended Customer Price")$stats
abline(lm(Recommended_Customer_Price~Status,data=df),col='blue')
box4<-boxplot(df$Recommended_Customer_Price~df$Intel_Hyper_Threading_Technology_, xlab = "Intel Hyper Threading Technology", ylab = "Recommended Customer Price")$stats
abline(lm(Recommended_Customer_Price~Intel_Hyper_Threading_Technology_,data=df),col='blue')
box5<-boxplot(df$Recommended_Customer_Price~df$Intel_Virtualization_Technology_VTx_, xlab = "Intel Virtualization Technology VTx", ylab = "Recommended Customer Price")$stats
abline(lm(Recommended_Customer_Price~Intel_Virtualization_Technology_VTx_,data=df),col='blue')


# Scatter plot for numerical columns
for (col in names(numeric_data)){
  plot (df[[col]], df$Recommended_Customer_Price, xlab =col, ylab = "Recommended_Customer_Price")
  abline(lm(Recommended_Customer_Price~df[[col]],data=df),col='blue')
}

# Correlation plot for the numeric columns
numeric_data <- select_if(df, is.numeric)
cor_matrix <- cor(numeric_data)
corrplot(cor_matrix,
         method = "pie",
         tl.cex = 0.8,           
         addCoefasPercent = TRUE, 
         number.cex = 0.8,      
         mar = c(0, 0, 0, 0),    
         cl.pos = "r",            
         cl.ratio = 0.2,          
         cl.offset = 1.3,)   
cor_matrix

# Shapiro-Wilk test
shapiro.test(df$Recommended_Customer_Price)

# Select 80% data as the train set and 20% as test set
set.seed(5) # Make data reproducible
s <-sample(seq_len(nrow(df)), size = floor(0.8*nrow(df)))
train <-df[s, ]
test <-df[-s, ]

### Linear regression
model <- lm(Recommended_Customer_Price ~ ., data = train)
summary(model)

# Comparing predicted values with test values
pred_values <- data.frame(predict(model, newdata = test))
compare <- cbind(test$Recommended_Customer_Price, pred_values)

MAPE <- mean(abs((compare[,1] - compare[,2]) / compare[,1])) * 100
cat("Mean Absolute Percentage Error (MAPE):", MAPE, "%\n")

SSE <- sum((df$Recommended_Customer_Price - pred_values)^2)
SST <- sum((df$Recommended_Customer_Price - mean(df$Recommended_Customer_Price))^2)
cat("The coefficient of determination of the model on test set: " , round((1 - SSE / SST )* 100 ,2) , "%" )

### Step-wise function
step_model <- step(model, direction='both')
summary(step_model)

# Comparing predicted values with test values
pred_values <- data.frame(predict(step_model, newdata = test))
compare <- cbind(test$Recommended_Customer_Price, pred_values)

MAPE <- mean(abs((compare[,1] - compare[,2]) / compare[,1])) * 100
cat("Mean Absolute Percentage Error (MAPE):", MAPE, "%\n")

SSE <- sum((df$Recommended_Customer_Price - pred_values)^2)
SST <- sum((df$Recommended_Customer_Price - mean(df$Recommended_Customer_Price))^2)
cat("The coefficient of determination of the step_model on test set: " , round((1 - SSE / SST )* 100 ,2) , "%" )

# Polynomial regression
poly2 <- lm(Recommended_Customer_Price ~ poly(Launch_Date, 2) + poly(Lithography, 2) + poly(nb_of_Cores, 2) +
              poly(nb_of_Threads, 2) + poly(Processor_Base_Frequency, 2) + poly(Cache, 2) + poly(TDP, 2) + 
              poly(Max_Memory_Size, 2) + poly(Max_Memory_Bandwidth, 2) + poly(Graphics_Base_Frequency, 2) +
              poly(Instruction_Set, 2), data = df)
summary(poly2)

# Comparing predicted values with test values
pred_values <- data.frame(predict(poly2, newdata = test))
compare <- cbind(test$Recommended_Customer_Price, pred_values)

MAPE <- mean(abs((compare[,1] - compare[,2]) / compare[,1])) * 100
cat("Mean Absolute Percentage Error (MAPE):", MAPE, "%\n")

SSE <- sum((df$Recommended_Customer_Price - pred_values)^2)
SST <- sum((df$Recommended_Customer_Price - mean(df$Recommended_Customer_Price))^2)
cat("The coefficient of determination of the step_model on test set: " , round((1 - SSE / SST )* 100 ,2) , "%" )

poly3 <- lm(Recommended_Customer_Price ~ poly(Launch_Date, 3) + poly(Lithography, 3) + poly(nb_of_Cores, 3) + 
              poly(nb_of_Threads, 3) + poly(Processor_Base_Frequency, 3) + poly(TDP, 3) + poly(Cache, 3) +
              poly(Max_Memory_Size, 3) + poly(Max_Memory_Bandwidth, 3) + poly(Graphics_Base_Frequency, 3) +
              poly(Instruction_Set, 3), data = df)
summary(poly3)

# Comparing predicted values with test values
pred_values <- data.frame(predict(poly3, newdata = test))
compare <- cbind(test$Recommended_Customer_Price, pred_values)

MAPE <- mean(abs((compare[,1] - compare[,2]) / compare[,1])) * 100
cat("Mean Absolute Percentage Error (MAPE):", MAPE, "%\n")

SSE <- sum((df$Recommended_Customer_Price - pred_values)^2)
SST <- sum((df$Recommended_Customer_Price - mean(df$Recommended_Customer_Price))^2)
cat("The coefficient of determination of the step_model on test set: " , round((1 - SSE / SST )* 100 ,2) , "%" )

poly4 <- lm(Recommended_Customer_Price ~ poly(Launch_Date, 4) + poly(Lithography, 4) + poly(nb_of_Cores, 4) + 
              poly(nb_of_Threads, 4) + poly(Processor_Base_Frequency, 4) + poly(TDP, 4) + poly(Cache, 4) +
              poly(Max_Memory_Size, 4) + poly(Max_Memory_Bandwidth, 4) + poly(Graphics_Base_Frequency, 4) +
              poly(Instruction_Set, 4), data = df)
summary(poly4)

# Comparing predicted values with test values
pred_values <- data.frame(predict(poly4, newdata = test))
compare <- cbind(test$Recommended_Customer_Price, pred_values)

MAPE <- mean(abs((compare[,1] - compare[,2]) / compare[,1])) * 100
cat("Mean Absolute Percentage Error (MAPE):", MAPE, "%\n")

SSE <- sum((df$Recommended_Customer_Price - pred_values)^2)
SST <- sum((df$Recommended_Customer_Price - mean(df$Recommended_Customer_Price))^2)
cat("The coefficient of determination of the step_model on test set: " , round((1 - SSE / SST )* 100 ,2) , "%" )