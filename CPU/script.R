install.packages('dplyr')
install.packages('ggplot2')
install.packages('corrplot')
install.packages('stringrr')

library('dplyr')
library('ggplot2')
library('corrplot')
library('stringr')

data <- read.csv('Intel_CPUs.csv')
summary(data)

str(data)

count_na <- function(col) {
  sum(is.na(col))
}

# Apply the count_na function to each column of the data frame
na_counts <- sapply(data, count_na)
print(na_counts)

# omit columns with all NA
data <- data %>% 
  select(-Processor_Graphics_, 
         -OpenGL_Support, 
         -Support_4k)

# omit column with no use
data <- data %>%
  select(-Processor_Number #specify by producers, not contribute to the performance
         )
sapply(data, unique)

######### DATA MODIFY ##########

# PRODUCT COLLECTION 
product_collect <- c('Legacy', 'Celeron', 'Pentium', 'Quark', 'Atom', 'Itanium', 'Xeon','Core')
for (i in product_collect) {
  data$Product_Collection <- ifelse(grepl(i, data$Product_Collection), i, data$Product_Collection)
}

### LAUNCH DATE -> YEAR
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
data$Launch_Date <- sapply(data$Launch_Date, convert_year)


### LITHOGRAPHY
convert_lithography <- function(string) {
  if (grepl("\\b\\d{2}\\b", string)) {
    numbers <- as.numeric(sub("(\\d{2}).*", "\\1", string))
    return(numbers)
  } else {
    return(NA)
  }
}
data$Lithography <- sapply(data$Lithography, convert_lithography)


### RECOMMENDED CUSTOMER PRICE
extract_number <- function(string) {

  if (grepl("-", string)) {
    # If the string contains a hyphen, extract both numbers and calculate the average
    numbers <- as.numeric(unlist(str_extract_all(string, "\\$\\d+\\.\\d+")))
    avg_number <- mean(numbers)
    return(avg_number)
  } else {
    # If the string doesn't contain a hyphen, extract the single number
    number <- as.numeric(str_extract(string, "\\$\\d+\\.\\d+"))
    return(number)
  }
}

data$Recommended_Customer_Price <- sapply(data$Recommended_Customer_Price, extract_number)


################################
print(sapply(data,unique))

# select categorical data
Categorical <- data %>%
  select(Product_Collection,
         Vertical_Segment,
         Status)
