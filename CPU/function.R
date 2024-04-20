install.packages('mice')
install.packages('stringr')
library('mice')
library('stringr')


impute <- function(data, col) {
  # Check if the specified column contains missing values
  if (any(is.na(data[[col]]))) {
    # If the column is categorical, impute using the mode
    if (is.factor(data[[col]])) {
      mode_val <- names(sort(table(data[[col]]), decreasing = TRUE))[1]
      data[is.na(data[[col]]), col] <- mode_val
    } else {
      # If the column is numeric, impute using KNN
      train <- data[!is.na(data[[col]]), ]
      test <- data[is.na(data[[col]]), ]
      
      if (nrow(test) > 0) {
        model <- kknn::train.kknn(as.formula(paste(col, "~ .")), train, test, k = 5) 
        imputed_values <- fitted(model)
        data[is.na(data[[col]]), col] <- imputed_values
      } else {
        print("No missing values to impute.")
      }
    }
  } else {
    print("Column has no missing values.")
  }
  return(data)
}


boolean_convert <- function(data, col) {
  data[[col]] <- factor(case_when(
    data[[col]] == 'Yes' ~ TRUE,
    data[[col]] == 'No' ~ FALSE,
    TRUE ~ NA
  ), levels=c(TRUE, FALSE, NA))
  return (data)
}

clean_graphics_base_frequency <- function(string) {
  # Extract the first 3 digits using regular expression
  digits <- as.numeric(substr(string, 1, nchar(string)-4))
  
  # If the string contains "MHz", divide the number by 1000
  if (grepl("MHz", string)) {
    digits <- digits / 1000
  }
  
  return(digits)
}


clean_graphics_video <- function(string) {
  # Extract the first 3 digits using regular expression
  digits <- as.numeric(substr(string, 1, nchar(string)-3))
  
  # If the string contains "MHz", divide the number by 1000
  if (grepl("MB", string)) {
    digits <- digits / 1024
  }
  
  return(digits)
}


clean_graphic_output <- function(string) {
  if (string == 'N/A' || string == '') return (NA)
  string <- str_trim(string)
  string <- gsub(",*\\s+|-", "/", string)
  if (string == 'LVDS/MIPI') 
    string <- 'MIPI/LVDS'
  return (string)
}

resolution_extract <- function(string) {
  if (string == 'N/A' || string == '') return (NA)
  resolution <- str_extract(string, '\\d{4}x\\d{4}')
  return (resolution)
}

resolution_freq_extract <- function(string) {
  if (string == 'N/A' || string == '') return (NA)
  freq <- str_extract(string, '\\d+Hz')
  return (freq)  
}

latest_pci <- function(string) {
  if (string == 'N/A' || string == '') return (NA)
  if (string == 'No' || string == 'None') return (0)
  if (grepl('1.0', string)) return (1)
  if (grepl('3.0', string)) return (3)
  return (2)
}

latest_directX_version <- function(string) {
  if (string == 'N/A' || string == '') return (NA)
  if (string == 'Yes') return (NA) # for later data fill (ex: mode filled)
  if (grepl('12', string)) return ('12')
  if (grepl('\\d+.\\d', string)) return (str_extract(string, '\\d+.\\d'))
  
}
temperature_extract <- function(string) {
  if (string == 'N/A' || string == '') return (NA)
  temp <- as.numeric(unlist((str_extract_all(string, '\\d+\\.*\\d*'))))
  if (length(temp) == 2) return ((temp[2] + temp[1]) / 2)
  return (temp)
}

custom_sort <- function(list) {
  parts <- strsplit(list, "x")
  
  first_numbers <- sapply(parts, function(part) as.numeric(part[1]))
  second_numbers <- sapply(parts, function(part) as.numeric(part[2]))
  
  order(first_numbers, second_numbers)
}

configuration_extract <- function(string) {
  if (is.na(string) || string == 'N/A' || string == '') return (NA)
  config <- unlist(str_extract_all(string, '\\d*x\\d+'))
  if (!length(config)) return (NA)
  config <- ifelse(grepl('\\bx\\d+', config), paste0(1, config), config)
  sorted_indices <- custom_sort(config)
  config <- config[sorted_indices]
  return (paste(config, collapse=' '))
}


