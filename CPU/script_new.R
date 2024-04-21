#install.packages('dplyr')
#install.packages('ggplot2')
#install.packages('corrplot')
#install.packages('stringrr')
#install.packages('naniar')
#install.packages('mice')
#install.packages('stringr')
#install.packages('kknn')

library('dplyr')
library('ggplot2')
library('corrplot')
library('stringr')
library('naniar')
library('mice')
library('stringr')
library('tidyr')
library('kknn')

df <- read.csv('Intel_CPUs.csv')

str(df)

df <- df %>% 
  replace_with_na_all(condition = ~.x %in% common_na_strings)

apply(is.na(df),2,sum) # Count the number of NA in each column
apply(is.na(df),2,mean) # Show the NA ratio
# omitting columns with all values are NA and NA ratio > 80% 
df <- df %>% 
  select(-Processor_Graphics_,
         -Graphics_Video_Max_Memory,
         -Support_4k,
         -Max_Resolution_HDMI,
         -Max_Resolution_DP,
         -Max_Resolution_eDP_Integrated_Flat_Panel,
         -DirectX_Support,
         -OpenGL_Support)

# omitting column with no use
df <- df %>%
  select(-Processor_Number #specify by producers, not contribute to the performance
  )

######### DATA MODIFY ##########

#### FUNCTIONS ####

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

mem_type_extract <- function(string) {
  if (is.na(string)) return (NA)
  ans <- ''
  if (grepl('LPDDR', string)) ans <- paste0(ans, 'Low_power')
  if (grepl('DDR\\dL', string)) ans <- paste(ans, 'Low_voltage')
  if (grepl('DDR', string)) ans <- paste(ans, 'Standard')
  return (ans)
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

cache_type_extract <- function(string){
  if (is.na(string)) return (NA)
  if (grepl("Last Level Cache",string)){
    type <- 'Last Level Cache'
    return(type)
  }
  else type <- str_extract(string, '\\b\\w+$')
  if (type == 'MB' || type == 'KB') return (NA)
  return(type)
}

bus_transfer_extract <- function(string){
  if (is.na(string) || string == 0 || string == '0  QPI' || string == '0 GT/s' || string == '0 GT/s QPI') return (NA)
  if (grepl("GT/s", string)){
    transfer <- str_extract(string, '\\d+ GT/s')
    return (transfer)
  }
  if (grepl("MHz", string)){
    transfer <- str_extract(string, '\\d+ MHz')
    return (transfer)
  }
  return(NA)
}

bus_type_extract <- function(string){
  if (is.na(string) || string == 0 || string == '0  QPI' || string == '0 GT/s' || string == '0 GT/s QPI') return (NA)
  type <- str_extract(string, '\\b\\w+$')
  if (type == 's') return (NA)
  return(type)
}

clean_base_frequency <- function(string) {
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
  
  # If the string contains "MB", divide the number by 1024
  if (grepl("MB", string)) {
    digits <- digits / 1024
  }
  
  return(digits)
}


clean_graphic_output <- function(string) {
  if (is.na(string)) return (NA)
  string <- str_trim(string)
  string <- gsub(",*\\s+|-", "/", string)
  if (string == 'LVDS/MIPI') 
    string <- 'MIPI/LVDS'
  return (string)
}

resolution_extract <- function(string) {
  resolution <- str_extract(string, '\\d{4}x\\d{4}')
  return (resolution)
}

resolution_freq_extract <- function(string) {
  freq <- str_extract(string, '\\d+Hz')
  return (freq)  
}

latest_pci <- function(string) {
  if (is.na(string)) return (NA)
  if (string == 'No' || string == 'None') return (0)
  if (grepl('1.0', string)) return (1)
  if (grepl('3.0', string)) return (3)
  return (2)
}

latest_directX_version <- function(string) {
  if (is.na(string)) return (NA)
  if (string == 'Yes') return (NA) # for later df fill (ex: mode filled)
  if (grepl('12', string)) return ('12')
  if (grepl('\\d+.\\d', string)) return (str_extract(string, '\\d+.\\d'))
  
}
temperature_extract <- function(string) {
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
  if (is.na(string)) return (NA)
  config <- unlist(str_extract_all(string, '\\d*x\\d+'))
  if (!length(config)) return (NA)
  config <- ifelse(grepl('\\bx\\d+', config), paste0(1, config), config)
  sorted_indices <- custom_sort(config)
  config <- config[sorted_indices]
  return (paste(config, collapse=' '))
}

#### APPLY ####

product_collect <- c('Legacy', 'Celeron', 'Pentium', 'Quark', 'Atom', 'Itanium', 'Xeon','Core')
for (i in product_collect) {
  df$Product_Collection <- ifelse(grepl(i, df$Product_Collection), i, df$Product_Collection)
}

df$Launch_Date <- sapply(df$Launch_Date, convert_year)

df$Lithography <-gsub(" nm", '', df$Lithography)
df$Lithography <- as.numeric(df$Lithography)

df$Recommended_Customer_Price <- sapply(df$Recommended_Customer_Price, convert_currency_to_numeric)

df$Processor_Base_Frequency <- sapply(df$Processor_Base_Frequency, clean_base_frequency)
df$Processor_Base_Frequency <- gsub(" GHz", '', df$Processor_Base_Frequency)
df$Processor_Base_Frequency <- as.numeric(df$Processor_Base_Frequency)

df$Max_Turbo_Frequency <- gsub(" GHz", '', df$Max_Turbo_Frequency)
df$Max_Turbo_Frequency <- as.numeric(df$Max_Turbo_Frequency)

#Cache
df['Cache_size'] <- sapply(df$Cache, cache_size_extract)
df['Cache_type'] <- sapply(df$Cache, cache_type_extract)
df$Cache_size <- sapply(df$Cache_size, clean_cache_size)
df$Cache_size <- gsub(" MB", '', df$Cache_size)
df$Cache_size <- as.numeric(df$Cache_size)
df <- df %>% select(-Cache)

# Bus_Speed
df['Bus_Transfer_or_Clock'] <- sapply(df$Bus_Speed, bus_transfer_extract)
df['Bus_Type'] <- sapply(df$Bus_Speed, bus_type_extract)
df <- df %>% select(-Bus_Speed)

df$TDP <- gsub(" W", '', df$TDP)
df$TDP <- as.numeric(df$TDP)

df <- boolean_convert(df, 'Embedded_Options_Available')
df <- boolean_convert(df, 'Conflict_Free')

df$Max_Memory_Size <- sapply(df$Max_Memory_Size, clean_max_memory_size)
df$Max_Memory_Size <- gsub(" GB", '', df$Max_Memory_Size)
df$Max_Memory_Size <- as.numeric(df$Max_Memory_Size)

df$Memory_Types <- sapply(df$Memory_Types, mem_type_extract)
head(df)

df$Max_Memory_Bandwidth <- gsub(" GB/s", '', df$Max_Memory_Bandwidth)
df$Max_Memory_Bandwidth <- as.numeric(df$Max_Memory_Bandwidth)

df <- boolean_convert(df, 'ECC_Memory_Supported')

df$Graphics_Base_Frequency <-gsub(" GHz", " MHz", df$Graphics_Base_Frequency)
df$Graphics_Base_Frequency <- sapply(df$Graphics_Base_Frequency, clean_base_frequency)
df$Graphics_Base_Frequency <- as.numeric(df$Graphics_Base_Frequency)

df$Graphics_Max_Dynamic_Frequency <- sapply(df$Graphics_Max_Dynamic_Frequency, clean_base_frequency)
df$Graphics_Max_Dynamic_Frequency <-gsub(" GHz", '', df$Graphics_Max_Dynamic_Frequency)
df$Graphics_Max_Dynamic_Frequency <- as.numeric(df$Graphics_Max_Dynamic_Frequency)

#df$Graphics_Video_Max_Memory <- sapply(df$Graphics_Video_Max_Memory, clean_graphics_video)

df <- boolean_convert(df, 'Intel_Hyper_Threading_Technology_')
df <- boolean_convert(df, 'Intel_Virtualization_Technology_VTx_')
df <- boolean_convert(df, 'Intel_64_')

df$Instruction_Set <- factor(ifelse(df$Instruction_Set == "", NA, df$Instruction_Set), 
                             levels=c('Itanium 64-bit', '64-bit', '32-bit'))

df <- boolean_convert(df, 'Idle_States')
df <- boolean_convert(df, 'Thermal_Monitoring_Technologies')
df <- boolean_convert(df, 'Secure_Key')
df <- boolean_convert(df, 'Execute_Disable_Bit')

df$Graphics_Output <- factor(sapply(df$Graphics_Output, clean_graphic_output))

df['SSE_FPU'] <- str_detect(df$Instruction_Set_Extensions, 'SSE|AVX|Yes')
df['AES'] <- str_detect(df$Instruction_Set_Extensions, 'AES')
df['MMX'] <- str_detect(df$Instruction_Set_Extensions, 'MMX')

df <- df %>% select(-Instruction_Set_Extensions)

#df['HDMI_resolution'] <- factor(sapply(df$Max_Resolution_HDMI, resolution_extract))
#df['HDMI_frequency'] <- factor(sapply(df$Max_Resolution_HDMI, resolution_freq_extract))

#df['DP_resolution'] <- factor(sapply(df$Max_Resolution_DP, resolution_extract))
#df['DP_frequency'] <- factor(sapply(df$Max_Resolution_DP, resolution_freq_extract))

#df['eDP_resolution'] <- factor(sapply(df$Max_Resolution_eDP_Integrated_Flat_Panel, resolution_extract))
#df['eDP_frequency'] <- factor(sapply(df$Max_Resolution_eDP_Integrated_Flat_Panel, resolution_freq_extract))

df$PCI_Express_Revision <- factor(sapply(df$PCI_Express_Revision, latest_pci), levels=c(3, 2, 1, 0))

df$T <- sapply(df$T, temperature_extract)

df$PCI_Express_Configurations_ <- factor(sapply(df$PCI_Express_Configurations_, configuration_extract))

#df$DirectX_Support <- factor(sapply(df$DirectX_Support, latest_directX_version))

get_mode <- function(x) {
  ux <- unique(x)
  freq <- tabulate(match(x, ux))
  max_freq_index <- which.max(freq)
  mode_val <- ux[max_freq_index]
  if (is.na(mode_val)) {
    non_na_freq <- freq[!is.na(ux)]
    non_na_index <- which.max(non_na_freq)
    mode_val <- ux[non_na_index]
  }
  return(mode_val)
}

# fill_na <- function(df) {
#   for (col in names(df)) {
#     if (is.numeric(df[[col]])) {
#       median_val <- median(df[[col]], na.rm = TRUE)
#       df[[col]][is.na(df[[col]])] <- median_val
#     } else {
#       mode_val <- as.character(get_mode(df[[col]]))
#       #cat(col, " ", mode_val, "\n")
#       df[[col]][is.na(df[[col]])] <- mode_val
#     }
#   }
#   return(df)
# }
# options(dplyr.width = Inf)
# df <- fill_na(df)
# apply(is.na(df),2,mean)

impute <- function(data, col) {
  # Check if the specified column contains missing values
  if (any(is.na(data[[col]]))) {
    if (is.numeric(data[[col]])) {
      # If the column is numeric, impute using KNN
      train <- data[!is.na(data[[col]]), ]
      test <- data[is.na(data[[col]]), ]
      
      if (nrow(test) > 0) {
        model <- kknn(as.formula(paste(col, "~ .")), train, test, k = 5) 
        imputed_values <- fitted(model)
        data[is.na(data[[col]]), col] <- imputed_values
      } else {
        print("No missing values to impute.")
      }
    } else {
      # If the column is categorical, impute using the mode
      mode_val <- as.character(get_mode(data[[col]]))
      data[[col]][is.na(data[[col]])] <- mode_val
    }
  } else {
    print("Column has no missing values.")
  }
  return(data)
}

for (col in names(df)){
  df <- impute(df, col)
}

# Select 80% data as the train set and 20% as test set
s <-sample(seq_len(nrow(df)), size = floor(0.8*nrow(df)))
train <-df[s, ]
test <-df[-s, ]