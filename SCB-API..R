# Install necessary packages 
install.packages("pxweb")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")

install.packages("ggplot2")
library(ggplot2)

# Load the packages
library(pxweb)
library(ggplot2)
library(dplyr)
library(tidyr)

install.packages("ggplot2")
install.packages("ggplot2", type = "binary")

# Load necessary libraries
library(httr)
library(jsonlite)
library(ggplot2)
library(dplyr)

# Set the path to the JSON query file
json_file_path <- "C:/Users/theop/Downloads/000001EQ_20240427-191709.json"

# Read the JSON query from the file
json_query <- fromJSON(json_file_path, simplifyDataFrame = TRUE)

# Define the API URL
api_url <- "https://www.statistikdatabasen.scb.se/sq/148839"

# Make the POST request to the SCB API using the JSON query
response <- POST(api_url, body = toJSON(json_query), encode = "json", content_type("application/json"))

# Check the response status
if (status_code(response) == 200) {
  # Extract data from the response
  data <- content(response, type = "parsed")
  
# Assuming the data structure contains 'data' which needs to be processed into a DataFrame
  df <- data.frame(year = sapply(data$data, function(x) x$Tid),
                   number_of_cars = sapply(data$data, function(x) x$Value))  
# Save the DataFrame to CSV
write.csv(df, "new_car_registrations.csv", row.names = FALSE)

library(httr)
library(jsonlite)
library(ggplot2)
library(dplyr)

# Set the path to the JSON query file and read it
json_file_path <- "C:/Users/theop/Downloads/000001EQ_20240427-191709.json"
json_query <- fromJSON(json_file_path, simplifyDataFrame = TRUE)  

# API URL
api_url <- "https://www.statistikdatabasen.scb.se/sq/148839"

# Make the POST request
response <- POST(api_url, body = toJSON(json_query), encode = "json", content_type("application/json"))
  
# Check the response status
if (status_code(response) == 200) {
  data <- content(response, type = "parsed")
  # Assuming 'data' contains the correct structure you need for your DataFrame
  df <- data.frame(year = sapply(data$data, function(x) x$Tid),
                   number_of_cars = sapply(data$data, function(x) x$Value))  
  
# Make the POST request
response <- POST(api_url, body = toJSON(json_query), encode = "json", content_type("application/json"))  

# Check the response status
if (status_code(response) == 200) {
  # Try to print raw content for inspection
  raw_data <- content(response, type = "text", encoding = "UTF-8")
  print("Raw data received:")
  print(raw_data)

  #parse JSON from raw data
  data <- fromJSON(raw_data, simplifyDataFrame = FALSE)
  print("Structured data:")
  print(data)
  
  # Check if data is as expected and attempt to process it
  if (is.list(data) && "data" %in% names(data)) {
    df <- data.frame(year = sapply(data$data, `[`, "Tid"),
                     number_of_cars = sapply(data$data, `[`, "Value"))
  


    # Plotting
    ggplot(df, aes(x = year, y = number_of_cars, group = 1)) +
      geom_line() +
      labs(title = "New Car Registrations in Sweden (2018-2023)", x = "Year", y = "Number of New Registrations") +
      theme_minimal()
  } else {
    print("Data structure is not as expected.")
  }
} else {
  print(paste("Failed to fetch data. Status Code:", status_code(response)))
  print("Error Message:")
  response_content <- content(response, "text", encoding = "UTF-8")
  print(response_content)
} 

library(jsonlite)
library(ggplot2)
library(dplyr)
  
# Given JSON data already in 'data'
data <- fromJSON("C:/Users/theop/Downloads/000001EQ_20240427-191709.json")

# Assuming 'data$value' contains the actual data and 'data$dimension$Tid$category$label' contains the labels for each time period
values <- data$value
time_labels <- names(data$dimension$Tid$category$label)
  
  
# Create DataFrame
df <- data.frame(
  Month = time_labels,
  Registrations = values
)  

# Convert Month to Date format if needed
df$Month <- as.Date(df$Month, format="%YM%m")
  
# Check the structure and head of the DataFrame
str(df)
head(df)

# Convert Month from format 'YYYYMM' to Date
df$Month <- as.Date(paste0(substr(df$Month, 1, 4), "-", substr(df$Month, 6, 7), "-01"), "%Y-%m-%d")

# Verify the conversion
head(df)

sum(is.na(df$Registrations))  # Count NA values
sum(!is.finite(df$Registrations))  # Count non-finite values

ggplot(df, aes(x = Month, y = Registrations)) +
  geom_line() +
  labs(title = "New Car Registrations in Sweden by Month", x = "Month", y = "Number of Registrations") +
  ylim(c(0, max(df$Registrations, na.rm = TRUE))) +  # Set y-axis limits from 0 to max of Registrations
  theme_minimal()



library(dplyr)
library(ggplot2)

# Assuming 'time_labels' contains date labels like '2023M01', '2023M02', etc.
# Example data frame creation (replace this with your actual data frame code)
df <- data.frame(
  Month = c("2018M04", "2018M05", "2018M06", "2018M07", "2018M08", "2018M09"),
  Registrations = c(23815, 27815, 37957, 35601, 39446, 67553)
)

# Convert Month from format 'YYYYMmm' to Date
df$Month <- as.Date(paste0(substr(df$Month, 1, 4), "-", substr(df$Month, 6, 7), "-01"), "%Y-%m-%d")

# Verify the conversion
str(df)
head(df)

# Plotting the data, assuming the conversion is now correct
ggplot(df, aes(x = Month, y = Registrations)) +
  geom_line() +  # Change to geom_bar(stat="identity") if bar chart is preferred
  labs(title = "New Car Registrations in Sweden by Month",
       x = "Month", y = "Number of Registrations") +
  theme_minimal()



library(jsonlite)
library(ggplot2)

# Simulate loading full data
data <- fromJSON("C:/Users/theop/Downloads/000001EQ_20240427-191709.json")

# Assuming 'data' contains the complete dataset
# Extract dates and values, assuming 'time_labels' and 'values' are complete arrays in 'data'
time_labels <- names(data$dimension$Tid$category$label)
values <- data$value

# Create DataFrame with all data points
df <- data.frame(
  Month = as.Date(paste0(substr(time_labels, 1, 4), "-", substr(time_labels, 6, 7), "-01"), "%Y-%m-%d"),
  Registrations = values
)

# Verify the conversion
str(df)
head(df, 20)  # See the first 20 entries to check early months

# Plotting the data
ggplot(df, aes(x = Month, y = Registrations)) +
  geom_line() +  # Change to geom_bar(stat="identity") if bar chart is preferred
  labs(title = "New Car Registrations in Sweden from 2018 to 2023",
       x = "Month", y = "Number of Registrations") +
  theme_minimal()


# Create and save the plot directly
ggplot(df, aes(x = Month, y = Registrations)) +
  geom_line() +
  labs(title = "New Car Registrations in Sweden by Month",
       x = "Month", y = "Number of Registrations") +
  theme_minimal() -> p  # Assign to 'p' right before saving

# Save the plot
ggsave("new_car_registrations_plot.png", plot = p, width = 10, height = 6, dpi = 300)



  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
