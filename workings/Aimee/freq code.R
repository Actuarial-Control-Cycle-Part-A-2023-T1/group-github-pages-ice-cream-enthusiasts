# Frequency

## Setting working directory
setwd("C:/Users/esoen/Desktop/aimee/2023/t1/actl4001/group assignment/data")

## Packages
install.packages("openxlsx")
install.packages("stringr")
library(openxlsx)
library(stringr)
library(ggplot2)

## Importing data
rows_selected <- 13:nrow(read.xlsx("2023-student-research-hazard-event-data.xlsx"))
cols_selected <- 2:9
hist_data <- read.xlsx("2023-student-research-hazard-event-data.xlsx", 
                       rows = rows_selected, cols = cols_selected, colNames = TRUE)
hist_data

### Arrange into dataframe
hist_data <- as.data.frame(hist_data)


## Exploring Frequency

### Total events or hazards
#### Total number of combined events with "/"
num_events_mix <- (unique(hist_data$Hazard.Event))

#### Different types of disasters
all_events <- unlist(strsplit(as.character(hist_data$Hazard.Event), "/"))
all_events <- unlist(strsplit(all_events, "-"))

##### Removing leading and trailing whitespace
(all_events)
all_events <- unlist(lapply(all_events, trimws))
type_event <- length(unique(all_events))
list_unique_events <- unique(all_events)

### Range of years (1999-2020)
years_vector <- unique(hist_data$Year)

### Frequency
frequency_event <- data.frame(matrix(ncol = length(list_unique_events) + 1, 
                                     nrow = length(unique(hist_data$Region))))
colnames(frequency_event) <- c("Region", list_unique_events)

num_region <- length(unique(hist_data$Region))
for (i in 1:num_region) {
  region_name <- unique(hist_data$Region)[i]
  region_data <- hist_data[hist_data$Region == region_name, "Hazard.Event"]
  for (j in 1:length(list_unique_events)) {
    event_name <- list_unique_events[j]
    frequency_event[i, j + 1] <- sum(grepl(event_name, region_data))
  }
  frequency_event[i, 1] <- region_name
}

### Frequency Table
frequency_event


### Plotting frequency
freq_reshaped <- tidyr::gather(frequency_event, Hazard, Frequency, -Region)

ggplot(freq_reshaped, aes(x = Region, y = Frequency, fill = Hazard)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Frequency of Hazards by Region (individual)")

  

ggplot(freq_reshaped, aes(x = Region, y = Frequency, fill = Hazard)) +
  geom_bar(stat = "identity", position = "stack") +
  ggtitle("Frequency of Hazards by Region (satcked)")
  

## Frequency over years
### Frequency Count
freq_time_table <- table(hist_data$Region, hist_data$Year
                         )
freq_time_table

### Plotting frequency over time

# Create a list of time series objects, one for each region
ts_list <- lapply(unique(hist_data$Region), function(x) {
  # Subset the data for the current region
  hist_data_region <- hist_data[hist_data$Region == x,]
  
  # Create a contingency table with counts for the current region
  table_df <- table(hist_data_region$Year)
  
  # Convert the contingency table to a time series object
  ts_obj <- ts(table_df, start = min(hist_data$Year), end = max(hist_data$Year), frequency = 1)
  
  # Return the time series object
  return(ts_obj)
})

ts_list
# Plot all the time series on the same plot
plot(ts_list[[1]], main = "Count of Hazards by Year for all Regions", 
     xlab = "Year", ylab = "Freq", ylim = c(0,100))

for (i in 2:length(ts_list)) {
  lines(ts_list[[i]], col = i)
}

legend("top", legend = unique(hist_data$Region), col = 1:length(unique(hist_data$Region)), lty = 1, 
       inset = c(0, 0, 0, 0), horiz = TRUE, x.intersp = 0.5, cex = 0.53, )
