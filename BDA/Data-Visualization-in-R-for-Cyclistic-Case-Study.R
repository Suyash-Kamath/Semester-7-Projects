# Load necessary libraries
library(tidyverse)
library(lubridate)

# Load the dataset into R
Combined_Divvy_Trips <- read.csv("Combined_Divvy_Trips.csv", stringsAsFactors = FALSE)

# Check if the dataset loaded correctly
print(head(Combined_Divvy_Trips))

# Convert trip duration to numeric, if necessary
Combined_Divvy_Trips$tripduration <- as.numeric(Combined_Divvy_Trips$tripduration)

# Convert start_time to datetime
Combined_Divvy_Trips$start_time <- ymd_hms(Combined_Divvy_Trips$start_time)

# Drop rows with NA in essential columns to prevent errors in plots
Combined_Divvy_Trips <- Combined_Divvy_Trips %>%
  drop_na(tripduration, usertype, gender, birthyear, from_station_name, to_station_name)

# 1. Histogram of Trip Duration
ggplot(Combined_Divvy_Trips, aes(x = tripduration)) +
  geom_histogram(binwidth = 300, fill = "blue", color = "black") +
  ggtitle("Distribution of Trip Duration") +
  xlab("Trip Duration (seconds)") +
  ylab("Frequency")

# 2. Trips by User Type (Subscriber vs. Customer)
ggplot(Combined_Divvy_Trips, aes(x = usertype)) +
  geom_bar(fill = "viridis") +
  ggtitle("Trips by User Type") +
  xlab("User Type") +
  ylab("Number of Trips")

# 3. Trips by Gender
ggplot(Combined_Divvy_Trips, aes(x = gender)) +
  geom_bar(fill = "pastel") +
  ggtitle("Trips by Gender") +
  xlab("Gender") +
  ylab("Number of Trips")

# 4. Trips by Birth Year (Ignoring NA values)
ggplot(Combined_Divvy_Trips %>% filter(!is.na(birthyear)), aes(x = birthyear)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black") +
  ggtitle("Distribution of User Birth Year") +
  xlab("Birth Year") +
  ylab("Frequency")

# 5. Trips Over the Days of the Week
Combined_Divvy_Trips$day_of_week <- weekdays(Combined_Divvy_Trips$start_time)

ggplot(Combined_Divvy_Trips, aes(x = day_of_week)) +
  geom_bar(fill = "Set2") +
  ggtitle("Trips by Day of the Week") +
  xlab("Day of Week") +
  ylab("Number of Trips")

# 6. Trips by Hour of the Day
Combined_Divvy_Trips$hour <- hour(Combined_Divvy_Trips$start_time)

ggplot(Combined_Divvy_Trips, aes(x = hour)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "black") +
  ggtitle("Trips by Hour of the Day") +
  xlab("Hour of Day") +
  ylab("Number of Trips")

# 7. Most Popular Start Stations
ggplot(Combined_Divvy_Trips %>% count(from_station_name) %>% top_n(10, n), aes(y = reorder(from_station_name, n), x = n)) +
  geom_bar(stat = "identity", fill = "coolwarm") +
  ggtitle("Most Popular Start Stations") +
  xlab("Number of Trips") +
  ylab("Start Station")

# 8. Most Popular End Stations
ggplot(Combined_Divvy_Trips %>% count(to_station_name) %>% top_n(10, n), aes(y = reorder(to_station_name, n), x = n)) +
  geom_bar(stat = "identity", fill = "coolwarm") +
  ggtitle("Most Popular End Stations") +
  xlab("Number of Trips") +
  ylab("End Station")

# 9. Average Trip Duration by User Type
ggplot(Combined_Divvy_Trips, aes(x = usertype, y = tripduration)) +
  geom_boxplot(fill = "muted") +
  ggtitle("Average Trip Duration by User Type") +
  xlab("User Type") +
  ylab("Trip Duration (seconds)")

# 10. Bike Usage Frequency
ggplot(Combined_Divvy_Trips %>% count(bikeid) %>% top_n(10, n), aes(y = reorder(bikeid, n), x = n)) +
  geom_bar(stat = "identity", fill = "coolwarm") +
  ggtitle("Bike Usage Frequency") +
  xlab("Number of Trips") +
  ylab("Bike ID")

# 11. Trip Duration by Gender
ggplot(Combined_Divvy_Trips, aes(x = gender, y = tripduration)) +
  geom_boxplot(fill = "pastel") +
  ggtitle("Trip Duration by Gender") +
  xlab("Gender") +
  ylab("Trip Duration (seconds)")

# 12. Trip Duration by Birth Year (Ignoring NA values)
ggplot(Combined_Divvy_Trips %>% filter(!is.na(birthyear)), aes(x = birthyear, y = tripduration)) +
  geom_point(alpha = 0.3) +
  ggtitle("Trip Duration vs. Birth Year") +
  xlab("Birth Year") +
  ylab("Trip Duration (seconds)")

# 13. Trips Over Time (Monthly)
Combined_Divvy_Trips$month <- floor_date(Combined_Divvy_Trips$start_time, "month")

ggplot(Combined_Divvy_Trips, aes(x = month)) +
  geom_bar(fill = "Blues") +
  ggtitle("Trips Over Time (Monthly)") +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Month") +
  ylab("Number of Trips")

# 14. Trips by Start and End Station
Combined_Divvy_Trips$route <- paste(Combined_Divvy_Trips$from_station_name, "->", Combined_Divvy_Trips$to_station_name)

ggplot(Combined_Divvy_Trips %>% count(route) %>% top_n(10, n), aes(y = reorder(route, n), x = n)) +
  geom_bar(stat = "identity", fill = "coolwarm") +
  ggtitle("Most Popular Routes") +
  xlab("Number of Trips") +
  ylab("Route")

# 15. Average Trip Duration by Start Station
Combined_Divvy_Trips %>% 
  group_by(from_station_name) %>% 
  summarize(mean_duration = mean(tripduration, na.rm = TRUE)) %>% 
  top_n(10, mean_duration) %>% 
  ggplot(aes(y = reorder(from_station_name, mean_duration), x = mean_duration)) +
  geom_bar(stat = "identity", fill = "purple") +
  ggtitle("Average Trip Duration by Start Station") +
  xlab("Average Trip Duration (seconds)") +
  ylab("Start Station")

# 16. Trips by Start Station and Gender
ggplot(Combined_Divvy_Trips, aes(x = from_station_name, fill = gender)) +
  geom_bar() +
  ggtitle("Trips by Start Station and Gender") +
  xlab("Start Station") +
  ylab("Number of Trips") +
  theme(axis.text.x = element_text(angle = 90))

# 17. Correlation Matrix Heatmap
corr_matrix <- cor(Combined_Divvy_Trips %>% select(tripduration, birthyear), use = "complete.obs")

ggplot(melt(corr_matrix), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient2() +
  ggtitle("Correlation Heatmap") +
  theme_minimal()

# 18. Trips by User Type and Gender
ggplot(Combined_Divvy_Trips, aes(x = usertype, fill = gender)) +
  geom_bar() +
  ggtitle("Trips by User Type and Gender") +
  xlab("User Type") +
  ylab("Number of Trips")

# 19. Average Trip Duration by End Station
Combined_Divvy_Trips %>% 
  group_by(to_station_name) %>% 
  summarize(mean_duration = mean(tripduration, na.rm = TRUE)) %>% 
  top_n(10, mean_duration) %>% 
  ggplot(aes(y = reorder(to_station_name, mean_duration), x = mean_duration)) +
  geom_bar(stat = "identity", fill = "cyan") +
  ggtitle("Average Trip Duration by End Station") +
  xlab("Average Trip Duration (seconds)") +
  ylab("End Station")

# 20. Trips by Bike ID and User Type
ggplot(Combined_Divvy_Trips, aes(x = bikeid, fill = usertype)) +
  geom_bar() +
  ggtitle("Trips by Bike ID and User Type") +
  xlab("Bike ID") +
  ylab("Number of Trips") +
  theme(axis.text.x = element_text(angle = 90))

print("Everything is done")
