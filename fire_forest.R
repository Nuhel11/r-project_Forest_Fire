install.packages("tidyverse")
library(tidyverse)
install.packages("readr")
library(readr)
forest_fires <- read_csv("C:/Users/zakir/Downloads/forest+fires/forestfires.csv")
show(forest_fires)
#what columns are in the datasets
colnames(forest_fires)
# Data Processing
#month and day are character variable, but we know that there is an inherent order to them. we'll convert these variables into factors so that they'll be sorted into the correct order when we plot them.
forest_fires %>% pull(month) %>% unique()
forest_fires %>% pull(day) %>% unique()
#rearraging the months
month_order <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
dow_order <- c("sun","mon","tue","wed","thu","fri", "sat")
forest_fires <- forest_fires %>% 
  mutate(
    month = factor(month, levels = month_order),
    day = factor(day, levels = dow_order)
  )
#when Do most forest fires occur?
# Count fires by month
forest_fires %>% count(month, sort = TRUE)
#visualize fire frequency by month
forest_fires %>% 
  count(month) %>% 
  ggplot(aes(x = month, y = n)) +
  geom_col(fill = "darkorange") +
  labs(title = "Forest Fires by Month", x = "Month", y = "Number of Fires")
#count fires by day of the week
forest_fires %>% 
  count(day, sort = TRUE)
#visualization
forest_fires %>% 
  count(day) %>% 
  ggplot(aes(x = day, y = n)) +
  geom_col(fill = "steelblue") +
  labs(title = "Forest Fires by Day of Week", x = "Day", y = "Number of Fires")
# combine month and day for deeper insight
forest_fires %>% 
  count(month, day) %>% 
  ggplot(aes(x = day, y = n, fill = month)) +
  geom_col(position = "dodge") +
  labs(title = "Forest Fires by Day and Month", x = "Day", y = "Number of Fires")
#plotting other variables against time
forest_fires_long <- forest_fires %>% 
  pivot_longer(
    cols = c("FFMC", "DMC", "DC", 
             "ISI", "temp", "RH", 
             "wind", "rain"),
    names_to = "data_col",
    values_to = "value"
  )

forest_fires_long %>% 
  ggplot(aes(x = month, y = value)) +
  geom_boxplot() +
  facet_wrap(vars(data_col), scale = "free_y") +
  labs(
    title = "Variable changes over month",
    x = "Month",
    y = "Variable value"
  )
#Define Severity Thresholds
forest_fires <- forest_fires %>%
  mutate(severity = case_when(
    area == 0 ~ "No Fire",
    area <= 5 ~ "Low",
    area <= 20 ~ "Moderate",
    area > 20 ~ "High"
  ))
#visualize severity distribution
ggplot(forest_fires, aes(x = severity)) +
  geom_bar(fill = "firebrick") +
  labs(title = "Forest Fire Severity Distribution", x = "Severity Level", y = "Count")
#explore severity vs weather conditions
ggplot(forest_fires, aes(x = temp, y = area)) +
  geom_point(aes(color = severity), alpha = 0.6) +
  labs(title = "Fire Severity vs Temperature", x = "Temperature (°C)", y = "Area Burned")
#severity by month or day
forest_fires %>%
  group_by(month, severity) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = month, y = count, fill = severity)) +
  geom_col(position = "dodge") +
  labs(title = "Fire Severity by Month", x = "Month", y = "Number of Fires")
#outlier problem
forest_fires_long %>% 
  filter(area < 300) %>% 
  ggplot(aes(x = value, y = area)) +
  geom_point() +
  facet_wrap(vars(data_col), scales = "free_x") +
  labs(
    title = "Relationships between other variables and area burned (area < 300)",
    x = "Value of column",
    y = "Area burned (hectare)"
  )
