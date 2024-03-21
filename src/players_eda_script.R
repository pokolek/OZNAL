
# Load necessary libraries
library(tidyverse)
# If needed, install additional libraries like ggplot2, dplyr, corrplot

# 1. Load and Inspect the Dataset
players <- read.csv("path/to/players_22.csv")
glimpse(players) # Provides a brief overview of the dataset
summary(players) # Summarizes the data giving quick insights

# 2. Data Cleaning
# Identify and handle missing values, outliers, and possibly convert data types as needed
players <- players %>% 
  filter(!is.na(overall)) # Example: Removing rows with missing 'overall' ratings

# 3. Distribution of Key Variables
# Plot the distribution of player attributes like age, overall, potential, etc.
ggplot(players, aes(x=age)) + geom_histogram(bins=30) + theme_minimal() + ggtitle("Distribution of Player Ages")

# 4. Relationship Between Variables
# Explore relationships, e.g., between a player's overall rating and their value_eur
ggplot(players, aes(x=overall, y=value_eur)) + geom_point() + theme_minimal() + ggtitle("Overall Rating vs. Market Value")

# 5. Comparisons and Aggregations
# Compare distributions or mean values across different groups, like comparing average overall ratings by league_name or nationality_name
players %>%
  group_by(league_name) %>%
  summarise(Avg_Overall = mean(overall, na.rm = TRUE)) %>%
  arrange(desc(Avg_Overall)) %>%
  head(10) %>% # Top 10 leagues by average player rating
  ggplot(aes(x=reorder(league_name, Avg_Overall), y=Avg_Overall)) +
  geom_bar(stat="identity") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Top 10 Leagues by Average Player Rating")

# 6. Explore Correlations
# Investigate correlations between numerical attributes to identify potential relationships
correlation_matrix <- cor(select(players, overall, potential, value_eur, age, wage_eur))
corrplot(correlation_matrix, method = "circle") # Note: Ensure 'corrplot' package is installed

# 7. Advanced Visualizations
# Consider creating more complex visualizations like scatter plot matrices, heatmaps, or even geographical plots if data includes location-based attributes
