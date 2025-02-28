# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyverse)

# Load and bind data for Outplant 1
Air_data <- bind_rows(
  read.csv("Air2/annotations/Airport2_0yr.csv"),
  read.csv("Air2/annotations/Airport2_0.5yr.csv"),
  read.csv("Air2/annotations/Airport2_1yr_ortho.csv")
)

# Load and bind data for Native
native_data <- bind_rows(
  read.csv("Air2/native/anotations/Airport2_0yr.csv"),
  read.csv("Air2/native/anotations/Airport2_0.5yr.csv"),
  read.csv("Air2/native/anotations/Airport2_1yr_ortho.csv")
)

# Add a Source column to distinguish datasets
Air_data <- Air_data %>% mutate(Source = "Outplant")
native_data <- native_data %>% mutate(Source = "Native")

# Combine both datasets
combined_data <- bind_rows(Air_data, native_data)



# Ensure HEALTH is numeric and create Year column
combined_data <- combined_data %>%
  mutate(
    HEALTH = as.numeric(HEALTH),
    Year = case_when(
      str_detect(Image.name, "0yr") ~ "0.0yr",
      str_detect(Image.name, "0.5yr") ~ "0.5yr",
      str_detect(Image.name, "1yr") ~ "1yr"
    )
  )

# Filter and clean data
combined_clean_data <- combined_data %>%
  filter(TagLab.Genet.Id >= 0 & TagLab.Genet.Id <= 121) %>%
  select(TagLab.Genet.Id, Year, TagLab.Area, HEALTH, Source) %>%
  arrange(TagLab.Genet.Id, Year)

# Aggregate data
combined_agg_data <- combined_clean_data %>%
  group_by(TagLab.Genet.Id, Year, Source) %>%
  summarize(
    Total_Area = sum(TagLab.Area, na.rm = TRUE),
    Avg_Health = coalesce(mean(HEALTH[HEALTH > 0], na.rm = TRUE), 0),
    Genet_Count = n(),
    Zero_Health_Count = sum(HEALTH == 0, na.rm = TRUE),
    .groups = "drop"
  )

# Manually input Initial_Count for Outplant and Native
initial_counts <- tibble(
  Source = c("Outplant", "Native"),
  Initial_Count = c(122, 122) # Replace with your actual counts
)

# Summarize status for each Source and Year
status_summary <- combined_agg_data %>%
  left_join(initial_counts, by = "Source") %>%
  group_by(Source, Year) %>%
  summarize(
    Alive = sum(Avg_Health > 0, na.rm = TRUE),
    Dead = sum(Avg_Health == 0, na.rm = TRUE),
    Initial_Count = first(Initial_Count)  # Manually input counts
  ) %>%
  mutate(
    Detached = Initial_Count - Alive - Dead,
    Alive = (Alive / Initial_Count) * 100,
    Dead = (Dead / Initial_Count) * 100,
    Detached = (Detached / Initial_Count) * 100
  )

# Reshape
status_summary_long <- status_summary %>%
  pivot_longer(cols = c("Alive", "Dead", "Detached"), names_to = "Status", values_to = "Percentage")

# Plot the status over the years for both datasets
ggplot(status_summary_long, aes(x = Year, y = Percentage, group = interaction(Status, Source), color = Status, linetype = Source)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Airport Colony Performance",
    x = "Year Since Outplant",
    y = "Percentage (%)",
    color = "Status",
    linetype = "Colony Type"
  ) +
  scale_color_manual(values = c("Alive" = "green", "Dead" = "red", "Detached" = "blue")) +
  scale_linetype_manual(values = c("Outplant" = "solid", "Native" = "twodash")) +  # Custom line types
  guides(
    linetype = guide_legend(override.aes = list(size = 2.5)) 
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))

