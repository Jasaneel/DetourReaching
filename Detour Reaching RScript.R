# Load necessary libraries
library(readr)
library(ggplot2)
library(dplyr)
library(ggforce)
library(tidyr)
library(usethis)

# Set Git user details
usethis::use_git_config(user.name = "Jasaneel", user.email = "jbhella18@gmail.com")

# Load CSV data
Detour_reaching_data_2024_Sheet1 <- read_csv("C:/Users/User/Documents/Detour Reaching/Detour_reaching_data_2024_Sheet1.csv",
                                             col_types = cols(...1 = col_skip(), `trial number/day` = col_integer(),
                                                              pass = col_skip(), ...9 = col_skip()))

# Remove unnecessary rows and columns
Detour_reaching_data_2024_Sheet1 <- Detour_reaching_data_2024_Sheet1[-c(1:25), ]
Detour_reaching_data_2024_Sheet1$Time <- NULL
Detour_reaching_data_2024_Sheet1$`trial number/day` <- NULL
Detour_reaching_data_2024_Sheet1$date <- NULL

# Filter for specific individuals
individuals <- c("SP1", "SP2", "SP4", "SP5", "SP6", "SP7", "SP8", "SP11", "SP12", "SP13", "SP14", "SP16", "SP17", 
                 "SP18", "SP19", "SP20", "SP21")
filtered_data <- Detour_reaching_data_2024_Sheet1 %>%
  filter(`MS ID` %in% individuals)

# Standardize 'stage' column
filtered_data_clean <- filtered_data %>%
  mutate(stage = gsub("\\s*\\d+$", "", stage)) %>%
  mutate(stage = tolower(trimws(stage))) %>%
  mutate(stage = case_when(
    grepl("neophobia", stage) ~ "neophobia",
    grepl("habituation", stage) ~ "habituation",
    grepl("training", stage) ~ "training",
    grepl("testing", stage) ~ "testing",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(stage))

# Summarize trials per stage
stage_summary <- filtered_data_clean %>%
  group_by(`MS ID`, stage) %>%
  summarise(trials_in_stage = n()) %>%
  complete(stage = c("neophobia", "habituation", "training", "testing"), fill = list(trials_in_stage = 0)) %>%
  ungroup() %>%
  mutate(stage = factor(stage, levels = c("neophobia", "habituation", "training", "testing"))) %>%
  arrange(`MS ID`, stage)

# Calculate cumulative start and end points
stage_summary <- stage_summary %>%
  group_by(`MS ID`) %>%
  mutate(start_point = cumsum(lag(trials_in_stage, default = 0)),
         end_point = start_point + trials_in_stage,
         stage_index = as.numeric(stage)) %>%
  ungroup()

# Create stepwise plot
ggplot(stage_summary) +
  geom_segment(aes(x = start_point, xend = end_point, y = stage_index, yend = stage_index, color = `MS ID`), linewidth = 1.5) +
  geom_segment(aes(x = end_point, xend = end_point, y = stage_index, yend = stage_index + 1, color = `MS ID`), 
               data = subset(stage_summary, stage != "testing"), linewidth = 1.2) +
  geom_text(aes(x = (start_point + end_point) / 2, y = stage_index, label = trials_in_stage), vjust = -0.5, size = 3) +
  scale_y_continuous(name = "Stage of Experiment", breaks = c(1, 2, 3, 4), labels = c("neophobia", "habituation", "training", "testing")) +
  scale_x_continuous(name = "Cumulative Number of Trials", expand = c(0, 0)) +
  facet_wrap_paginate(~ `MS ID`, nrow = 1, ncol = 3, page = 1) +
  labs(title = "Stepwise Progression of Trials Across Stages", color = "Individual") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "top",
    panel.grid.major = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    strip.text.y = element_text(margin = margin(0, 0, 10, 0))
  )

# Save the plot
ggsave("Stepwise_detour_reaching_stages.png", width = 15, height = 10)
